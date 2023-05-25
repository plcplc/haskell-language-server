-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import           Control.Arrow                ((&&&))
import           Control.Monad                (void)
import           Control.Monad.IO.Class       (liftIO)
import           Data.Function                ((&))
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Development.IDE.Main         as GhcideMain
import           Development.IDE.Types.Logger (Doc,
                                               Priority (Debug, Error, Info),
                                               WithPriority (WithPriority, priority),
                                               cfilter, cmapWithPrio,
                                               defaultLayoutOptions,
                                               layoutPretty,
                                               makeDefaultStderrRecorder,
                                               payload, renderStrict,
                                               withDefaultRecorder)
import qualified Development.IDE.Types.Logger as Logger
import qualified HlsPlugins                   as Plugins
import           Ide.Arguments                (Arguments (..),
                                               GhcideArguments (..),
                                               getArguments)
import           Ide.Main                     (defaultMain)
import qualified Ide.Main                     as IdeMain
import           Ide.PluginUtils              (pluginDescToIdePlugins)
import           Ide.Types                    (PluginDescriptor (pluginNotificationHandlers),
                                               defaultPluginDescriptor,
                                               mkPluginNotificationHandler)
import           Language.LSP.Server          as LSP
import           Language.LSP.Types           as LSP
#if MIN_VERSION_prettyprinter(1,7,0)
import           Prettyprinter                (Pretty (pretty), vsep)
#else
import           Data.Text.Prettyprint.Doc    (Pretty (pretty), vsep)
#endif

import Data.Time
import GHC.Debug.Stub
import qualified GHC.Debug.Client as Client
import qualified GHC.Debug.Snapshot as Client
import GHC.Debug.Convention (socketDirectory, snapshotDirectory)
import Data.Maybe
import System.Environment
import System.Posix.Process
import System.FilePath
import System.Exit
import Control.Concurrent

data Log
  = LogIdeMain IdeMain.Log
  | LogPlugins Plugins.Log

instance Pretty Log where
  pretty log = case log of
    LogIdeMain ideMainLog -> pretty ideMainLog
    LogPlugins pluginsLog -> pretty pluginsLog

with_ghc_debug main = do
  defaultSocketPath <- getDefaultSocketPath
  socketPath <- fromMaybe defaultSocketPath <$> lookupEnv "GHC_DEBUG_SOCKET"
  withGhcDebugUnix socketPath (main socketPath)
  where
  getDefaultSocketPath = do
      socketOverride <- fromMaybe "" <$> lookupEnv "GHC_DEBUG_SOCKET"
      if not (null socketOverride)
      then return socketOverride
      else do
          dir <- socketDirectory
          name <- getProgName
          pid <- show <$> getProcessID
          let socketName = pid ++ "-" ++ name
          return (dir </> socketName)

takeSnapshot :: FilePath -> LanguageContextEnv config -> IO () -> IO ()
takeSnapshot socket env snapshot = do
  snapshotDir <- snapshotDirectory
  snapshotPath <- formatTime defaultTimeLocale (snapshotDir </> takeFileName socket ++ "-snapshot-%Y-%m-%d-%H%M%S") <$> getZonedTime
  pid <- forkProcess $ do
    Client.withDebuggeeConnect socket $ \debugee -> do
      Client.fork debugee
      Client.makeSnapshot debugee snapshotPath
      Client.resume debugee
  void $ LSP.runLspT env $ LSP.sendNotification SWindowShowMessage $ ShowMessageParams MtInfo $ "Taking snapshot..."
  void $ forkIO $ do
    getProcessStatus True False pid >>= \case
      Just (Exited ExitSuccess) -> do
        void $ LSP.runLspT env $ LSP.sendNotification SWindowShowMessage $ ShowMessageParams MtInfo $ "Saved snapshot to " <> T.pack snapshotPath
      status -> do
        void $ LSP.runLspT env $ LSP.sendNotification SWindowShowMessage $ ShowMessageParams MtError $ "Snapshot process exited with " <> T.pack (show status)
    snapshot

main :: IO ()
main = with_ghc_debug $ \socket -> do
    -- plugin cli commands use stderr logger for now unless we change the args
    -- parser to get logging arguments first or do more complicated things
    pluginCliRecorder <- cmapWithPrio pretty <$> makeDefaultStderrRecorder Nothing
    args <- getArguments "haskell-language-server" (Plugins.idePlugins (cmapWithPrio LogPlugins pluginCliRecorder))

    (lspLogRecorder, cb1) <- Logger.withBacklog Logger.lspClientLogRecorder
    (lspMessageRecorder, cb2) <- Logger.withBacklog Logger.lspClientMessageRecorder
    -- This plugin just installs a handler for the `initialized` notification, which then
    -- picks up the LSP environment and feeds it to our recorders
    let lspRecorderPlugin = (defaultPluginDescriptor "LSPRecorderCallback")
          { pluginNotificationHandlers = mkPluginNotificationHandler LSP.SInitialized $ \_ _ _ _ -> do
              env <- LSP.getLspEnv
              liftIO $ (cb1 <> cb2) env

              let snapshot = void
                           $ LSP.sendRequest SWindowShowMessageRequest (ShowMessageRequestParams MtInfo "Take Snapshot?" (Just [MessageActionItem "yes"]))
                           $ \case
                              Right (Just (MessageActionItem "yes")) -> liftIO (takeSnapshot socket env (LSP.runLspT env snapshot))
                              _ -> snapshot
              snapshot

          }

    let (argsTesting, minPriority, logFilePath) =
          case args of
            Ghcide GhcideArguments{ argsTesting, argsDebugOn, argsLogFile} ->
              let minPriority = if argsDebugOn || argsTesting then Debug else Info
              in (argsTesting, minPriority, argsLogFile)
            _ -> (False, Info, Nothing)

    withDefaultRecorder logFilePath Nothing $ \textWithPriorityRecorder -> do
      let
        recorder = cmapWithPrio (pretty &&& id) $ mconcat
            [textWithPriorityRecorder
                & cfilter (\WithPriority{ priority } -> priority >= minPriority)
                & cmapWithPrio fst
            , lspMessageRecorder
                & cfilter (\WithPriority{ priority } -> priority >= Error)
                & cmapWithPrio (renderDoc . fst)
            , lspLogRecorder
                & cfilter (\WithPriority{ priority } -> priority >= minPriority)
                & cmapWithPrio (renderStrict . layoutPretty defaultLayoutOptions . fst)
                -- do not log heap stats to the LSP log as they interfere with the
                -- ability of lsp-test to detect a stuck server in tests and benchmarks
                & if argsTesting then cfilter (not . heapStats . snd . payload) else id
            ]
        plugins = Plugins.idePlugins (cmapWithPrio LogPlugins recorder)

      defaultMain
        (cmapWithPrio LogIdeMain recorder)
        args
        (plugins <> pluginDescToIdePlugins [lspRecorderPlugin])

renderDoc :: Doc a -> Text
renderDoc d = renderStrict $ layoutPretty defaultLayoutOptions $ vsep
    ["Error condition, please check your setup and/or the [issue tracker](" <> issueTrackerUrl <> "): "
    ,d
    ]

issueTrackerUrl :: Doc a
issueTrackerUrl = "https://github.com/haskell/haskell-language-server/issues"

heapStats :: Log -> Bool
heapStats (LogIdeMain (IdeMain.LogIDEMain (GhcideMain.LogHeapStats _))) = True
heapStats _                                                             = False
