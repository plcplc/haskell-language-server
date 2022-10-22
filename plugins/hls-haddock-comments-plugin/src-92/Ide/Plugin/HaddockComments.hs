{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}

module Ide.Plugin.HaddockComments
    ( descriptor
    ) where

import qualified Control.Lens                           as L
import           Control.Monad.IO.Class                 (liftIO)
import           Control.Monad.Trans.Maybe
import           Data.Foldable                          (toList)
import           Data.Maybe                             (catMaybes, fromMaybe)
import qualified Data.Text                              as T
import qualified Data.Text.IO                           as T
import           Development.IDE                        hiding (pluginHandlers)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.ExactPrint
import           Development.IDE.GHC.ExactPrint         (GetAnnotatedParsedSource (..))
import qualified Development.IDE.GHC.ExactPrint         as ExactPrint
import           Development.IDE.Plugin.CodeAction      (mkExactprintPluginDescriptor)
import           Development.IDE.Plugin.CodeAction.Util (traceAst)
import           Development.IDE.Spans.Common           (DocMap)
import           GHC.Utils.Outputable
import qualified Ide.Plugin.HaddockComments.Data        as Data
import           Ide.Types
import           Language.LSP.Types
import           Language.LSP.Types.Lens                (HasChanges (changes))

data Log = LogExactPrint ExactPrint.Log

instance Pretty Log where
    pretty (LogExactPrint log) = pretty log

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = mkExactprintPluginDescriptor (cmapWithPrio LogExactPrint recorder) $
  (defaultPluginDescriptor plId)
    { pluginHandlers = mkPluginHandler STextDocumentCodeAction codeActionProvider
    }

codeActionProvider :: PluginMethodHandler IdeState TextDocumentCodeAction
codeActionProvider ideState _pId (CodeActionParams _ _ (TextDocumentIdentifier uri) range
    CodeActionContext {_diagnostics = List diags}) = fmap (fromMaybe defaultResult) . runMaybeT $ do
    nfp <- MaybeT . pure . uriToNormalizedFilePath . toNormalizedUri $ uri
    docMap <- fmap getDocMap . MaybeT . liftIO . runAction "HaddockComments.GetDocMap" ideState $ use GetDocMap nfp
    liftIO . print . nonDetUFMToList $ docMap
    pm <- MaybeT . liftIO $ runAction "HaddockComments.GetAnnotatedParsedSource" ideState $
        use GetAnnotatedParsedSource nfp
    let locDecls = hsmodDecls . unLoc . astA $ pm
        codeActions = fmap InR $ take 1 $ catMaybes
            [ runDeclHaddockGenerator uri (generator docMap) locDecl |
                noErr,
                locDecl <- locDecls,
                declInterleaveWithRange locDecl range,
                generator <- declHaddockGenerator
            ]
    pure . Right . List $ codeActions
  where
    defaultResult = Right $ List []
    noErr = and $ (/= Just DsError) . _severity <$> diags

declHaddockGenerator :: [DocMap -> LHsDecl GhcPs -> Maybe (LHsDecl GhcPs)]
declHaddockGenerator =
    [ Data.generateHaddockComments
    ]

declInterleaveWithRange :: LHsDecl GhcPs -> Range -> Bool
declInterleaveWithRange (L (locA -> (RealSrcSpan sp _)) _) (Range s2 e2) =
    not (e1 <= s2 || e2 <= s1)
  where
    Range s1 e1 = realSrcSpanToRange sp
declInterleaveWithRange _ _                                      = False

runDeclHaddockGenerator
    :: Uri
    -> (LHsDecl GhcPs -> Maybe (LHsDecl GhcPs))
    -> LHsDecl GhcPs
    -> Maybe CodeAction
runDeclHaddockGenerator uri generator locDecl@(L l _) = do
    updatedDecl <- T.pack . exactPrint <$> generator locDecl
    range :: Range <- srcSpanToRange $ locA l
    let edits = [ TextEdit range updatedDecl ]
    pure $ CodeAction {
        _title = "Generate haddock comments",
        _kind = Just CodeActionQuickFix,
        _diagnostics = Nothing,
        _isPreferred = Nothing,
        _disabled = Nothing,
        _edit = Just $ mempty L.& changes L.?~ [(uri, List edits)],
        _command = Nothing,
        _xdata = Nothing
    }
