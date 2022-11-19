{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}

module Ide.Plugin.HaddockComments
    ( descriptor
    ) where

import qualified Control.Lens                           as L
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Data.Function                          ((&))
import           Data.Generics                          (Data, cast, everything)
import           Data.List                              (foldl')
import           Data.Maybe                             (fromMaybe, isJust,
                                                         mapMaybe, maybeToList)
import           Data.Set                               (Set)
import qualified Data.Set                               as Set
import qualified Data.Text                              as T
import           Debug.Trace
import           Development.IDE                        hiding (pluginHandlers)
import           Development.IDE.GHC.Compat             hiding (Env)
import           Development.IDE.GHC.Compat.ExactPrint
import           Development.IDE.GHC.ExactPrint         (GetAnnotatedParsedSource (..))
import qualified Development.IDE.GHC.ExactPrint         as ExactPrint
import           Development.IDE.Plugin.CodeAction      (mkExactprintPluginDescriptor)
import           Development.IDE.Plugin.CodeAction.Util (traceAst)
import           Development.IDE.Spans.Common           (DocMap)
import           GHC                                    (LEpaComment(..), Anchor(..), AnchorOperation(..), DeltaPos(..))
import           Ide.Plugin.GHC.Lens
import           Ide.Types
import           Language.LSP.Types
import           Language.LSP.Types.Lens                (HasChanges (changes))
import Language.Haskell.GHC.ExactPrint.Utils (mkLEpaComment)

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

    -- Haddock comment code actions are improvements, so we don't show them if there are exiting unresolved errors
    let noErr = and $ (/= Just DsError) . _severity <$> diags
    unless noErr . MaybeT . pure $ Nothing

    nfp <- MaybeT . pure . uriToNormalizedFilePath . toNormalizedUri $ uri
    (env, decls) <- MaybeT . liftIO . runAction "HaddockComments" ideState $ do
        docMap <- fmap getDocMap <$> use GetDocMap nfp
        parsedSource <- use GetAnnotatedParsedSource nfp
        tcModRes <- use TypeCheck nfp
        let hsGroup = tcg_rn_decls . tmrTypechecked =<< tcModRes
            env = Env <$> docMap <*> hsGroup
            decls = filter (declInterleaveWithRange range) . hsmodDecls . unLoc . astA <$> parsedSource
        pure $ (,) <$> env <*> decls

    let decls' = mapMaybe (flip runReader env. runMaybeT . handle) decls
        codeActionResp = Right . List . fmap InR . take 1 . mapMaybe (buildCodeAction uri) $ decls'

    pure codeActionResp
  where
    defaultResult = Right $ List []

declInterleaveWithRange :: Range -> LHsDecl GhcPs -> Bool
declInterleaveWithRange (Range s2 e2) (L (locA -> (RealSrcSpan sp _)) _) =
    not (e1 <= s2 || e2 <= s1)
  where
    Range s1 e1 = realSrcSpanToRange sp
declInterleaveWithRange _ _                                      = False

buildCodeAction :: Uri -> LHsDecl GhcPs -> Maybe CodeAction
buildCodeAction uri decl = do
    let updatedSource = T.pack (exactPrint decl)
    range <- srcSpanToRange $ getLoc decl
    let edits = [ TextEdit range updatedSource ]
    pure CodeAction {
        _title = "Generate haddock comments",
        _kind = Just CodeActionQuickFix,
        _diagnostics = Nothing,
        _isPreferred = Nothing,
        _disabled = Nothing,
        _edit = Just $ mempty L.& changes L.?~ [(uri, List edits)],
        _command = Nothing,
        _xdata = Nothing
    }

data Env = Env
    { docMap  :: DocMap
    , hsGroup :: HsGroup GhcRn
    }

handle :: LHsDecl GhcPs -> MaybeT (Reader Env) (LHsDecl GhcPs)
handle lDecl = do
    Env{..} <- ask
    let documentedLocations = buildDocumentedLocSet hsGroup docMap
        documentableLocations = Set.fromList $
            concatMap (\t -> lDecl L.^.. L.runTraversal t . nodeRealLoc) documentableNameLenses
        undocumentedLocations = documentableLocations Set.\\ documentedLocations
    trace ("documented locs: " ++ show documentedLocations) .
        trace ("documentable locs: " ++ show documentableLocations) .
        trace ("undocumented locs: " ++ show undocumentedLocations) .
        when (null undocumentedLocations) $ -- Current declaration is fully documented, no furthur action needed
        MaybeT $ pure Nothing
    pure $ addCommentToNames undocumentedLocations lDecl

addCommentToNames :: Set RealSrcSpan -> LHsDecl GhcPs -> LHsDecl GhcPs
addCommentToNames undocumentedLocations decl = foldl' go decl documentableNameLenses
  where
    go :: LHsDecl GhcPs -> L.ReifiedTraversal' (LHsDecl GhcPs) (LIdP GhcPs) -> LHsDecl GhcPs
    go decl refiedTraversal = decl & L.runTraversal refiedTraversal L.%~ \name ->
        if maybe False (flip Set.member undocumentedLocations) (name L.^? nodeRealLoc)
            then prependHaddockCommentToName name
            else name

prependHaddockCommentToName :: LIdP GhcPs -> LIdP GhcPs
prependHaddockCommentToName name = name & _L . L._1 . _ann . _comments . _priorComments L.%~ (emptyHaddockComment++)
  where
    emptyHaddockComment :: [LEpaComment]
    emptyHaddockComment = maybeToList $ mkLEpaComment "-- |" <$> anchor' <*> loc
    anchor = name L.^? _L . L._1 . _ann . _entry
    anchor' = anchor & L._Just . _anchor_op L..~ MovedAnchor (DifferentLine 1 0)
    loc = anchor L.^? L._Just . _anchor

nodeRealLoc :: L.Traversal' (GenLocated (SrcSpanAnn' a) b) RealSrcSpan
nodeRealLoc = _L . L._1 . _locA . _RealSrcSpan . L._1

buildDocumentedLocSet :: HsGroup GhcRn -> DocMap -> Set RealSrcSpan
buildDocumentedLocSet hsGroup docMap = everything mappend go hsGroup
  where
    go :: forall a. Data a => a -> Set RealSrcSpan
    go x = case cast x of
        Nothing -> mempty
        Just (name :: LIdP GhcRn) ->
            if isJust (lookupNameEnv docMap (unLoc name))
                then maybe mempty Set.singleton $ getLoc name L.^? _RealSrcSpan . L._1
                else mempty

documentableNameLenses :: [L.ReifiedTraversal' (LHsDecl GhcPs) (LIdP GhcPs)]
documentableNameLenses =
    [ L.Traversal $ tyClD _tcdLName
    , L.Traversal $ dataCon _con_name
    , L.Traversal $ dataCon $ _con_args . _RecCon . ast . L.traversed
        . ast . _cd_fld_type . ast . _HsTyVar . L._3
    -- , L.Traversal $ dataCon $ _con_args . _RecCon . ast . L.traversed
    --     . ast . _cd_fld_names . L.traversed . ast . _rdrNameFieldOcc
    ]
  where
    ast :: L.Traversal' (GenLocated l ast) ast
    ast = _L . L._2

    tyClD x = ast . _TyClD . L._2 . x

    dataCon x = tyClD $ _tcdDataDefn . _dd_cons . L.traversed . ast . x
