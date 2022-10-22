{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
module Ide.Plugin.HaddockComments.Data
    ( generateHaddockComments
    ) where

import           Control.Lens
import           Control.Monad                          (unless)
import           Data.List                              (isPrefixOf)
import           Debug.Trace
import           Development.IDE.GHC.Compat
import           Development.IDE.Plugin.CodeAction.Util (traceAst)
import           Development.IDE.Spans.Common           (DocMap)
import           GHC                                    (AddEpAnn (..),
                                                         AnnList (..),
                                                         DeltaPos (..),
                                                         EpAnn (..),
                                                         EpAnnComments (..),
                                                         EpaLocation (..),
                                                         LEpaComment,
                                                         SrcSpanAnn' (..),
                                                         TrailingAnn (..), ann,
                                                         comments)
import           Language.Haskell.GHC.ExactPrint        (balanceCommentsList',
                                                         runTransform)
import           Language.Haskell.GHC.ExactPrint.Utils  (ghcCommentText)

generateHaddockComments :: DocMap -> LHsDecl GhcPs -> Maybe (LHsDecl GhcPs)
generateHaddockComments docMap hsDecl@(L _ (TyClD _ (DataDecl { tcdLName, tcdDataDefn = HsDataDefn { dd_cons = cons } }))) = do
    let cons' = fixLConDecl <$> runTransform (fmap balanceFieldsComments <$> balanceCommentsList' cons) ^. _1
    unless (missingSomeHaddock cons') Nothing

    pure hsDecl
generateHaddockComments _ _ = Nothing

balanceFieldsComments :: LConDecl GhcPs -> LConDecl GhcPs
balanceFieldsComments (L locCon con@ConDeclH98 {con_args = RecCon (L locFields fields)}) =
    let fields' = runTransform (balanceCommentsList' fields) ^. _1
     in L locCon con {con_args = RecCon (L locFields fields')}
balanceFieldsComments lCon = lCon

missingSomeHaddock :: [LConDecl GhcPs] -> Bool
missingSomeHaddock = any $ \lcon@(L _ conDecl) -> case conDecl of
    ConDeclH98 { con_args = RecCon (L _ fields) } ->
        elem (Just False) $ hasHaddock lcon : fmap hasHaddock fields
    _ -> False

fixLConDecl :: LConDecl GhcPs -> LConDecl GhcPs
fixLConDecl (L declLoc decl@ConDeclH98 {con_args = RecCon (L fieldsLoc fields)}) =
    L declLoc decl {con_args = RecCon (L fieldsLoc fields')}
  where
    fields' = over _last (\(L loc@(SrcSpanAnn{ann}) field) -> L (loc {ann = updateLastFieldAnn ann}) field) fields

    updateLastFieldAnn EpAnnNotUsed    = EpAnnNotUsed
    updateLastFieldAnn ann@EpAnn{anns = AnnListItem annTrailing } =
        ann { anns = AnnListItem
                (AddCommaAnn (EpaDelta (SameLine 1) misplacedFollowingHaddockComments) : annTrailing)
            }

    misplacedFollowingHaddockComments :: [LEpaComment]
    misplacedFollowingHaddockComments = case ann fieldsLoc of
        EpAnn {anns = AnnList {al_close = Just (AddEpAnn AnnCloseC (EpaDelta _ comments))}} ->
            filter (matchCommentPrefix followingCommentPrefix) comments
        _ -> []
fixLConDecl x                                    = x

hasHaddock :: GenLocated (SrcAnn AnnListItem) b -> Maybe Bool
hasHaddock (L (ann -> EpAnn { anns = AnnListItem{lann_trailing}, comments }) _) = Just $
    any (matchCommentPrefix priorCommentPrefix) (priorComments comments)
    || any trailingAnnHasHaddock lann_trailing
  where
    trailingAnnHasHaddock :: TrailingAnn -> Bool
    trailingAnnHasHaddock (AddCommaAnn (EpaDelta _ comments)) = any (matchCommentPrefix followingCommentPrefix) comments
    trailingAnnHasHaddock _ = False
hasHaddock _                                 = Nothing

priorCommentPrefix :: [String]
priorCommentPrefix = ["-- |", "{-|", "{- |"]

followingCommentPrefix :: [String]
followingCommentPrefix = ["-- ^", "{-^", "{- ^"]

matchCommentPrefix :: [String] -> LEpaComment -> Bool
matchCommentPrefix prefix comment = any (`isPrefixOf` ghcCommentText comment) prefix
