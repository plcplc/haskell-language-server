{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}

module Ide.Plugin.GHC.Lens where

import           Control.Lens
import           GHC
import           Ide.Plugin.GHC.Lens.THUtil      (prefixUnderscoreRules)

makePrisms ''GenLocated
makePrisms ''HsDecl
makeLensesWith prefixUnderscoreRules ''TyClDecl
makePrisms ''Sig
makeLensesWith prefixUnderscoreRules ''HsDataDefn
makeLensesWith prefixUnderscoreRules ''ConDecl
makePrisms ''HsConDetails
makeLensesWith prefixUnderscoreRules ''ConDeclField
makeLensesWith prefixUnderscoreRules ''FieldOcc
makePrisms ''SrcLoc
makePrisms ''SrcSpan
makePrisms ''HsType
makeLensesWith prefixUnderscoreRules ''HsType
makeLensesWith prefixUnderscoreRules ''SrcSpanAnn'
makeLensesWith prefixUnderscoreRules ''EpAnn
makeLensesWith prefixUnderscoreRules ''EpAnnComments
makeLensesWith prefixUnderscoreRules ''Anchor
makePrisms ''AnchorOperation
