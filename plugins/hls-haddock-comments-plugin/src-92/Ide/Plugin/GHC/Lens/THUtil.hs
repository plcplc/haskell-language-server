module Ide.Plugin.GHC.Lens.THUtil
    ( prefixUnderscoreRules
    ) where

import           Control.Lens

prefixUnderscoreNamer :: FieldNamer
prefixUnderscoreNamer = mappingNamer (\n -> ['_' : n])

prefixUnderscoreRules :: LensRules
prefixUnderscoreRules = lensRules & lensField .~ prefixUnderscoreNamer
