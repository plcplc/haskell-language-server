{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ide.Plugin.HaddockComments.Prelude
    ( module Development.IDE.GHC.Compat
    ) where

import           Data.List                             (intercalate)
import           Debug.Trace                           (trace)
import           Development.IDE.GHC.Compat
import           Generics.SYB                          (Data, cast, everything)
import           GHC                                   (EpAnnComments (..),
                                                        LEpaComment)
import           Language.Haskell.GHC.ExactPrint.Utils (ghcCommentText)
