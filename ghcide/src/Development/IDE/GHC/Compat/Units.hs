{-# LANGUAGE CPP             #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Compat module for 'UnitState' and 'UnitInfo'.
module Development.IDE.GHC.Compat.Units (
    -- * UnitState
    UnitState,
#if MIN_VERSION_ghc(9,3,0)
    initUnits,
#endif
    oldInitUnits,
    unitState,
    getUnitName,
    explicitUnits,
    preloadClosureUs,
    listVisibleModuleNames,
    LookupResult(..),
    lookupModuleWithSuggestions,
    -- * UnitInfoMap
    UnitInfoMap,
    getUnitInfoMap,
    lookupUnit,
    lookupUnit',
    -- * UnitInfo
    UnitInfo,
    unitExposedModules,
    unitDepends,
    unitHaddockInterfaces,
    unitInfoId,
    unitPackageNameString,
    unitPackageVersion,
    -- * UnitId helpers
    UnitId,
    Unit,
    unitString,
    stringToUnit,
    definiteUnitId,
    defUnitId,
    installedModule,
    -- * Module
    toUnitId,
    Development.IDE.GHC.Compat.Units.moduleUnitId,
    moduleUnit,
    -- * ExternalPackageState
    ExternalPackageState(..),
    -- * Utils
    filterInplaceUnits,
    FinderCache,
    showSDocForUser',
    ) where

import           Control.Monad
import qualified Data.List.NonEmpty              as NE
import qualified Data.Map.Strict                 as Map
#if MIN_VERSION_ghc(9,3,0)
import           GHC.Unit.Home.ModInfo
#endif
#if MIN_VERSION_ghc(9,2,0)
import qualified GHC.Data.ShortText              as ST
#if !MIN_VERSION_ghc(9,3,0)
import           GHC.Driver.Env                  (hsc_unit_dbs)
#endif
import           GHC.Driver.Ppr
import           GHC.Unit.Env
import           GHC.Unit.External
import           GHC.Unit.Finder
#else
import           GHC.Driver.Types
#endif
import           GHC.Data.FastString
import qualified GHC.Driver.Session              as DynFlags
import           GHC.Types.Unique.Set
import qualified GHC.Unit.Info                   as UnitInfo
import           GHC.Unit.State                  (LookupResult, UnitInfo,
                                                  UnitState (unitInfoMap))
import qualified GHC.Unit.State                  as State
import           GHC.Unit.Types                  hiding (moduleUnit, toUnitId)
import qualified GHC.Unit.Types                  as Unit
import           GHC.Utils.Outputable

import           Development.IDE.GHC.Compat.Core
import           Development.IDE.GHC.Compat.Env
#if !MIN_VERSION_ghc(9,2,0)
import           Data.Map                        (Map)
#endif
import           Data.Either
import           Data.Version
import qualified GHC

type PreloadUnitClosure = UniqSet UnitId
#if MIN_VERSION_ghc(9,2,0)
type UnitInfoMap = State.UnitInfoMap
#else
type UnitInfoMap = Map UnitId UnitInfo
#endif


unitState :: HscEnv -> UnitState
#if MIN_VERSION_ghc(9,2,0)
unitState = ue_units . hsc_unit_env
#else
unitState = DynFlags.unitState . hsc_dflags
#endif

#if MIN_VERSION_ghc(9,3,0)
createUnitEnvFromFlags :: NE.NonEmpty DynFlags -> HomeUnitGraph
createUnitEnvFromFlags unitDflags =
  let
    newInternalUnitEnv dflags = mkHomeUnitEnv dflags emptyHomePackageTable Nothing
    unitEnvList = NE.map (\dflags -> (homeUnitId_ dflags, newInternalUnitEnv dflags)) unitDflags
  in
    unitEnv_new (Map.fromList (NE.toList (unitEnvList)))

initUnits :: [DynFlags] -> HscEnv -> IO HscEnv
initUnits unitDflags env = do
  let dflags0         = hsc_dflags env
  -- additionally, set checked dflags so we don't lose fixes
  let initial_home_graph = createUnitEnvFromFlags (dflags0 NE.:| unitDflags)
      home_units = unitEnv_keys initial_home_graph
  home_unit_graph <- forM initial_home_graph $ \homeUnitEnv -> do
    let cached_unit_dbs = homeUnitEnv_unit_dbs homeUnitEnv
        dflags = homeUnitEnv_dflags homeUnitEnv
        old_hpt = homeUnitEnv_hpt homeUnitEnv

    (dbs,unit_state,home_unit,mconstants) <- State.initUnits (hsc_logger env) dflags cached_unit_dbs home_units

    updated_dflags <- DynFlags.updatePlatformConstants dflags mconstants
    pure HomeUnitEnv
      { homeUnitEnv_units = unit_state
      , homeUnitEnv_unit_dbs = Just dbs
      , homeUnitEnv_dflags = updated_dflags
      , homeUnitEnv_hpt = old_hpt
      , homeUnitEnv_home_unit = Just home_unit
      }

  let dflags1 = homeUnitEnv_dflags $ unitEnv_lookup (homeUnitId_ dflags0) home_unit_graph
  let unit_env = UnitEnv
        { ue_platform        = targetPlatform dflags1
        , ue_namever         = GHC.ghcNameVersion dflags1
        , ue_home_unit_graph = home_unit_graph
        , ue_current_unit    = homeUnitId_ dflags0
        , ue_eps             = ue_eps (hsc_unit_env env)
        }
  pure $ hscSetFlags dflags1 $ hscSetUnitEnv unit_env env
#endif

-- | oldInitUnits only needs to modify DynFlags for GHC <9.2
-- For GHC >= 9.2, we need to set the hsc_unit_env also, that is
-- done later by initUnits
oldInitUnits :: DynFlags -> IO DynFlags
#if MIN_VERSION_ghc(9,2,0)
oldInitUnits = pure
#else
oldInitUnits dflags = do
  newFlags <- State.initUnits dflags
  pure newFlags
#endif

explicitUnits :: UnitState -> [Unit]
explicitUnits ue =
#if MIN_VERSION_ghc(9,3,0)
  map fst $ State.explicitUnits ue
#else
  State.explicitUnits ue
#endif

listVisibleModuleNames :: HscEnv -> [ModuleName]
listVisibleModuleNames env =
  State.listVisibleModuleNames $ unitState env

getUnitName :: HscEnv -> UnitId -> Maybe PackageName
getUnitName env i =
  State.unitPackageName <$> State.lookupUnitId (unitState env) i

lookupModuleWithSuggestions
  :: HscEnv
  -> ModuleName
#if MIN_VERSION_ghc(9,3,0)
  -> GHC.PkgQual
#else
  -> Maybe FastString
#endif
  -> LookupResult
lookupModuleWithSuggestions env modname mpkg =
  State.lookupModuleWithSuggestions (unitState env) modname mpkg

getUnitInfoMap :: HscEnv -> UnitInfoMap
getUnitInfoMap =
#if MIN_VERSION_ghc(9,2,0)
  unitInfoMap . ue_units . hsc_unit_env
#else
  unitInfoMap . unitState
#endif

lookupUnit :: HscEnv -> Unit -> Maybe UnitInfo
lookupUnit env pid = State.lookupUnit (unitState env) pid

lookupUnit' :: Bool -> UnitInfoMap -> PreloadUnitClosure -> Unit -> Maybe UnitInfo
lookupUnit' = State.lookupUnit'

preloadClosureUs :: HscEnv -> PreloadUnitClosure
#if MIN_VERSION_ghc(9,2,0)
preloadClosureUs = State.preloadClosure . unitState
#else
preloadClosureUs = State.preloadClosure . unitState
#endif

unitExposedModules :: UnitInfo -> [(ModuleName, Maybe Module)]
unitExposedModules ue =
  UnitInfo.unitExposedModules ue

unitDepends :: UnitInfo -> [UnitId]
unitDepends = State.unitDepends

unitPackageNameString :: UnitInfo -> String
unitPackageNameString =
  UnitInfo.unitPackageNameString

unitPackageVersion :: UnitInfo -> Version
unitPackageVersion =
  UnitInfo.unitPackageVersion

unitInfoId :: UnitInfo -> Unit
unitInfoId =
  UnitInfo.mkUnit

unitHaddockInterfaces :: UnitInfo -> [FilePath]
unitHaddockInterfaces =
#if MIN_VERSION_ghc(9,2,0)
  fmap ST.unpack . UnitInfo.unitHaddockInterfaces
#else
  UnitInfo.unitHaddockInterfaces
#endif

-- ------------------------------------------------------------------
-- Backwards Compatible UnitState
-- ------------------------------------------------------------------

-- ------------------------------------------------------------------
-- Patterns and helpful definitions
-- ------------------------------------------------------------------

#if MIN_VERSION_ghc(9,2,0)
definiteUnitId :: Definite uid -> GenUnit uid
definiteUnitId         = RealUnit
defUnitId :: unit -> Definite unit
defUnitId              = Definite
installedModule :: unit -> ModuleName -> GenModule unit
installedModule        = Module

#else
definiteUnitId         = RealUnit
defUnitId              = Definite
installedModule        = Module

#endif

toUnitId :: Unit -> UnitId
toUnitId =
    Unit.toUnitId

moduleUnitId :: Module -> UnitId
moduleUnitId =
    Unit.toUnitId . Unit.moduleUnit

moduleUnit :: Module -> Unit
moduleUnit =
    Unit.moduleUnit

filterInplaceUnits :: [UnitId] -> [PackageFlag] -> ([UnitId], [PackageFlag])
filterInplaceUnits us packageFlags =
  partitionEithers (map isInplace packageFlags)
  where
    isInplace :: PackageFlag -> Either UnitId PackageFlag
    isInplace p@(ExposePackage _ (UnitIdArg u) _) =
      if toUnitId u `elem` us
        then Left $ toUnitId  u
        else Right p
    isInplace p = Right p

showSDocForUser' :: HscEnv -> GHC.PrintUnqualified -> SDoc -> String
#if MIN_VERSION_ghc(9,2,0)
showSDocForUser' env = showSDocForUser (hsc_dflags env) (unitState env)
#else
showSDocForUser' env = showSDocForUser (hsc_dflags env)
#endif
