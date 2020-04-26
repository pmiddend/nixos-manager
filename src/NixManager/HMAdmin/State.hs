{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-|
  Description: Contains all the state for the home-manager Administration tab
  -}
module NixManager.HMAdmin.State
  ( State
  , rebuildData
  , garbageData
  , changes
  , generationsState
  , determineChanges
  , initState
  )
where

import           GHC.Generics                   ( Generic )
import           NixManager.ChangeType          ( ChangeType(Changes, NoChanges)
                                                )
import           NixManager.HMAdmin.RebuildData ( RebuildData
                                                , initialRebuildData
                                                )
import           NixManager.HMAdmin.GarbageData ( GarbageData
                                                , initialGarbageData
                                                )
import           NixManager.HMServicesUtil      ( locatePendingServicesFile
                                                , locateInstalledServicesFile
                                                )
import           NixManager.HMPackagesUtil      ( locatePendingPackagesFile
                                                , locateInstalledPackagesFile
                                                )
import           NixManager.Util                ( determineFilesEqual )
import           NixManager.HMAdmin.GenerationsState
                                                ( GenerationsState
                                                , initGenerationsState
                                                )

-- | Contains all the state for the administration tab
data State = State {
    rebuildData :: RebuildData -- ^ The “Rebuild” GUI state
  , garbageData :: GarbageData -- ^ The “Collect garbage” GUI state
  , changes :: ChangeType -- ^ Information about whether we have unapplied changes
  , generationsState :: GenerationsState -- ^ Information about home-manager generations
  } deriving(Generic)

-- | Determine if there are changes that have to be applied.
determineChanges :: IO ChangeType
determineChanges = do
  packagesEqual <- determineFilesEqual locatePendingPackagesFile
                                       locateInstalledPackagesFile
  servicesEqual <- determineFilesEqual locatePendingServicesFile
                                       locateInstalledServicesFile

  pure (if packagesEqual && servicesEqual then NoChanges else Changes)

-- | The initial Administation tab state (needs to determine changes, hence the side-effect)
initState :: IO State
initState =
  State initialRebuildData initialGarbageData
    <$> determineChanges
    <*> initGenerationsState
