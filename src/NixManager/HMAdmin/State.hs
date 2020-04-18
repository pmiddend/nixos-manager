{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
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

import           NixManager.ChangeType          ( ChangeType(Changes, NoChanges)
                                                )
import           Control.Lens                   ( makeLenses )
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
    _rebuildData :: RebuildData -- ^ The “Rebuild” GUI state
  , _garbageData :: GarbageData -- ^ The “Collect garbage” GUI state
  , _changes :: ChangeType -- ^ Information about whether we have unapplied changes
  , _generationsState :: GenerationsState -- ^ Information about home-manager generations
  }

makeLenses ''State

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
