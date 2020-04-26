{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
  Description: Contains all the state for the Administration tab
Contains all the state for the Administration tab
  -}
module NixManager.Admin.State
  ( State(..)
  , initState
  , determineChanges
  )
where

import           NixManager.ChangeType          ( ChangeType(Changes, NoChanges)
                                                )
import           NixManager.Admin.RebuildData   ( RebuildData
                                                , initialRebuildData
                                                )
import           NixManager.Admin.GarbageData   ( GarbageData
                                                , initialGarbageData
                                                )
import           NixManager.NixServicesUtil     ( locateLocalServicesFile
                                                , locateRootServicesFile
                                                )
import           NixManager.NixPackagesUtil     ( locateLocalPackagesFile
                                                , locateRootPackagesFile
                                                )
import           NixManager.Util                ( determineFilesEqual )
import           Data.Generics.Labels           ( )
import           GHC.Generics                   ( Generic )

-- | Contains all the state for the administration tab
data State = State {
    rebuildData :: RebuildData -- ^ The “Rebuild” GUI state
  , garbageData :: GarbageData -- ^ The “Collect garbage” GUI state
  , changes :: ChangeType -- ^ Information about whether we have unapplied changes
  } deriving(Generic)

-- | Determine if there are changes that have to be applied.
determineChanges :: IO ChangeType
determineChanges = do
  packagesEqual <- determineFilesEqual locateLocalPackagesFile
                                       locateRootPackagesFile
  servicesEqual <- determineFilesEqual locateLocalServicesFile
                                       locateRootServicesFile
  pure (if packagesEqual && servicesEqual then NoChanges else Changes)


-- | The initial Administation tab state (needs to determine changes, hence the side-effect)
initState :: IO State
initState = State initialRebuildData initialGarbageData <$> determineChanges
