{-|
  Description: Contains all data for the rebuild GUI
Contains all data for the rebuild GUI
  -}
{-# LANGUAGE DeriveGeneric #-}
module NixManager.HMAdmin.RebuildData
  ( RebuildData(..)
  , initialRebuildData
  )
where

import           NixManager.Process             ( ProcessOutput )
import           NixManager.HMAdmin.BuildState  ( BuildState )
import           NixManager.View.DetailsState   ( DetailsState
                                                  ( DetailsContracted
                                                  )
                                                )
import           GHC.Generics                   ( Generic )

-- | Contains all data for the rebuild GUI
data RebuildData = RebuildData {
    processOutput :: ProcessOutput  -- ^ Output of the current or last rebuild process (possibly empty)
  , buildState :: Maybe BuildState  -- ^ Contains the current build state of the rebuild
  , activeRebuildModeIdx :: Int -- ^ Index of the active rebuild mode, see "NixManager.HMAdmin.RebuildMode"
  , detailsState :: DetailsState -- ^ Are the Details expanded?
  } deriving(Generic)

-- | The initial rebuild state
initialRebuildData :: RebuildData
initialRebuildData = RebuildData mempty Nothing 0 DetailsContracted
