{-|
  Description: Contains all data for the rebuild GUI
Contains all data for the rebuild GUI
  -}
{-# LANGUAGE TemplateHaskell #-}
module NixManager.HMAdmin.RebuildData
  ( RebuildData(..)
  , rdBuildState
  , rdActiveRebuildModeIdx
  , rdProcessOutput
  , rdDetailsState
  , initialRebuildData
  )
where

import           Control.Lens                   ( makeLenses )
import           NixManager.Process             ( ProcessOutput )
import           NixManager.HMAdmin.BuildState  ( BuildState )
import           NixManager.View.DetailsState   ( DetailsState
                                                  ( DetailsContracted
                                                  )
                                                )

-- | Contains all data for the rebuild GUI
data RebuildData = RebuildData {
    _rdProcessOutput :: ProcessOutput  -- ^ Output of the current or last rebuild process (possibly empty)
  , _rdBuildState :: Maybe BuildState  -- ^ Contains the current build state of the rebuild
  , _rdActiveRebuildModeIdx :: Int -- ^ Index of the active rebuild mode, see "NixManager.HMAdmin.RebuildMode"
  , _rdDetailsState :: DetailsState -- ^ Are the Details expanded?
  }

makeLenses ''RebuildData

-- | The initial rebuild state
initialRebuildData :: RebuildData
initialRebuildData = RebuildData mempty Nothing 0 DetailsContracted
