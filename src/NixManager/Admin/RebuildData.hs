{-|
  Description: Contains all data for the rebuild GUI
  -}
{-# LANGUAGE TemplateHaskell #-}
module NixManager.Admin.RebuildData
  ( RebuildData(..)
  , rdBuildState
  , rdActiveRebuildModeIdx
  , rdDoUpdate
  , rdDoRollback
  , rdProcessOutput
  , rdDetailsState
  , initialRebuildData
  )
where

import           Control.Lens                   ( makeLenses )
import           NixManager.Process             ( ProcessOutput )
import           NixManager.Admin.BuildState    ( BuildState )
import           NixManager.Admin.DetailsState  ( DetailsState
                                                  ( DetailsContracted
                                                  )
                                                )

-- | Contains all data for the rebuild GUI
data RebuildData = RebuildData {
    _rdProcessOutput :: ProcessOutput  -- ^ Output of the current or last rebuild process (possibly empty)
  , _rdBuildState :: Maybe BuildState  -- ^ Contains the current build state of the rebuild
  , _rdActiveRebuildModeIdx :: Int -- ^ Index of the active rebuild mode, see "NixManager.NixRebuildMode"
  , _rdDetailsState :: DetailsState -- ^ Are the Details expanded?
  , _rdDoUpdate :: Bool -- ^ Shall we do an update?
  , _rdDoRollback :: Bool -- ^ Shall we do a rollback
  }

makeLenses ''RebuildData

-- | The initial rebuild state
initialRebuildData :: RebuildData
initialRebuildData = RebuildData mempty Nothing 0 DetailsContracted False False
