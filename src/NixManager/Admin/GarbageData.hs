{-|
  Description: Contains all data for the garbage collection GUI
Contains all data for the garbage collection GUI
  -}
{-# LANGUAGE TemplateHaskell #-}
module NixManager.Admin.GarbageData
  ( GarbageData(..)
  , initialGarbageData
  , gdBuildState
  , gdProcessOutput
  , gdDetailsState
  , gdOlderGenerations
  )
where

import           Control.Lens                   ( makeLenses )
import           NixManager.View.DetailsState   ( DetailsState
                                                  ( DetailsContracted
                                                  )
                                                )
import           NixManager.Process             ( ProcessOutput )
import           NixManager.Admin.BuildState    ( BuildState )

-- | Contains all data for the garbage collection GUI
data GarbageData = GarbageData {
    _gdProcessOutput :: ProcessOutput -- ^ Output of the current or last garbage collection process (possibly empty)
  , _gdBuildState :: Maybe BuildState -- ^ Contains the current build state of the garbage collection
  , _gdDetailsState :: DetailsState -- ^ Are the Details expanded?
  , _gdOlderGenerations :: Bool -- ^ Shall we delete older generations?
  }

makeLenses ''GarbageData

-- | The initial garbage collection state
initialGarbageData :: GarbageData
initialGarbageData = GarbageData mempty Nothing DetailsContracted False
