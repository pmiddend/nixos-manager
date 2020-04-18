{-# LANGUAGE TemplateHaskell #-}
{-|
  Description: Contains just the "BuildState" data type
 Contains just the "BuildState" data type
  -}
module NixManager.HMAdmin.BuildState
  ( BuildState(BuildState)
  , bsCounter
  , bsProcessData
  )
where

import           NixManager.Process             ( ProcessData )
import           Control.Lens                   ( makeLenses )

-- | Contains all the data corresponding to “some program that’s currently running”
data BuildState = BuildState {
    _bsCounter :: Int  -- ^ This field is necessary to “pulse” the GTK progress bar while building, see "NixManager.View.ProgressBar" for details
  , _bsProcessData :: ProcessData -- ^ The process data
  }

makeLenses ''BuildState

