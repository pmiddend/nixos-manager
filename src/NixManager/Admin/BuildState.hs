{-# LANGUAGE TemplateHaskell #-}
{-|
  Description: Contains just the "BuildState" data type
Contains just the "BuildState" data type
  -}
module NixManager.Admin.BuildState
  ( BuildState(BuildState)
  , bsCounter
  , bsProcessData
  , bsPassword
  )
where

import           NixManager.Process             ( ProcessData )
import           Control.Lens                   ( makeLenses )
import           NixManager.Password            ( Password )

-- | Contains all the data corresponding to “some sudo program that’s currently running”
data BuildState = BuildState {
    _bsCounter :: Int  -- ^ This field is necessary to “pulse” the GTK progress bar while building, see "NixManager.View.ProgressBar" for details
  , _bsProcessData :: ProcessData -- ^ The process data
  , _bsPassword :: Password -- ^ The password used to call the process. This is needed to cancel it again.
  }

makeLenses ''BuildState

