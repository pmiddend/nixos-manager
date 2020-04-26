{-# LANGUAGE DeriveGeneric #-}
{-|
  Description: Contains just the "BuildState" data type
 Contains just the "BuildState" data type
  -}
module NixManager.HMAdmin.BuildState
  ( BuildState(BuildState)
  )
where

import           NixManager.Process             ( ProcessData )
import           GHC.Generics                   ( Generic )

-- | Contains all the data corresponding to “some program that’s currently running”
data BuildState = BuildState {
    counter :: Int  -- ^ This field is necessary to “pulse” the GTK progress bar while building, see "NixManager.View.ProgressBar" for details
  , processData :: ProcessData -- ^ The process data
  } deriving(Generic)
