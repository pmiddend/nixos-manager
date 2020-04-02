{-# LANGUAGE TemplateHaskell #-}
module NixManager.Admin.BuildState
  ( BuildState(BuildState)
  , bsCounter
  , bsProcessData
  )
where

import           NixManager.Process             ( ProcessData )
import           Control.Lens                   ( makeLenses )


data BuildState = BuildState {
    _bsCounter :: Int
  , _bsProcessData :: ProcessData
  }

makeLenses ''BuildState

