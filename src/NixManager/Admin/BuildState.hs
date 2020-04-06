{-# LANGUAGE TemplateHaskell #-}
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

data BuildState = BuildState {
    _bsCounter :: Int
  , _bsProcessData :: ProcessData
  , _bsPassword :: Password
  }

makeLenses ''BuildState

