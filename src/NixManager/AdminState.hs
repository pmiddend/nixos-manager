{-# LANGUAGE TemplateHaskell #-}
module NixManager.AdminState
  ( AdminState(..)
  , AdminBuildState(AdminBuildState)
  , _AdminStateBuilding
  , absProcessOutput
  , absCounter
  )
where

import           Control.Lens                   ( makeLenses
                                                , makePrisms
                                                )
import           Data.ByteString                ( ByteString )
import           NixManager.Process             ( ProcessOutput
                                                , ProcessData
                                                )

data AdminBuildState = AdminBuildState {
    _absProcessOutput :: ProcessOutput
  , _absProcessData :: ProcessData
  , _absCounter :: Int
  }

makeLenses ''AdminBuildState

data AdminState = AdminStateNothing ProcessOutput
                | AdminStateBuilding AdminBuildState

makePrisms ''AdminState
