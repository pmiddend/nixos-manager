{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.Admin.State
  ( State(..)
  , BuildState(BuildState)
  , asBuildState
  , asProcessOutput
  , asActiveBuildType
  , absCounter
  , absProcessData
  , initState
  )
where

import           Control.Lens                   ( makeLenses )
import           NixManager.Process             ( ProcessOutput
                                                , ProcessData
                                                )
import           Data.Text                      ( Text )

data BuildState = BuildState {
    _absCounter :: Int
  , _absProcessData :: ProcessData
  }

makeLenses ''BuildState

data State = State {
    _asProcessOutput :: ProcessOutput
  , _asBuildState :: Maybe BuildState
  , _asActiveBuildType :: Text
  }

makeLenses ''State

initState :: State
initState = State mempty Nothing "switch"
