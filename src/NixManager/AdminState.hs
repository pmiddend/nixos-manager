{-# LANGUAGE TemplateHaskell #-}
module NixManager.AdminState
  ( AdminState(..)
  , AdminBuildState(AdminBuildState)
  , asBuildState
  , asProcessOutput
  , asActiveBuildType
  , absCounter
  , absProcessData
  )
where

import           Control.Lens                   ( makeLenses )
import           NixManager.Process             ( ProcessOutput
                                                , ProcessData
                                                )
import           Data.Text                      ( Text )

data AdminBuildState = AdminBuildState {
    _absCounter :: Int
  , _absProcessData :: ProcessData
  }

makeLenses ''AdminBuildState

data AdminState = AdminState {
    _asProcessOutput :: ProcessOutput
  , _asBuildState :: Maybe AdminBuildState
  , _asActiveBuildType :: Text
  }

makeLenses ''AdminState
