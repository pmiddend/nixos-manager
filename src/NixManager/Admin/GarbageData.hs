{-# LANGUAGE TemplateHaskell #-}
module NixManager.Admin.GarbageData
  ( GarbageData
  , initialGarbageData
  , gdBuildState
  , gdProcessOutput
  , gdDetailsState
  , gdOlderGenerations
  )
where

import           Control.Lens                   ( makeLenses )
import           NixManager.Admin.DetailsState  ( DetailsState
                                                  ( DetailsContracted
                                                  )
                                                )
import           NixManager.Process             ( ProcessOutput )
import           NixManager.Admin.BuildState    ( BuildState )

data GarbageData = GarbageData {
    _gdProcessOutput :: ProcessOutput
  , _gdBuildState :: Maybe BuildState
  , _gdDetailsState :: DetailsState
  , _gdOlderGenerations :: Bool
  }

makeLenses ''GarbageData

initialGarbageData :: GarbageData
initialGarbageData = GarbageData mempty Nothing DetailsContracted True
