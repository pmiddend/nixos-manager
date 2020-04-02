{-# LANGUAGE TemplateHaskell #-}
module NixManager.Admin.RebuildData
  ( RebuildData(..)
  , rdBuildState
  , rdActiveRebuildMode
  , rdDoUpdate
  , rdDoRollback
  , rdProcessOutput
  , rdDetailsState
  , initialRebuildData
  )
where

import           NixManager.NixRebuildMode      ( NixRebuildMode
                                                  ( NixRebuildSwitch
                                                  )
                                                )
import           Control.Lens                   ( makeLenses )
import           NixManager.Process             ( ProcessOutput )
import           NixManager.Admin.BuildState    ( BuildState )
import           NixManager.Admin.DetailsState  ( DetailsState
                                                  ( DetailsContracted
                                                  )
                                                )

data RebuildData = RebuildData {
    _rdProcessOutput :: ProcessOutput
  , _rdBuildState :: Maybe BuildState
  , _rdActiveRebuildMode :: NixRebuildMode
  , _rdDetailsState :: DetailsState
  , _rdDoUpdate :: Bool
  , _rdDoRollback :: Bool
  }

makeLenses ''RebuildData

initialRebuildData :: RebuildData
initialRebuildData =
  RebuildData mempty Nothing NixRebuildSwitch DetailsContracted False False
