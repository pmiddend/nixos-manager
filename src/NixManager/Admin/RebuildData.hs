{-# LANGUAGE TemplateHaskell #-}
module NixManager.Admin.RebuildData
  ( RebuildData(..)
  , rdBuildState
  , rdActiveRebuildModeIdx
  , rdDoUpdate
  , rdDoRollback
  , rdProcessOutput
  , rdDetailsState
  , initialRebuildData
  )
where

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
  , _rdActiveRebuildModeIdx :: Int
  , _rdDetailsState :: DetailsState
  , _rdDoUpdate :: Bool
  , _rdDoRollback :: Bool
  }

makeLenses ''RebuildData

initialRebuildData :: RebuildData
initialRebuildData = RebuildData mempty Nothing 0 DetailsContracted False False
