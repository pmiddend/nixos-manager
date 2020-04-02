{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.Admin.State
  ( State(..)
  , BuildState(BuildState)
  , asBuildState
  , asProcessOutput
  , asActiveRebuildMode
  , absCounter
  , asDoUpdate
  , asDoRollback
  , absProcessData
  , asChanges
  , initState
  , asDetailsState
  , detailsBool
  , DetailsState(..)
  )
where

import           NixManager.NixRebuildMode      ( NixRebuildMode
                                                  ( NixRebuildSwitch
                                                  )
                                                )
import           NixManager.Changes             ( ChangeType
                                                , determineChanges
                                                )
import           Control.Lens                   ( makeLenses
                                                , Iso'
                                                , iso
                                                )
import           NixManager.Process             ( ProcessOutput
                                                , ProcessData
                                                )

data BuildState = BuildState {
    _absCounter :: Int
  , _absProcessData :: ProcessData
  }

makeLenses ''BuildState

data DetailsState = DetailsContracted
                  | DetailsExpanded

detailsBool :: Iso' DetailsState Bool
detailsBool = iso toBool fromBool
 where
  toBool DetailsContracted = False
  toBool DetailsExpanded   = True
  fromBool False = DetailsContracted
  fromBool True  = DetailsExpanded

data State = State {
    _asProcessOutput :: ProcessOutput
  , _asBuildState :: Maybe BuildState
  , _asActiveRebuildMode :: NixRebuildMode
  , _asDetailsState :: DetailsState
  , _asDoUpdate :: Bool
  , _asDoRollback :: Bool
  , _asChanges :: ChangeType
  }

makeLenses ''State

initState :: IO State
initState =
  State mempty Nothing NixRebuildSwitch DetailsContracted False False
    <$> determineChanges
