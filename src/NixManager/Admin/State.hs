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
  , asDetailsState
  , detailsBool
  , DetailsState(..)
  )
where

import           Control.Lens                   ( makeLenses
                                                , Iso'
                                                , iso
                                                )
import           NixManager.Process             ( ProcessOutput
                                                , ProcessData
                                                )
import           Data.Text                      ( Text )

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
  , _asActiveBuildType :: Text
  , _asDetailsState :: DetailsState
  }

makeLenses ''State

initState :: State
initState = State mempty Nothing "switch" DetailsContracted
