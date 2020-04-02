{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.Admin.State
  ( State(..)
  , asChanges
  , asRebuildData
  , asGarbageData
  , initState
  )
where

import           NixManager.Changes             ( ChangeType
                                                , determineChanges
                                                )
import           Control.Lens                   ( makeLenses )
import           NixManager.Admin.RebuildData   ( RebuildData
                                                , initialRebuildData
                                                )
import           NixManager.Admin.GarbageData   ( GarbageData
                                                , initialGarbageData
                                                )

data State = State {
    _asRebuildData :: RebuildData
  , _asGarbageData :: GarbageData
  , _asChanges :: ChangeType
  }

makeLenses ''State

initState :: IO State
initState = State initialRebuildData initialGarbageData <$> determineChanges
