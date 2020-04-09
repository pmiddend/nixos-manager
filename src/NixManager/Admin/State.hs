{-|
  Description: Contains all the state for the Administration tab
  -}
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

-- | Contains all the state for the administration tab
data State = State {
    _asRebuildData :: RebuildData -- ^ The “Rebuild” GUI state
  , _asGarbageData :: GarbageData -- ^ The “Collect garbage” GUI state
  , _asChanges :: ChangeType -- ^ Information about whether we have unapplied changes
  }

makeLenses ''State

-- | The initial Administation tab state (needs to determine changes, hence the side-effect)
initState :: IO State
initState = State initialRebuildData initialGarbageData <$> determineChanges
