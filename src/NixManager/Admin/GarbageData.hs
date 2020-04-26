{-|
  Description: Contains all data for the garbage collection GUI
Contains all data for the garbage collection GUI
  -}
{-# LANGUAGE DeriveGeneric #-}
module NixManager.Admin.GarbageData
  ( GarbageData(..)
  , initialGarbageData
  )
where

import           NixManager.View.DetailsState   ( DetailsState
                                                  ( DetailsContracted
                                                  )
                                                )
import           NixManager.Process             ( ProcessOutput )
import           NixManager.Admin.BuildState    ( BuildState )
import           Data.Generics.Labels           ( )
import           GHC.Generics                   ( Generic )

-- | Contains all data for the garbage collection GUI
data GarbageData = GarbageData {
    processOutput :: ProcessOutput -- ^ Output of the current or last garbage collection process (possibly empty)
  , buildState :: Maybe BuildState -- ^ Contains the current build state of the garbage collection
  , detailsState :: DetailsState -- ^ Are the Details expanded?
  , olderGenerations :: Bool -- ^ Shall we delete older generations?
  } deriving(Generic)

-- | The initial garbage collection state
initialGarbageData :: GarbageData
initialGarbageData = GarbageData mempty Nothing DetailsContracted False
