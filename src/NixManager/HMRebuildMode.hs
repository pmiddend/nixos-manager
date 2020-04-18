{-# LANGUAGE OverloadedStrings #-}
{-|
  Description: Contains the "HMRebuildMode" data type and corresponding lenses/functions
Contains the "HMRebuildMode" data type and corresponding lenses/functions
  -}
module NixManager.HMRebuildMode
  ( HMRebuildMode(..)
  , rebuildModeToText
  , rebuildModeToDescription
  , rebuildModes
  , rebuildModeIdx
  )
where

import           Data.Text                      ( Text )
import           Control.Lens                   ( iso
                                                , Iso'
                                                )
import           Data.List                      ( elemIndex )
import           Data.Maybe                     ( fromJust )

-- | Specifies home-manager’s rebuild modes
data HMRebuildMode = RebuildSwitch -- ^ Corresponds to @home-manager switch@
                   | RebuildDrySwitch -- ^ Corresponds to @home-manager switch -n@
                   deriving(Eq, Ord, Enum, Bounded, Show)

-- | Convert a rebuild mode to a text for the UI
rebuildModeToText :: HMRebuildMode -> Text
rebuildModeToText RebuildSwitch    = "switch"
rebuildModeToText RebuildDrySwitch = "dry-switch"

-- | Convert a rebuild mode to a description text for the UI
rebuildModeToDescription :: HMRebuildMode -> Text
rebuildModeToDescription RebuildSwitch = "Build and activate configuration"
rebuildModeToDescription RebuildDrySwitch =
  "Do a dry run, only print what actions would be taken"

-- | List of all rebuild modes
rebuildModes :: [HMRebuildMode]
rebuildModes = [minBound .. maxBound]

-- | The index of a rebuild mode inside the list of all rebuild modes and vice-versa.
rebuildModeIdx :: Iso' HMRebuildMode Int
rebuildModeIdx = iso (fromJust . (`elemIndex` rebuildModes)) (rebuildModes !!)
