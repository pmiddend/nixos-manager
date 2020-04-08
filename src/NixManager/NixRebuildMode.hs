{-|
  Description: Provides an enum for all @nixos-rebuild@ rebuild modes
  -}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.NixRebuildMode
  ( NixRebuildMode(..)
  , isDry
  , rebuildModes
  , rebuildModeIdx
  , rebuildModeToText
  )
where

import           NixManager.Util                ( showText
                                                , kebapize
                                                )
import           Data.List                      ( elemIndex )
import           Data.Maybe                     ( fromJust )
import           Data.Text                      ( Text )
import           Control.Lens                   ( iso
                                                , Iso'
                                                )

-- | All possible arguments to @nixos-rebuild@
data NixRebuildMode = NixRebuildSwitch
 | NixRebuildBoot
 | NixRebuildTest
 | NixRebuildBuild
 | NixRebuildDryBuild
 | NixRebuildDryActivate
 | NixRebuildEdit
 | NixRebuildBuildVm
 | NixRebuildBuildVmWithBootloader
 deriving(Eq, Ord, Bounded, Enum, Show)

-- | Convert a rebuild mode to the corresponding @nixos-rebuild@ command
rebuildModeToText :: NixRebuildMode -> Text
rebuildModeToText = kebapize "NixRebuild" . showText

-- | Determine if a rebuild mode is a “dry” mode.
isDry :: NixRebuildMode -> Bool
isDry NixRebuildDryBuild              = True
isDry NixRebuildDryActivate           = True
isDry NixRebuildBuildVm               = True
isDry NixRebuildBuildVmWithBootloader = True
isDry _                               = False

-- | List of all rebuild modes
rebuildModes :: [NixRebuildMode]
rebuildModes = [minBound .. maxBound]

-- | The index of a rebuild mode inside the list of all rebuild modes and vice-versa.
rebuildModeIdx :: Iso' NixRebuildMode Int
rebuildModeIdx = iso (fromJust . (`elemIndex` rebuildModes)) (rebuildModes !!)
