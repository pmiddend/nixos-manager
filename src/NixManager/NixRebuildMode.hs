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

rebuildModeToText :: NixRebuildMode -> Text
rebuildModeToText = kebapize "NixRebuild" . showText

isDry :: NixRebuildMode -> Bool
isDry NixRebuildDryBuild              = True
isDry NixRebuildDryActivate           = True
isDry NixRebuildBuildVm               = True
isDry NixRebuildBuildVmWithBootloader = True
isDry _                               = False

rebuildModes :: [NixRebuildMode]
rebuildModes = [minBound .. maxBound]

rebuildModeIdx :: Iso' NixRebuildMode Int
rebuildModeIdx = iso (fromJust . (`elemIndex` rebuildModes)) (rebuildModes !!)
