{-# LANGUAGE OverloadedStrings #-}
module NixManager.NixRebuildMode
  ( NixRebuildMode(..)
  , parseRebuildMode
  )
where

import           Data.Text                      ( Text )

data NixRebuildMode = NixRebuildSwitch
 | NixRebuildBoot
 | NixRebuildTest
 | NixRebuildBuild
 | NixRebuildDryBuild
 | NixRebuildDryActivate
 | NixRebuildEdit
 | NixRebuildBuildVm
 | NixRebuildBuildVmWithBootloader
 deriving(Eq, Ord, Bounded, Enum)

instance Show NixRebuildMode where
  show NixRebuildSwitch                = "switch"
  show NixRebuildBoot                  = "boot"
  show NixRebuildTest                  = "test"
  show NixRebuildBuild                 = "build"
  show NixRebuildDryBuild              = "dry-build"
  show NixRebuildDryActivate           = "dry-activate"
  show NixRebuildEdit                  = "edit"
  show NixRebuildBuildVm               = "build-vm"
  show NixRebuildBuildVmWithBootloader = "build-vm-with-bootloader"

parseRebuildMode :: Text -> Maybe NixRebuildMode
parseRebuildMode "switch"       = Just NixRebuildSwitch
parseRebuildMode "boot"         = Just NixRebuildBoot
parseRebuildMode "test"         = Just NixRebuildTest
parseRebuildMode "build"        = Just NixRebuildBuild
parseRebuildMode "dry-build"    = Just NixRebuildDryBuild
parseRebuildMode "dry-activate" = Just NixRebuildDryActivate
parseRebuildMode "edit"         = Just NixRebuildEdit
parseRebuildMode "build-vm"     = Just NixRebuildBuildVm
parseRebuildMode "build-vm-with-bootloader" =
  Just NixRebuildBuildVmWithBootloader
parseRebuildMode _ = Nothing
