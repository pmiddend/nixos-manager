{-# LANGUAGE TemplateHaskell #-}

module NixManager.NixPackageStatus
  ( NixPackageStatus(..)
  , _NixPackageInstalled
  , _NixPackagePendingInstall
  , _NixPackagePendingUninstall
  )
where

import           Control.Lens                   ( makePrisms )

data NixPackageStatus = NixPackageNothing
                      | NixPackageInstalled
                      | NixPackagePendingInstall
                      | NixPackagePendingUninstall
                      deriving (Eq, Show, Bounded, Enum)

makePrisms ''NixPackageStatus

