{-|
  Description: Provides an enum value for a package’s status (installed, pending, …)
  -}
{-# LANGUAGE TemplateHaskell #-}

module NixManager.NixPackageStatus
  ( NixPackageStatus(..)
  , _NixPackageInstalled
  , _NixPackagePendingInstall
  , _NixPackagePendingUninstall
  )
where

import           Control.Lens                   ( makePrisms )

-- | Enum containing a package’s status
data NixPackageStatus = NixPackageNothing -- ^ Package is not installed and not pending for installation/deinstallation
                      | NixPackageInstalled -- ^ Package is installed
                      | NixPackagePendingInstall -- ^ Package is pending installation
                      | NixPackagePendingUninstall -- ^ Package is installed and pending uninstallation
                      deriving (Eq, Show, Bounded, Enum)

makePrisms ''NixPackageStatus

