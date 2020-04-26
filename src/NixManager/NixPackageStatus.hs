{-|
  Description: Provides an enum value for a package’s status (installed, pending, …)
Provides an enum value for a package’s status (installed, pending, …)
  -}
{-# LANGUAGE DeriveGeneric #-}
module NixManager.NixPackageStatus
  ( NixPackageStatus(..)
  )
where

import           GHC.Generics                   ( Generic )

-- | Enum containing a package’s status
data NixPackageStatus = NixPackageNothing -- ^ Package is not installed and not pending for installation/deinstallation
                      | NixPackageInstalled -- ^ Package is installed
                      | NixPackagePendingInstall -- ^ Package is pending installation
                      | NixPackagePendingUninstall -- ^ Package is installed and pending uninstallation
                      deriving (Eq, Show, Bounded, Enum, Generic)

