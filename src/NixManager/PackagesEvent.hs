module NixManager.PackagesEvent
  ( PackagesEvent(..)
  )
where

import           NixManager.NixPackage          ( NixPackage )
import           Data.Text                      ( Text )
import           NixManager.Message             ( Message )


data PackagesEvent = PackagesEventSearchChanged Text
                   | PackagesEventPackageSelected (Maybe Int)
                   | PackagesEventInstall
                   | PackagesEventInstallCompleted [NixPackage]
                   | PackagesEventUninstallCompleted [NixPackage]
                   | PackagesEventUninstall
                   | PackagesEventTryInstall
                   | PackagesEventShowMessage Message
