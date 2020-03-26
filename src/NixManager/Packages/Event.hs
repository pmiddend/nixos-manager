module NixManager.Packages.Event
  ( Event(..)
  )
where

import           NixManager.NixPackage          ( NixPackage )
import           Data.Text                      ( Text )
import           NixManager.Message             ( Message )


data Event = EventSearchChanged Text
           | EventPackageSelected (Maybe Int)
           | EventInstall
           | EventInstallCompleted [NixPackage]
           | EventUninstallCompleted [NixPackage]
           | EventUninstall
           | EventTryInstall
           | EventShowMessage Message
