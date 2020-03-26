module NixManager.Packages.Event
  ( Event(..)
  )
where

import           NixManager.NixPackage          ( NixPackage )
import           Data.Text                      ( Text )
import           NixManager.Message             ( Message )
import           NixManager.Process             ( ProcessData
                                                , ProcessOutput
                                                )


data Event = EventSearchChanged Text
           | EventPackageSelected (Maybe Int)
           | EventInstall
           | EventInstallCompleted [NixPackage]
           | EventUninstallCompleted [NixPackage]
           | EventUninstall
           | EventTryInstall
           | EventTryInstallStarted NixPackage ProcessData
           | EventTryInstallFailed Message
           | EventTryInstallSuccess
           | EventTryInstallCancel
           | EventTryInstallWatch ProcessData ProcessOutput
           | EventShowMessage Message
