module NixManager.Packages.Event
  ( Event(..)
  , CompletionType(..)
  , InstallationType(..)
  )
where

import           NixManager.Packages.PackageCategory
                                                ( PackageCategory )
import           NixManager.NixPackage          ( NixPackage )
import           Data.Text                      ( Text )
import           NixManager.Message             ( Message )
import           NixManager.Process             ( ProcessData
                                                , ProcessOutput
                                                )

data CompletionType = CompletionReload | CompletionPass

data InstallationType = Cancelled | Uncancelled

data Event = EventSearchChanged Text
           | EventPackageSelected (Maybe Int)
           | EventInstall InstallationType
           | EventInstallCompleted [NixPackage] InstallationType
           | EventUninstallCompleted [NixPackage] InstallationType
           | EventUninstall InstallationType
           | EventTryInstall
           | EventTryInstallStarted NixPackage ProcessData
           | EventTryInstallFailed Message
           | EventTryInstallSuccess
           | EventTryInstallCancel
           | EventTryInstallWatch ProcessData ProcessOutput
           | EventOperationCompleted Message CompletionType
           | EventCategoryChanged Int
           | EventReload
           | EventReloadFinished [NixPackage]
