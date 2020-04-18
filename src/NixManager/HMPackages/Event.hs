{-|
  Description: Contains the event type for all events corresponding to the Packages tab
Contains the event type for all events corresponding to the Packages tab
  -}
module NixManager.HMPackages.Event
  ( Event(..)
  )
where

import           NixManager.NixPackage          ( NixPackage )
import           NixManager.Message             ( Message )
import qualified NixManager.View.PackageEditView
                                               as PackageEditView

data Event = EventPackageEditView PackageEditView.Event -- ^ Triggered whenever the search entry changes
           | EventInstallCompleted [NixPackage] PackageEditView.InstallationType -- ^ Triggered when the installation of a package is successful. We pass the new package cache here so we can immediately update the state with it.
           | EventUninstallCompleted [NixPackage] PackageEditView.InstallationType -- ^ Triggered when the uninstallation of a package is successful. We pass the new package cache here so we can immediately update the state with it.
           | EventOperationCompleted Message PackageEditView.CompletionType -- ^ Whenever an operation (install/uninstall) completes, we emit this event and display a message
           | EventReload -- ^ This is triggered externally whenever we need to reload the cache. For example, when we rebuild successfully, we need to update the package’s status.
           | EventReloadFinished [NixPackage] -- ^ Contains the new cache when the reloading finished.
