{-|
  Description: Contains the event type for all events corresponding to the Packages tab
Contains the event type for all events corresponding to the Packages tab
  -}
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

-- | (Boolean) enum signifying if, after we made some change to the packages (say we marked a package for installation), it’s time to re-evaluate if we have changes to apply. If we mark a package for installation, we, of course, have changes to apply. A case for the opposite is marking a package for installation and immediately unmarking it again.
data CompletionType = CompletionReload | CompletionPass

-- | Here, two concepts clash a bit. We have the operation “Mark for installation”, which actually stands for two things, depending on the current state of a package. If the package is currently marked for uninstallation, installing it actually just cancels the uninstallation. If it’s not marked for uninstallation, installing it, well, marks it for installation. Depending on which of the two we wanted to do when we pressed the corresponding button, this enum is passed to certain events.
data InstallationType = Cancelled | Uncancelled

data Event = EventSearchChanged Text -- ^ Triggered whenever the search entry changes
           | EventPackageSelected (Maybe Int) -- ^ Triggered whenever the currently selected package changes
           | EventInstall InstallationType  -- ^ Triggered when the user clicks on the installation button (the passed enum determines what to do and what to show when the operation completes)
           | EventInstallCompleted [NixPackage] InstallationType -- ^ Triggered when the installation of a package is successful. We pass the new package cache here so we can immediately update the state with it.
           | EventUninstallCompleted [NixPackage] InstallationType -- ^ Triggered when the uninstallation of a package is successful. We pass the new package cache here so we can immediately update the state with it.
           | EventUninstall InstallationType  -- ^ Triggered when the user clicks on the uninstallation button (the passed enum determines what to do and what to show when the operation completes)
           | EventTryInstall -- ^ Triggered when the user presses the “Try install” button. The next event will be 'EventTryInstallStarted'
           | EventTryInstallStarted NixPackage ProcessData -- ^ Triggered just after the trial installation has started. This initializes the state and goes on to watch the installation process using the 'EventTryInstallWatch' event.
           | EventTryInstallFailed Message -- ^ Triggered when the trial installation failed for some reason.
           | EventTryInstallSuccess -- ^ Triggered when the trial installation succeeded
           | EventTryInstallCancel -- ^ Triggered when the user presses the Cancel button on a trial installation
           | EventTryInstallWatch ProcessData ProcessOutput -- ^ Triggered regularly while the trial installation is running. We cumulate the process output and keep the process data.
           | EventOperationCompleted Message CompletionType -- ^ Whenever an operation (install/uninstall) completes, we emit this event and display a message
           | EventCategoryChanged Int -- ^ Triggered when the category combobox changes
           | EventReload -- ^ This is triggered externally whenever we need to reload the cache. For example, when we rebuild successfully, we need to update the package’s status.
           | EventReloadFinished [NixPackage] -- ^ Contains the new cache when the reloading finished.
