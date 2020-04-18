{-|
  Description: Contains the update logic for the Packages tab
Contains the update logic for the Packages tab
  -}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.HMPackages.Update
  ( updateEvent
  )
where

import           Control.Lens                   ( (&)
                                                , (^?)
                                                , (?~)
                                                , (.~)
                                                )
import           NixManager.ManagerState        ( ManagerState(..)
                                                , msHMPackagesState
                                                )
import           NixManager.HMPackages.Event    ( Event
                                                  ( EventOperationCompleted
                                                  , EventInstallCompleted
                                                  , EventUninstallCompleted
                                                  , EventReload
                                                  , EventReloadFinished
                                                  , EventPackageEditView
                                                  )
                                                )
import qualified NixManager.HMAdmin.Event      as HMAdminEvent
import           NixManager.Message             ( errorMessage
                                                , infoMessage
                                                , Message
                                                )
import           NixManager.ManagerEvent        ( ManagerEvent
                                                  ( ManagerEventHMPackages
                                                  )
                                                , pureTransition
                                                , liftUpdate
                                                , hmPackagesEvent
                                                , hmAdminEvent
                                                )
import           NixManager.HMPackagesUtil      ( installPackage
                                                , readPackageCache
                                                , uninstallPackage
                                                )
import           GI.Gtk.Declarative.App.Simple  ( Transition(Transition) )
import           Prelude                 hiding ( length
                                                , putStrLn
                                                )
import qualified NixManager.View.PackageEditView
                                               as PEV

-- | What message to display when the install operation completes
installCompletedMessage :: PEV.InstallationType -> Message
installCompletedMessage PEV.Uncancelled = infoMessage
  "Marked for installation! Head to the Admin tab to apply the changes."
installCompletedMessage PEV.Cancelled = infoMessage "Uninstall cancelled!"

-- | What message to display when the uninstall operation completes
uninstallCompletedMessage :: PEV.InstallationType -> Message
uninstallCompletedMessage PEV.Uncancelled = infoMessage
  "Marked for uninstall! Head to the Admin tab to apply the changes."
uninstallCompletedMessage PEV.Cancelled = infoMessage "Installation cancelled!"

-- | The actual update function
updateEvent :: ManagerState -> Event -> Transition ManagerState ManagerEvent
updateEvent s (EventOperationCompleted e completionType) =
  Transition (s & msHMPackagesState . PEV.psLatestMessage ?~ e)
    $ case completionType of
        PEV.CompletionReload -> pure (hmAdminEvent HMAdminEvent.EventReload)
        PEV.CompletionPass   -> pure Nothing
updateEvent s (EventInstallCompleted cache installationType) = Transition
  (  s
  &  msHMPackagesState
  .  PEV.psPackageCache
  .~ cache
  &  msHMPackagesState
  .  PEV.psSelectedIdx
  .~ Nothing
  )
  (pure
    (hmPackagesEvent
      (EventOperationCompleted (installCompletedMessage installationType)
                               PEV.CompletionReload
      )
    )
  )
updateEvent s (EventUninstallCompleted cache installationType) = Transition
  (  s
  &  msHMPackagesState
  .  PEV.psPackageCache
  .~ cache
  &  msHMPackagesState
  .  PEV.psSelectedIdx
  .~ Nothing
  )
  (pure
    (hmPackagesEvent
      (EventOperationCompleted (uninstallCompletedMessage installationType)
                               PEV.CompletionReload
      )
    )
  )
updateEvent s (EventPackageEditView (PEV.EventInstall installationType)) =
  case s ^? msHMPackagesState . PEV.psSelectedPackage of
    Nothing       -> pureTransition s
    Just selected -> Transition
      s
      do
        installResult <- installPackage selected
        cacheResult   <- readPackageCache
        case installResult >>= const cacheResult of
          Right newCache ->
            pure
              (hmPackagesEvent (EventInstallCompleted newCache installationType)
              )
          Left e -> pure
            (hmPackagesEvent
              (EventOperationCompleted
                (errorMessage ("Install failed: " <> e))
                PEV.CompletionReload
              )
            )
updateEvent s (EventPackageEditView (PEV.EventUninstall installationType)) =
  case s ^? msHMPackagesState . PEV.psSelectedPackage of
    Nothing       -> pureTransition s
    Just selected -> Transition
      s
      do
        uninstallResult <- uninstallPackage selected
        cacheResult     <- readPackageCache
        case uninstallResult >>= const cacheResult of
          Right newCache ->
            pure
              (hmPackagesEvent
                (EventUninstallCompleted newCache installationType)
              )
          Left e -> pure
            (hmPackagesEvent
              (EventOperationCompleted
                (errorMessage ("Uninstall failed: " <> e))
                PEV.CompletionReload
              )
            )
updateEvent s EventReload = Transition
  s
  do
    cacheResult <- readPackageCache
    case cacheResult of
      Right newCache -> pure (hmPackagesEvent (EventReloadFinished newCache))
      Left  e        -> pure
        (hmPackagesEvent
          (EventOperationCompleted
            (errorMessage ("Couldn't reload packages cache: " <> e))
            PEV.CompletionPass
          )
        )
updateEvent s (EventReloadFinished newCache) =
  pureTransition (s & msHMPackagesState . PEV.psPackageCache .~ newCache)
updateEvent s (EventPackageEditView e) = liftUpdate
  PEV.updateEvent
  msHMPackagesState
  (ManagerEventHMPackages . EventPackageEditView)
  s
  e
