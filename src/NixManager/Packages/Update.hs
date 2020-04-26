{-|
  Description: Contains the update logic for the Packages tab
Contains the update logic for the Packages tab
  -}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.Packages.Update
  ( updateEvent
  )
where

import           Data.Functor                   ( ($>) )
import           Data.Validation                ( Validation(Failure, Success)
                                                , bindValidation
                                                )
import           Control.Lens                   ( (&)
                                                , (?~)
                                                , (.~)
                                                , (^?)
                                                )
import           NixManager.ManagerState        ( ManagerState(..) )
import           NixManager.Packages.Event      ( Event
                                                  ( EventOperationCompleted
                                                  , EventInstallCompleted
                                                  , EventUninstallCompleted
                                                  , EventReload
                                                  , EventReloadFinished
                                                  , EventPackageEditView
                                                  )
                                                )
import qualified NixManager.Admin.Event        as AdminEvent
import           NixManager.Message             ( errorMessage
                                                , infoMessage
                                                , Message
                                                )
import           NixManager.ManagerEvent        ( ManagerEvent
                                                  ( ManagerEventPackages
                                                  )
                                                , pureTransition
                                                , liftUpdate
                                                , packagesEvent
                                                , adminEvent
                                                )
import           NixManager.NixPackagesUtil     ( installPackage
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
  Transition (s & #packagesState . #latestMessage ?~ e) $ case completionType of
    PEV.CompletionReload -> pure (adminEvent AdminEvent.EventReload)
    PEV.CompletionPass   -> pure Nothing
updateEvent s (EventInstallCompleted cache installationType) = Transition
  (  s
  &  #packagesState
  .  #packageCache
  .~ cache
  &  #packagesState
  .  #selectedIdx
  .~ Nothing
  )
  (pure
    (packagesEvent
      (EventOperationCompleted (installCompletedMessage installationType)
                               PEV.CompletionReload
      )
    )
  )
updateEvent s (EventUninstallCompleted cache installationType) = Transition
  (  s
  &  #packagesState
  .  #packageCache
  .~ cache
  &  #packagesState
  .  #selectedIdx
  .~ Nothing
  )
  (pure
    (packagesEvent
      (EventOperationCompleted (uninstallCompletedMessage installationType)
                               PEV.CompletionReload
      )
    )
  )
updateEvent s (EventPackageEditView (PEV.EventInstall installationType)) =
  case s ^? #packagesState . PEV.selectedPackage of
    Nothing       -> pureTransition s
    Just selected -> Transition s $ do
      installResult <- installPackage selected
      cacheResult   <- readPackageCache
      case installResult *> cacheResult of
        Success newCache ->
          pure (packagesEvent (EventInstallCompleted newCache installationType))
        Failure e -> pure
          (packagesEvent
            (EventOperationCompleted (errorMessage ("Install failed: " <> e))
                                     PEV.CompletionReload
            )
          )
updateEvent s (EventPackageEditView (PEV.EventUninstall installationType)) =
  case s ^? #packagesState . PEV.selectedPackage of
    Nothing       -> pureTransition s
    Just selected -> Transition s $ do
      uninstallResult <- uninstallPackage selected
      cacheResult     <- readPackageCache
      case uninstallResult *> cacheResult of
        Success newCache -> pure
          (packagesEvent (EventUninstallCompleted newCache installationType))
        Failure e -> pure
          (packagesEvent
            (EventOperationCompleted
              (errorMessage ("Uninstall failed: " <> e))
              PEV.CompletionReload
            )
          )
updateEvent s EventReload = Transition s $ do
  cacheResult <- readPackageCache
  case cacheResult of
    Success newCache -> pure (packagesEvent (EventReloadFinished newCache))
    Failure e        -> pure
      (packagesEvent
        (EventOperationCompleted
          (errorMessage ("Couldn't reload packages cache: " <> e))
          PEV.CompletionPass
        )
      )
updateEvent s (EventReloadFinished newCache) =
  pureTransition (s & #packagesState . #packageCache .~ newCache)
updateEvent s (EventPackageEditView e) = liftUpdate
  PEV.updateEvent
  #packagesState
  (ManagerEventPackages . EventPackageEditView)
  s
  e
