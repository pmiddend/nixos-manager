{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.Packages.Update
  ( updateEvent
  )
where

import           Data.Foldable                  ( for_ )
import           System.Exit                    ( ExitCode
                                                  ( ExitSuccess
                                                  , ExitFailure
                                                  )
                                                )
import           NixManager.Process             ( updateProcess
                                                , poResult
                                                , terminate
                                                , poStderr
                                                , poStdout
                                                )
import           System.FilePath                ( (</>) )
import           Data.Text.IO                   ( putStrLn )
import           Data.Monoid                    ( getFirst )
import           Control.Lens                   ( (^.)
                                                , (&)
                                                , (?~)
                                                , (^?!)
                                                , (^?)
                                                , folded
                                                , traversed
                                                , (.~)
                                                , to
                                                , (+~)
                                                )
import           NixManager.ManagerState        ( ManagerState(..)
                                                , msPackagesState
                                                )
import qualified NixManager.Admin.Event        as AdminEvent
import           NixManager.Packages.State      ( psInstallingPackage
                                                , isProcessData
                                                , psSelectedPackage
                                                , psCategoryIdx
                                                , psLatestMessage
                                                , psPackageCache
                                                , psSelectedIdx
                                                , psSearchString
                                                , isPackage
                                                , isCounter
                                                , InstallingState
                                                  ( InstallingState
                                                  )
                                                )
import           NixManager.Packages.Event      ( Event
                                                  ( EventSearchChanged
                                                  , EventPackageSelected
                                                  , EventReload
                                                  , EventReloadFinished
                                                  , EventTryInstallWatch
                                                  , EventCategoryChanged
                                                  , EventTryInstallStarted
                                                  , EventInstall
                                                  , EventInstallCompleted
                                                  , EventUninstallCompleted
                                                  , EventTryInstallSuccess
                                                  , EventUninstall
                                                  , EventTryInstallCancel
                                                  , EventTryInstall
                                                  , EventTryInstallFailed
                                                  , EventOperationCompleted
                                                  )
                                                , CompletionType
                                                  ( CompletionReload
                                                  , CompletionPass
                                                  )
                                                , InstallationType
                                                  ( Cancelled
                                                  , Uncancelled
                                                  )
                                                )
import           NixManager.Util                ( TextualError
                                                , replaceHtmlEntities
                                                , decodeUtf8
                                                , showText
                                                , threadDelayMillis
                                                )
import           NixManager.Message             ( errorMessage
                                                , infoMessage
                                                , Message
                                                )
import           NixManager.ManagerEvent        ( ManagerEvent
                                                , pureTransition
                                                , packagesEvent
                                                , adminEvent
                                                )
import           NixManager.NixPackages         ( installPackage
                                                , readPackageCache
                                                , dryInstall
                                                , startProgram
                                                , uninstallPackage
                                                , executablesFromStorePath
                                                )
import           NixManager.NixPackage          ( npName )
import           GI.Gtk.Declarative.App.Simple  ( Transition(Transition) )
import           Prelude                 hiding ( length
                                                , putStrLn
                                                )

installCompletedMessage :: InstallationType -> Message
installCompletedMessage Uncancelled = infoMessage
  "Marked for installation! Head to the Admin tab to apply the changes."
installCompletedMessage Cancelled = infoMessage "Uninstall cancelled!"

uninstallCompletedMessage :: InstallationType -> Message
uninstallCompletedMessage Uncancelled = infoMessage
  "Marked for uninstall! Head to the Admin tab to apply the changes."
uninstallCompletedMessage Cancelled = infoMessage "Installation cancelled!"

updateEvent :: ManagerState -> Event -> Transition ManagerState ManagerEvent
updateEvent s (EventCategoryChanged newCategory) =
  pureTransition (s & msPackagesState . psCategoryIdx .~ newCategory)
updateEvent s (EventOperationCompleted e completionType) =
  Transition (s & msPackagesState . psLatestMessage ?~ e)
    $ case completionType of
        CompletionReload -> pure (adminEvent AdminEvent.EventReload)
        CompletionPass   -> pure Nothing
updateEvent s (EventInstallCompleted cache installationType) = Transition
  (s & msPackagesState . psPackageCache .~ cache)
  (pure
    (packagesEvent
      (EventOperationCompleted (installCompletedMessage installationType)
                               CompletionReload
      )
    )
  )
updateEvent s (EventUninstallCompleted cache installationType) = Transition
  (s & msPackagesState . psPackageCache .~ cache)
  (pure
    (packagesEvent
      (EventOperationCompleted (uninstallCompletedMessage installationType)
                               CompletionReload
      )
    )
  )
updateEvent s (EventInstall installationType) =
  case s ^. msPackagesState . psSelectedPackage of
    Nothing       -> pureTransition s
    Just selected -> Transition s $ do
      installResult <- installPackage (selected ^. npName)
      cacheResult   <- readPackageCache
      case installResult >>= const cacheResult of
        Right newCache ->
          pure (packagesEvent (EventInstallCompleted newCache installationType))
        Left e -> pure
          (packagesEvent
            (EventOperationCompleted (errorMessage ("Install failed: " <> e))
                                     CompletionReload
            )
          )
updateEvent s (EventUninstall installationType) =
  case s ^. msPackagesState . psSelectedPackage of
    Nothing       -> pureTransition s
    Just selected -> Transition s $ do
      uninstallResult <- uninstallPackage (selected ^. npName)
      cacheResult     <- readPackageCache
      case uninstallResult >>= const cacheResult of
        Right newCache -> pure
          (packagesEvent (EventUninstallCompleted newCache installationType))
        Left e -> pure
          (packagesEvent
            (EventOperationCompleted
              (errorMessage ("Uninstall failed: " <> e))
              CompletionReload
            )
          )
updateEvent s EventTryInstallCancel =
  Transition (s & msPackagesState . psInstallingPackage .~ Nothing) $ do
    for_
      (s ^? msPackagesState . psInstallingPackage . traversed . isProcessData)
      terminate
    pure Nothing
updateEvent s EventTryInstall =
  case s ^. msPackagesState . psSelectedPackage of
    Nothing       -> pureTransition s
    Just selected -> Transition s $ do
      pd <- dryInstall selected
      pure (packagesEvent (EventTryInstallStarted selected pd))
updateEvent s (EventTryInstallStarted pkg pd) = Transition
  (s & msPackagesState . psInstallingPackage ?~ InstallingState pkg 0 pd)
  (pure (packagesEvent (EventTryInstallWatch pd mempty)))
updateEvent s (EventTryInstallFailed e) = pureTransition
  (  s
  &  msPackagesState
  .  psLatestMessage
  ?~ e
  &  msPackagesState
  .  psInstallingPackage
  .~ Nothing
  )
updateEvent s EventTryInstallSuccess = pureTransition
  (  s
  &  msPackagesState
  .  psLatestMessage
  ?~ infoMessage
       "Downloaded and started the application!\nIf nothing happens, it's probably a terminal application and cannot be started from NixOS manager."

  &  msPackagesState
  .  psInstallingPackage
  .~ Nothing
  )
updateEvent s (EventTryInstallWatch pd po) =
  Transition
      (s & msPackagesState . psInstallingPackage . traversed . isCounter +~ 1)
    $ do
        newOutput <- (po <>) <$> updateProcess pd
        case newOutput ^. poResult . to getFirst of
          Nothing -> do
            threadDelayMillis 500
            pure (packagesEvent (EventTryInstallWatch pd newOutput))
          Just ExitSuccess ->
            executablesFromStorePath
                (   s
                ^?! msPackagesState
                .   psInstallingPackage
                .   folded
                .   isPackage
                )
                (newOutput ^. poStdout)
              >>= \case
                    (_, []) -> pure
                      (packagesEvent
                        (EventTryInstallFailed
                          (errorMessage "No binaries found in this package!")
                        )
                      )
                    (bp, [singleBinary]) -> do
                      startProgram (bp </> singleBinary)
                      pure (packagesEvent EventTryInstallSuccess)
                    multipleBinaries -> do
                      putStrLn
                        $  "found more bins: "
                        <> showText multipleBinaries
                      pure
                        (packagesEvent
                          (EventTryInstallFailed
                            (errorMessage
                              "Multiple binaries found in this package!"
                            )
                          )
                        )
          Just (ExitFailure code) -> pure
            (packagesEvent
              (EventTryInstallFailed
                (errorMessage
                  (  "Installing failed, exit code: "
                  <> showText code
                  <> ", standard error:\n<tt>"
                  <> replaceHtmlEntities (newOutput ^. poStderr . decodeUtf8)
                  <> "</tt>"
                  )
                )
              )
            )

updateEvent s (EventPackageSelected i) =
  pureTransition (s & msPackagesState . psSelectedIdx .~ i)
updateEvent s (EventSearchChanged t) = pureTransition
  (  s
  &  msPackagesState
  .  psSearchString
  .~ t
  &  msPackagesState
  .  psSelectedIdx
  .~ Nothing
  )
updateEvent s EventReload = Transition s $ do
  cacheResult <- readPackageCache
  case cacheResult of
    Right newCache -> pure (packagesEvent (EventReloadFinished newCache))
    Left   e        -> pure
      (packagesEvent
        (EventOperationCompleted
          (errorMessage ("Couldn't reload packages cache: " <> e))
          CompletionPass
        )
      )
updateEvent s (EventReloadFinished newCache) =
  pureTransition (s & msPackagesState . psPackageCache .~ newCache)
