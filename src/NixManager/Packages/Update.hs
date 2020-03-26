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
import           NixManager.Packages.State      ( psInstallingPackage
                                                , isProcessData
                                                , psSelectedPackage
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
                                                  , EventTryInstallWatch
                                                  , EventTryInstallStarted
                                                  , EventInstall
                                                  , EventInstallCompleted
                                                  , EventUninstallCompleted
                                                  , EventUninstall
                                                  , EventTryInstallCancel
                                                  , EventTryInstall
                                                  , EventTryInstallFailed
                                                  , EventShowMessage
                                                  )
                                                )
import           NixManager.Util                ( MaybeError(Success, Error)
                                                , replaceHtmlEntities
                                                , decodeUtf8
                                                , showText
                                                , threadDelayMillis
                                                )
import           NixManager.Message             ( errorMessage
                                                , infoMessage
                                                )
import           NixManager.ManagerEvent        ( ManagerEvent(..) )
import           NixManager.PackageSearch       ( installPackage
                                                , readCache
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

packagesEvent :: Event -> Maybe ManagerEvent
packagesEvent = Just . ManagerEventPackages

pureTransition :: ManagerState -> Transition ManagerState ManagerEvent
pureTransition x = Transition x (pure Nothing)



updateEvent :: ManagerState -> Event -> Transition ManagerState ManagerEvent
updateEvent s (EventShowMessage e) =
  pureTransition (s & msPackagesState . psLatestMessage ?~ e)
updateEvent s (EventInstallCompleted cache) = Transition
  (s & msPackagesState . psPackageCache .~ cache)
  (pure (packagesEvent (EventShowMessage (infoMessage "Install completed!"))))
updateEvent s (EventUninstallCompleted cache) = Transition
  (s & msPackagesState . psPackageCache .~ cache)
  (pure (packagesEvent (EventShowMessage (infoMessage "Uninstall completed!"))))
updateEvent s EventInstall = case s ^. msPackagesState . psSelectedPackage of
  Nothing       -> pureTransition s
  Just selected -> Transition s $ do
    installResult <- installPackage (selected ^. npName)
    cacheResult   <- readCache
    case installResult >>= const cacheResult of
      Success newCache -> pure (packagesEvent (EventInstallCompleted newCache))
      Error e ->
        pure
          (packagesEvent
            (EventShowMessage (errorMessage ("Install failed: " <> e)))
          )
updateEvent s EventUninstall = case s ^. msPackagesState . psSelectedPackage of
  Nothing       -> pureTransition s
  Just selected -> Transition s $ do
    uninstallResult <- uninstallPackage (selected ^. npName)
    cacheResult     <- readCache
    case uninstallResult >>= const cacheResult of
      Success newCache ->
        pure (packagesEvent (EventUninstallCompleted newCache))
      Error e -> pure
        (packagesEvent
          (EventShowMessage (errorMessage ("Uninstall failed: " <> e)))
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
                      pure Nothing
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
updateEvent s (EventSearchChanged t) =
  pureTransition (s & msPackagesState . psSearchString .~ t)
