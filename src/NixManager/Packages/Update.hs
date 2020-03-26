{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.Packages.Update
  ( updateEvent
  )
where

import           System.FilePath                ( (</>) )
import           Data.Text.IO                   ( putStrLn )
import           Control.Lens                   ( (^.)
                                                , (&)
                                                , (?~)
                                                , (.~)
                                                )
import           NixManager.ManagerState        ( ManagerState(..)
                                                , msPackagesState
                                                )
import           NixManager.Packages.State      ( psInstallingPackage
                                                , psSelectedPackage
                                                , psLatestMessage
                                                , psPackageCache
                                                , psSelectedIdx
                                                , psSearchString
                                                )
import           NixManager.Packages.Event      ( Event
                                                  ( EventSearchChanged
                                                  , EventPackageSelected
                                                  , EventInstall
                                                  , EventInstallCompleted
                                                  , EventUninstallCompleted
                                                  , EventUninstall
                                                  , EventTryInstall
                                                  , EventShowMessage
                                                  )
                                                )
import           NixManager.Util                ( MaybeError(Success, Error)
                                                , showText
                                                )
import           NixManager.Message             ( errorMessage
                                                , infoMessage
                                                )
import           NixManager.ManagerEvent        ( ManagerEvent(..) )
import           NixManager.PackageSearch       ( installPackage
                                                , readCache
                                                , startProgram
                                                , uninstallPackage
                                                , getExecutables
                                                )
import           NixManager.NixPackage          ( NixPackage
                                                , npName
                                                )
import           GI.Gtk.Declarative.App.Simple  ( Transition(Transition) )
import           Prelude                 hiding ( length
                                                , putStrLn
                                                )

packagesEvent :: Event -> Maybe ManagerEvent
packagesEvent = Just . ManagerEventPackages

tryInstall :: NixPackage -> IO (Maybe ManagerEvent)
tryInstall p = do
  bins <- getExecutables p
  case bins of
    (_, []) -> pure
      (packagesEvent
        (EventShowMessage (errorMessage "No binaries found in this package!"))
      )
    (bp, [singleBinary]) -> do
      startProgram (bp </> singleBinary)
      pure Nothing
    multipleBinaries -> do
      putStrLn $ "found more bins: " <> showText multipleBinaries
      pure
        (packagesEvent
          (EventShowMessage
            (errorMessage "Multiple binaries found in this package!")
          )
        )

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
updateEvent s EventTryInstall =
  case s ^. msPackagesState . psSelectedPackage of
    Nothing       -> pureTransition s
    Just selected -> Transition
      (s & msPackagesState . psInstallingPackage ?~ selected)
      (tryInstall selected)
updateEvent s (EventPackageSelected i) =
  pureTransition (s & msPackagesState . psSelectedIdx .~ i)
updateEvent s (EventSearchChanged t) =
  pureTransition (s & msPackagesState . psSearchString .~ t)
