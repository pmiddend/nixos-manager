{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.ManagerMain where

import           System.FilePath                ( (</>) )
import           NixManager.Css
import           Data.Text.IO                   ( putStrLn )
import           Control.Lens                   ( (^.)
                                                , (&)
                                                , (?~)
                                                , (.~)
                                                )
import           NixManager.ManagerState
import           NixManager.Nix
import           NixManager.Util
import           NixManager.Message
import           NixManager.ManagerEvent
import           NixManager.View
import           GI.Gtk.Declarative.App.Simple  ( App(App)
                                                , view
                                                , update
                                                , Transition(Transition, Exit)
                                                , inputs
                                                , initialState
                                                , run
                                                )
import           Control.Monad                  ( void )
import qualified GI.Gtk                        as Gtk
import           Prelude                 hiding ( length
                                                , putStrLn
                                                )

pureTransition :: ManagerState -> Transition ManagerState ManagerEvent
pureTransition x = Transition x (pure Nothing)

tryInstall :: NixPackage -> IO (Maybe ManagerEvent)
tryInstall p = do
  bins <- getExecutables p
  case bins of
    (_, []) -> pure
      (Just
        (ManagerEventShowMessage
          (errorMessage "No binaries found in this package!")
        )
      )
    (bp, [singleBinary]) -> do
      startProgram (bp </> singleBinary)
      pure Nothing
    multipleBinaries -> do
      putStrLn $ "found more bins: " <> showText multipleBinaries
      pure
        (Just
          (ManagerEventShowMessage
            (errorMessage "Multiple binaries found in this package!")
          )
        )

update' :: ManagerState -> ManagerEvent -> Transition ManagerState ManagerEvent
update' _ ManagerEventClosed = Exit
update' s (ManagerEventShowMessage e) =
  pureTransition (s & msLatestMessage ?~ e)
update' s (ManagerEventInstallCompleted cache) = Transition
  (s & msPackageCache .~ cache)
  (pure (Just (ManagerEventShowMessage (infoMessage "Install completed!"))))
update' s (ManagerEventUninstallCompleted cache) = Transition
  (s & msPackageCache .~ cache)
  (pure (Just (ManagerEventShowMessage (infoMessage "Uninstall completed!"))))
update' s ManagerEventInstall = case s ^. msSelectedPackage of
  Nothing       -> pureTransition s
  Just selected -> Transition s $ do
    installResult <- installPackage (selected ^. npName)
    newCache      <- readCache
    case installResult of
      Nothing -> pure (Just (ManagerEventInstallCompleted newCache))
      Just e ->
        pure
          (Just
            (ManagerEventShowMessage (errorMessage ("Install failed: " <> e)))
          )
update' s ManagerEventUninstall = case s ^. msSelectedPackage of
  Nothing       -> pureTransition s
  Just selected -> Transition s $ do
    uninstallResult <- uninstallPackage (selected ^. npName)
    newCache        <- readCache
    case uninstallResult of
      Nothing -> pure (Just (ManagerEventUninstallCompleted newCache))
      Just e ->
        pure
          (Just
            (ManagerEventShowMessage (errorMessage ("Uninstall failed: " <> e)))
          )
update' s ManagerEventTryInstall = case s ^. msSelectedPackage of
  Nothing -> pureTransition s
  Just selected ->
    Transition (s & msInstallingPackage ?~ selected) (tryInstall selected)
update' s (ManagerEventPackageSelected i) =
  pureTransition (s & msSelectedPackageIdx .~ i)
update' s (ManagerEventSearchChanged t) =
  pureTransition (s & msSearchString .~ t)


nixMain :: IO ()
nixMain = do
  void (Gtk.init Nothing)
  putStrLn "Reading cache..."
  putStrLn "Starting..."
  initCss
  cache <- readCache
  void $ run App
    { view         = view'
    , update       = update'
    , inputs       = []
    , initialState = ManagerState { _msPackageCache       = cache
                                  , _msSearchString       = mempty
                                  , _msSelectedPackageIdx = Nothing
                                  , _msInstallingPackage  = Nothing
                                  , _msLatestMessage      = Nothing
                                  }
    }
