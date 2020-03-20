{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.ManagerMain where

import           NixManager.ErrorDialog         ( runErrorDialog )
import           System.FilePath                ( (</>) )
import           NixManager.Css
import           Data.Text.IO                   ( putStrLn )
import           Control.Lens                   ( (^.)
                                                , over
                                                , (&)
                                                , (?~)
                                                , (.~)
                                                )
import           NixManager.ManagerState        ( ManagerState(..)
                                                , msInstallingPackage
                                                , msSelectedPackage
                                                , msServiceExpression
                                                , msLatestMessage
                                                , msPackageCache
                                                , msSelectedServiceIdx
                                                , msSelectedPackageIdx
                                                , msSearchString
                                                )
import           NixManager.NixServiceOption    ( readOptionsFile )
import           NixManager.NixService          ( makeServices
                                                , readServiceFile
                                                , writeServiceFile
                                                )
import           NixManager.Util                ( MaybeError(Success, Error)
                                                , ifSuccessIO
                                                , showText
                                                )
import           NixManager.Message             ( errorMessage
                                                , infoMessage
                                                )
import           NixManager.ManagerEvent        ( ManagerEvent(..) )
import           NixManager.Nix                 ( installPackage
                                                , readCache
                                                , startProgram
                                                , uninstallPackage
                                                , getExecutables
                                                )
import           NixManager.NixPackage          ( NixPackage
                                                , npName
                                                )
import           NixManager.View                ( view' )
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
update' s (ManagerEventSettingChanged setter) =
  let newState = over msServiceExpression setter s
  in  Transition newState $ do
        writeServiceFile (newState ^. msServiceExpression)
        pure Nothing

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
    cacheResult   <- readCache
    case installResult >>= const cacheResult of
      Success newCache -> pure (Just (ManagerEventInstallCompleted newCache))
      Error e ->
        pure
          (Just
            (ManagerEventShowMessage (errorMessage ("Install failed: " <> e)))
          )
update' s ManagerEventUninstall = case s ^. msSelectedPackage of
  Nothing       -> pureTransition s
  Just selected -> Transition s $ do
    uninstallResult <- uninstallPackage (selected ^. npName)
    cacheResult     <- readCache
    case uninstallResult >>= const cacheResult of
      Success newCache -> pure (Just (ManagerEventUninstallCompleted newCache))
      Error e ->
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
update' s (ManagerEventServiceSelected i) =
  pureTransition (s & msSelectedServiceIdx .~ i)
update' s (ManagerEventSearchChanged t) =
  pureTransition (s & msSearchString .~ t)
update' s ManagerEventDiscard = pureTransition s

initState :: IO (MaybeError ManagerState)
initState = ifSuccessIO readServiceFile $ \serviceExpr ->
  ifSuccessIO (readOptionsFile "options.json") $ \options ->
    ifSuccessIO readCache $ \cache -> pure $ Success $ ManagerState
      { _msPackageCache       = cache
      , _msSearchString       = mempty
      , _msSelectedPackageIdx = Nothing
      , _msInstallingPackage  = Nothing
      , _msLatestMessage      = Nothing
      , _msServiceCache       = makeServices options
      , _msSelectedServiceIdx = Nothing
      , _msServiceExpression  = serviceExpr
      }

nixMain :: IO ()
nixMain = do
  void (Gtk.init Nothing)
  initCss
  initialState' <- initState
  case initialState' of
    Error e -> runErrorDialog e
    Success s ->
      void $ run App { view         = view'
                     , update       = update'
                     , inputs       = []
                     , initialState = s
                     }
