{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.ManagerMain where

import           NixManager.ServiceStateData    ( ServiceStateData
                                                  ( ServiceStateData
                                                  )
                                                , ssdServiceExpression
                                                , ssdSelectedServiceIdx
                                                )
import           NixManager.ServiceState        ( ServiceState
                                                  ( ServiceStateDownloading
                                                  , ServiceStateInvalidOptions
                                                  , ServiceStateInvalidExpr
                                                  , ServiceStateDone
                                                  )
                                                , _ServiceStateDone
                                                , _ServiceStateDownloading
                                                , ssddCounter
                                                , ssddVar
                                                , ServiceStateDownloadingData
                                                  ( ServiceStateDownloadingData
                                                  )
                                                )
import qualified NixManager.ServiceDownload    as ServiceDownload
import           NixManager.View.ErrorDialog    ( runErrorDialog )
import           System.FilePath                ( (</>) )
import           NixManager.View.Css            ( initCss )
import           Data.Text.IO                   ( putStrLn )
import           Data.Foldable                  ( for_ )
import           Control.Lens                   ( (^.)
                                                , over
                                                , (&)
                                                , (^?)
                                                , (?~)
                                                , (.~)
                                                , (+~)
                                                , (^?!)
                                                )
import           NixManager.ManagerState        ( ManagerState(..)
                                                , msInstallingPackage
                                                , msSelectedPackage
                                                , msServiceState
                                                , msLatestMessage
                                                , msPackageCache
                                                , msSelectedPackageIdx
                                                , msSearchString
                                                )
import           NixManager.NixServiceOption    ( readOptionsFile
                                                , locateOptionsFile
                                                )
import           NixManager.NixService          ( makeServices
                                                , writeServiceFile
                                                , readServices
                                                )
import           NixManager.Util                ( MaybeError(Success, Error)
                                                , ifSuccessIO
                                                , showText
                                                , threadDelayMillis
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
import           NixManager.View.Root           ( view' )
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
  let newState = over
        (msServiceState . _ServiceStateDone . ssdServiceExpression)
        setter
        s
  in  Transition newState $ do
        writeServiceFile
          (   newState
          ^?! msServiceState
          .   _ServiceStateDone
          .   ssdServiceExpression
          )
        pure Nothing

update' _ ManagerEventClosed                = Exit
update' s ManagerEventServiceDownloadCancel = Transition s $ do
  for_ (s ^? msServiceState . _ServiceStateDownloading . ssddVar)
       ServiceDownload.cancel
  pure (Just ManagerEventServiceStateReload)
update' s (ManagerEventServiceStateResult newServiceState) =
  pureTransition (s & msServiceState .~ newServiceState)
update' s ManagerEventServiceStateReload =
  Transition s (Just . ManagerEventServiceStateResult <$> initServiceState)
update' s (ManagerEventServiceDownloadCheck var) =
  Transition (s & msServiceState . _ServiceStateDownloading . ssddCounter +~ 1)
    $ do
        downloadResult <- ServiceDownload.result var
        case downloadResult of
          Just (Error e) -> pure
            (Just
              (ManagerEventServiceStateResult
                (ServiceStateInvalidOptions (Just e))
              )
            )
          Just (Success _) -> pure (Just ManagerEventServiceStateReload)
          Nothing          -> threadDelayMillis 500
            >> pure (Just (ManagerEventServiceDownloadCheck var))
update' s (ManagerEventServiceDownloadStarted var) =
  Transition
      (s & msServiceState .~ ServiceStateDownloading
        (ServiceStateDownloadingData 0 var)
      )
    $ do
        threadDelayMillis 500
        pure (Just (ManagerEventServiceDownloadCheck var))
update' s ManagerEventServiceDownloadStart = Transition
  s
  (Just . ManagerEventServiceDownloadStarted <$> ServiceDownload.start)

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
update' s (ManagerEventServiceSelected i) = pureTransition
  (s & msServiceState . _ServiceStateDone . ssdSelectedServiceIdx .~ i)
update' s (ManagerEventSearchChanged t) =
  pureTransition (s & msSearchString .~ t)
update' s ManagerEventDiscard = pureTransition s

-- FIXME: Better happy path
initServiceState :: IO ServiceState
initServiceState = do
  optionsFile' <- locateOptionsFile
  case optionsFile' of
    Nothing          -> pure (ServiceStateInvalidOptions Nothing)
    Just optionsFile -> do
      options' <- readOptionsFile optionsFile
      case options' of
        Error   e       -> pure (ServiceStateInvalidOptions (Just e))
        Success options -> do
          services' <- readServices
          case services' of
            Error   e        -> pure (ServiceStateInvalidExpr e)
            Success services -> pure $ ServiceStateDone
              (ServiceStateData (makeServices options) Nothing services)


initState :: IO (MaybeError ManagerState)
initState = ifSuccessIO readCache $ \cache -> do
  serviceState <- initServiceState
  pure $ Success $ ManagerState { _msPackageCache       = cache
                                , _msSearchString       = mempty
                                , _msSelectedPackageIdx = Nothing
                                , _msInstallingPackage  = Nothing
                                , _msLatestMessage      = Nothing
                                , _msServiceState       = serviceState
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
