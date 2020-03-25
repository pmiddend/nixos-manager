{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.ManagerMain where

import           NixManager.UpdateHandler       ( update' )
import           NixManager.AdminState          ( AdminState(AdminStateNothing)
                                                )
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
                                                , initServiceState
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
import           NixManager.AdminEvent          ( AdminEvent
                                                  ( AdminEventRebuild
                                                  , AdminEventRebuildStarted
                                                  )
                                                )
import           NixManager.ManagerState        ( ManagerState(..)
                                                , msInstallingPackage
                                                , msSelectedPackage
                                                , msAdminState
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
import           NixManager.Rebuild             ( rebuild )
import           NixManager.PackageSearch       ( installPackage
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

initState :: IO (MaybeError ManagerState)
initState = ifSuccessIO readCache $ \cache -> do
  serviceState <- initServiceState
  pure $ Success $ ManagerState { _msPackageCache       = cache
                                , _msSearchString       = mempty
                                , _msSelectedPackageIdx = Nothing
                                , _msInstallingPackage  = Nothing
                                , _msLatestMessage      = Nothing
                                , _msServiceState       = serviceState
                                , _msAdminState = AdminStateNothing mempty
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
