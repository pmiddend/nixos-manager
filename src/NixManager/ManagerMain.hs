{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.ManagerMain where

import           NixManager.UpdateHandler       ( update' )
import qualified NixManager.Admin.State        as AdminState
import qualified NixManager.Services.State     as ServicesState
import qualified NixManager.Packages.State     as PackagesState
import           NixManager.View.ErrorDialog    ( runErrorDialog )
import           NixManager.View.Css            ( initCss )
import           NixManager.ManagerState        ( ManagerState(..) )
import           NixManager.Util                ( MaybeError(Success, Error)
                                                , ifSuccessIO
                                                )
import           NixManager.View.Root           ( view' )
import           GI.Gtk.Declarative.App.Simple  ( App(App)
                                                , view
                                                , update
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
initState = ifSuccessIO PackagesState.initState $ \packagesState -> do
  serviceState <- ServicesState.initState
  pure $ Success $ ManagerState { _msPackagesState = packagesState
                                , _msServiceState  = serviceState
                                , _msAdminState    = AdminState.initState
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
