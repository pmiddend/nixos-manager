{-|
  Description: Contains the update logic for the Services tab
Contains the update logic for the Services tab
  -}
{-# LANGUAGE OverloadedLabels #-}
module NixManager.Services.Update
  ( updateEvent
  )
where

import           Data.Validation                ( Validation(Failure, Success) )
import           NixManager.ManagerEvent        ( servicesEvent
                                                , pureTransition
                                                , ManagerEvent
                                                )
import qualified NixManager.View.ServiceEditView
                                               as EditView
import           NixManager.Services.State      ( State
                                                  ( StateDownloading
                                                  , StateInvalidOptions
                                                  )
                                                , initState
                                                , StateDownloadingData
                                                  ( StateDownloadingData
                                                  )
                                                )
import qualified NixManager.Services.Download  as ServiceDownload
import           Data.Foldable                  ( for_ )
import           Control.Lens                   ( over
                                                , (&)
                                                , (^?)
                                                , (.~)
                                                , (+~)
                                                , (%~)
                                                , (^?!)
                                                )
import           NixManager.ManagerState        ( ManagerState(..) )
import           NixManager.NixServicesUtil     ( writeLocalServiceFile )
import           NixManager.Services.Event      ( Event
                                                  ( EventDownloadStart
                                                  , EventDownloadCancel
                                                  , EventStateResult
                                                  , EventEditView
                                                  , EventStateReload
                                                  , EventDownloadCheck
                                                  , EventDownloadStarted
                                                  )
                                                )
import           NixManager.Util                ( threadDelayMillis )
import           GI.Gtk.Declarative.App.Simple  ( Transition(Transition) )
import           Prelude                 hiding ( length
                                                , putStrLn
                                                )


-- | The actual update function
updateEvent :: ManagerState -> Event -> Transition ManagerState ManagerEvent
updateEvent s EventDownloadStart =
  Transition s (servicesEvent . EventDownloadStarted <$> ServiceDownload.start)
updateEvent s (EventEditView (EditView.EditViewSettingChanged setter)) =
  let newState = over (#serviceState . #_StateDone . #expression) setter s
  in  Transition newState $ do
        writeLocalServiceFile
          (newState ^?! #serviceState . #_StateDone . #expression)
        pure Nothing
updateEvent s (EventEditView e) =
  pureTransition (s & #serviceState . #_StateDone %~ EditView.updateEvent e)
updateEvent s EventDownloadCancel = Transition s $ do
  for_ (s ^? #serviceState . #_StateDownloading . #var) ServiceDownload.cancel
  pure (servicesEvent EventStateReload)
updateEvent s (EventStateResult newServiceState) =
  pureTransition (s & #serviceState .~ newServiceState)
updateEvent s EventStateReload =
  Transition s (servicesEvent . EventStateResult <$> initState)
updateEvent s (EventDownloadCheck var) =
  Transition (s & #serviceState . #_StateDownloading . #counter +~ 1) $ do
    downloadResult <- ServiceDownload.result var
    case downloadResult of
      Just (Failure e) ->
        pure (servicesEvent (EventStateResult (StateInvalidOptions (Just e))))
      Just (Success _) -> pure (servicesEvent EventStateReload)
      Nothing ->
        threadDelayMillis 500 >> pure (servicesEvent (EventDownloadCheck var))
updateEvent s (EventDownloadStarted var) =
  Transition
      (s & #serviceState .~ StateDownloading (StateDownloadingData 0 var))
    $ do
        threadDelayMillis 500
        pure (servicesEvent (EventDownloadCheck var))
