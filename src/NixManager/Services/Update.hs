module NixManager.Services.Update
  ( updateEvent
  )
where

import           NixManager.Services.StateData  ( sdExpression
                                                , sdSelectedIdx
                                                )
import           NixManager.Services.State      ( State
                                                  ( StateDownloading
                                                  , StateInvalidOptions
                                                  )
                                                , _StateDone
                                                , _StateDownloading
                                                , sddCounter
                                                , initState
                                                , sddVar
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
                                                , (^?!)
                                                )
import           NixManager.ManagerState        ( ManagerState(..)
                                                , msServiceState
                                                )
import           NixManager.NixService          ( writeServiceFile )
import           NixManager.Services.Event      ( Event
                                                  ( EventDownloadStart
                                                  , EventSettingChanged
                                                  , EventDownloadCancel
                                                  , EventStateResult
                                                  , EventStateReload
                                                  , EventDownloadCheck
                                                  , EventDownloadStarted
                                                  , EventSelected
                                                  )
                                                )
import           NixManager.Util                ( MaybeError(Success, Error)
                                                , threadDelayMillis
                                                )
import           NixManager.ManagerEvent        ( ManagerEvent(..) )
import           GI.Gtk.Declarative.App.Simple  ( Transition(Transition) )
import           Prelude                 hiding ( length
                                                , putStrLn
                                                )


servicesEvent :: Event -> Maybe ManagerEvent
servicesEvent = Just . ManagerEventServices

pureTransition :: ManagerState -> Transition ManagerState ManagerEvent
pureTransition x = Transition x (pure Nothing)

updateEvent :: ManagerState -> Event -> Transition ManagerState ManagerEvent
updateEvent s EventDownloadStart =
  Transition s (servicesEvent . EventDownloadStarted <$> ServiceDownload.start)
updateEvent s (EventSettingChanged setter) =
  let newState = over (msServiceState . _StateDone . sdExpression) setter s
  in  Transition newState $ do
        writeServiceFile
          (newState ^?! msServiceState . _StateDone . sdExpression)
        pure Nothing
updateEvent s EventDownloadCancel = Transition s $ do
  for_ (s ^? msServiceState . _StateDownloading . sddVar) ServiceDownload.cancel
  pure (servicesEvent EventStateReload)
updateEvent s (EventStateResult newServiceState) =
  pureTransition (s & msServiceState .~ newServiceState)
updateEvent s EventStateReload =
  Transition s (servicesEvent . EventStateResult <$> initState)
updateEvent s (EventDownloadCheck var) =
  Transition (s & msServiceState . _StateDownloading . sddCounter +~ 1) $ do
    downloadResult <- ServiceDownload.result var
    case downloadResult of
      Just (Error e) ->
        pure (servicesEvent (EventStateResult (StateInvalidOptions (Just e))))
      Just (Success _) -> pure (servicesEvent EventStateReload)
      Nothing ->
        threadDelayMillis 500 >> pure (servicesEvent (EventDownloadCheck var))
updateEvent s (EventDownloadStarted var) =
  Transition
      (s & msServiceState .~ StateDownloading (StateDownloadingData 0 var))
    $ do
        threadDelayMillis 500
        pure (servicesEvent (EventDownloadCheck var))
updateEvent s (EventSelected i) =
  pureTransition (s & msServiceState . _StateDone . sdSelectedIdx .~ i)
