module NixManager.HMServices.Update
  ( updateEvent
  )
where

import           NixManager.HMServices.Event    ( Event
                                                  ( EventEditView
                                                  , EventReload
                                                  , EventReloaded
                                                  )
                                                )
import qualified NixManager.View.ServiceEditView
                                               as EditView
import           NixManager.HMServices.State    ( _HomeManagerPresent )
import           Control.Lens                   ( over
                                                , (^?!)
                                                , (%~)
                                                , (&)
                                                )
import           GI.Gtk.Declarative.App.Simple  ( Transition(Transition) )
import           NixManager.HMServicesUtil      ( writePendingServicesFile )
import           NixManager.Services.StateData  ( sdExpression )
import           NixManager.ManagerState        ( ManagerState
                                                , msHMServiceState
                                                )
import           NixManager.ManagerEvent        ( ManagerEvent
                                                , pureTransition
                                                )

updateEvent :: ManagerState -> Event -> Transition ManagerState ManagerEvent
updateEvent s (EventEditView (EditView.EditViewSettingChanged setter)) =
  let newState =
          over (msHMServiceState . _HomeManagerPresent . sdExpression) setter s
  in  Transition newState $ do
        writePendingServicesFile
          (newState ^?! msHMServiceState . _HomeManagerPresent . sdExpression)
        pure Nothing
updateEvent s (EventEditView e) = pureTransition
  (s & msHMServiceState . _HomeManagerPresent %~ EditView.updateEvent e)
updateEvent s EventReload       = pureTransition s
updateEvent s (EventReloaded _) = pureTransition s
