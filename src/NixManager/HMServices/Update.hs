{-# LANGUAGE OverloadedLabels #-}
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
import           Control.Lens                   ( over
                                                , (^?!)
                                                , (%~)
                                                , (&)
                                                )
import           GI.Gtk.Declarative.App.Simple  ( Transition(Transition) )
import           NixManager.HMServicesUtil      ( writePendingServicesFile )
import           NixManager.ManagerState        ( ManagerState )
import           NixManager.ManagerEvent        ( ManagerEvent
                                                , pureTransition
                                                )

updateEvent :: ManagerState -> Event -> Transition ManagerState ManagerEvent
updateEvent s (EventEditView (EditView.EditViewSettingChanged setter)) =
  let newState =
          over (#hmServiceState . #_HomeManagerPresent . #expression) setter s
  in  Transition newState $ do
        writePendingServicesFile
          (newState ^?! #hmServiceState . #_HomeManagerPresent . #expression)
        pure Nothing
updateEvent s (EventEditView e) = pureTransition
  (s & #hmServiceState . #_HomeManagerPresent %~ EditView.updateEvent e)
updateEvent s EventReload       = pureTransition s
updateEvent s (EventReloaded _) = pureTransition s
