module NixManager.HMServices.Event
  ( Event(..)
  )
where

import           NixManager.HMServices.State    ( State )
import           NixManager.View.ServiceEditView
                                                ( EditViewEvent )

data Event = EventReload
           | EventReloaded State
           | EventEditView EditViewEvent
