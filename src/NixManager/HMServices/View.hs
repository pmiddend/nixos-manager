{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
module NixManager.HMServices.View
  ( servicesBox
  )
where

import qualified NixManager.View.IconName      as IconName
import           NixManager.View.GtkUtil        ( expandAndFill )
import           GI.Gtk.Declarative             ( bin
                                                , BoxChild(BoxChild)
                                                , container
                                                )
import qualified GI.Gtk                        as Gtk
import           Control.Lens                   ( (^.) )
import           NixManager.HMServices.Event    ( Event
                                                  ( EventReload
                                                  , EventEditView
                                                  )
                                                )
import           NixManager.ManagerEvent        ( ManagerEvent
                                                  ( ManagerEventHMServices
                                                  )
                                                )
import           NixManager.Services.View       ( noticeBox )
import           NixManager.HMServices.State    ( State
                                                  ( NoHomeManager
                                                  , InvalidHomeManager
                                                  , HomeManagerPresent
                                                  )
                                                )
import           NixManager.View.ServiceEditView
                                                ( editView )
import           NixManager.ManagerState        ( msHMServiceState )

servicesBox' NoHomeManager _ = bin Gtk.ScrolledWindow [] $ noticeBox
  IconName.DialogError
  (ManagerEventHMServices EventReload)
  IconName.ViewRefresh
  "Reload"
  "You don't have home-manager installed, or it isn’t configured properly. INSERT MORE DOCS HERE."
servicesBox' (InvalidHomeManager errorMessage) _ =
  bin Gtk.ScrolledWindow [] $ noticeBox IconName.DialogError
                                        (ManagerEventHMServices EventReload)
                                        IconName.ViewRefresh
                                        "Reload"
                                        errorMessage
servicesBox' (HomeManagerPresent sd) s =
  ManagerEventHMServices . EventEditView <$> editView sd s

--servicesBox :: ManagerState -> Widget ManagerEvent
-- This extra container is there to circumvent a bug that switches to the next page when one page is replaced.
servicesBox s = container
  Gtk.Box
  []
  [BoxChild expandAndFill (servicesBox' (s ^. msHMServiceState) s)]
