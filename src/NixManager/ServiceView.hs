{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.ServiceView
  ( servicesBox
  )
where

import           NixManager.PackageView         ( packagesBox )
import           Data.Text                      ( intercalate )
import           NixManager.Nix
import           GI.Gtk.Declarative             ( bin
                                                , pane
                                                , paned
                                                , notebook
                                                , page
                                                , defaultPaneProperties
                                                , FromWidget
                                                , Bin
                                                , widget
                                                , Attribute((:=))
                                                , container
                                                , on
                                                )
import           GI.Gtk.Declarative.App.Simple  ( AppView )
import           Data.Vector.Lens               ( toVectorOf )
import qualified GI.Gtk                        as Gtk
import           Control.Lens                   ( (^.)
                                                , to
                                                , folded
                                                )
import           NixManager.ManagerState
import           NixManager.ManagerEvent


buildServiceRow
  :: FromWidget (Bin Gtk.ListBoxRow) target => NixService -> target event
buildServiceRow svc = bin
  Gtk.ListBoxRow
  []
  (widget Gtk.Label [#label := (svc ^. serviceLoc . to (intercalate "."))])



serviceRows s = toVectorOf (msServiceCache . folded . to buildServiceRow) s

servicesLeftPane s =
  bin Gtk.ScrolledWindow [] (container Gtk.ListBox [] (serviceRows s))

servicesBox s = paned
  []
  (pane defaultPaneProperties (servicesLeftPane s))
  (pane defaultPaneProperties $ widget Gtk.Label [#label := "right"])
