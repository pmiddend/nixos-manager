{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.View.Root
  ( view'
  )
where

import qualified NixManager.Packages.View      as PackagesView
import qualified NixManager.Services.View      as ServicesView
import qualified NixManager.Admin.View         as AdminView
import           GI.Gtk.Declarative             ( Attribute((:=))
                                                , on
                                                , bin
                                                , notebook
                                                , page
                                                )
import           GI.Gtk.Declarative.App.Simple  ( AppView )
import qualified GI.Gtk                        as Gtk
import           NixManager.ManagerState        ( ManagerState )
import           NixManager.ManagerEvent        ( ManagerEvent
                                                  ( ManagerEventClosed
                                                  )
                                                )
import           Data.Vector                    ( Vector )

windowAttributes :: Vector (Attribute Gtk.Window ManagerEvent)
windowAttributes =
  [ #title := "nixos-manager 1.0"
  , on #deleteEvent (const (True, ManagerEventClosed))
  , #widthRequest := 1024
  , #heightRequest := 768
  ]

view' :: ManagerState -> AppView Gtk.Window ManagerEvent
view' s =
  let windowContents = notebook
        []
        [ page "Admin"    (AdminView.adminBox s)
        , page "Packages" (PackagesView.packagesBox s)
        , page "Services" (ServicesView.servicesBox s)
        ]
  in  bin Gtk.Window windowAttributes windowContents
