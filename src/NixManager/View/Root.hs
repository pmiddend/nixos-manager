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
                                                , container
                                                , page
                                                , pageWithTab
                                                , BoxChild(BoxChild)
                                                , defaultBoxChildProperties
                                                , widget
                                                )
import           NixManager.View.Icon           ( icon )
import qualified NixManager.View.IconName      as IconName
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

imagedLabel iconName text = container
  Gtk.Box
  [#orientation := Gtk.OrientationHorizontal, #spacing := 5]
  [ BoxChild defaultBoxChildProperties (icon [] iconName)
  , BoxChild defaultBoxChildProperties
             (widget Gtk.Label [#label := text, #valign := Gtk.AlignCenter])
  ]

view' :: ManagerState -> AppView Gtk.Window ManagerEvent
view' s =
  let windowContents = notebook
        []
        [ pageWithTab (imagedLabel IconName.ApplicationsSystem "Admin")
                      (AdminView.adminBox s)
        , pageWithTab (imagedLabel IconName.PackageXGeneric "Packages")
                      (PackagesView.packagesBox s)
        , pageWithTab (imagedLabel IconName.PreferencesOther "Services")
                      (ServicesView.servicesBox s)
        ]
  in  bin Gtk.Window windowAttributes windowContents
