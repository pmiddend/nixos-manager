{-|
  Description: The root of the view hierarchy

The root of the view hierarchy
  -}
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
                                                , pageWithTab
                                                , BoxChild(BoxChild)
                                                , widget
                                                )
import           Data.Default                   ( def )
import           NixManager.View.Icon           ( icon
                                                , IconProps(IconProps)
                                                )
import qualified NixManager.View.IconName      as IconName
import           GI.Gtk.Declarative.App.Simple  ( AppView )
import qualified GI.Gtk                        as Gtk
import           NixManager.ManagerState        ( ManagerState )
import           NixManager.ManagerEvent        ( ManagerEvent
                                                  ( ManagerEventClosed
                                                  )
                                                )
import           Data.Vector                    ( Vector )

-- | The main window’s attributes
windowAttributes :: Vector (Attribute Gtk.Window ManagerEvent)
windowAttributes =
  [ #title := "nixos-manager 1.0"
  , on #deleteEvent (const (True, ManagerEventClosed))
  , #widthRequest := 1024
  , #heightRequest := 768
  ]

-- | A label with an image next to it (used in the notebook’s tab headers)
imagedLabel iconProps text = container
  Gtk.Box
  [#orientation := Gtk.OrientationHorizontal, #spacing := 5]
  [ BoxChild def (icon [] iconProps)
  , BoxChild def (widget Gtk.Label [#label := text, #valign := Gtk.AlignCenter])
  ]

-- | The root view function
view' :: ManagerState -> AppView Gtk.Window ManagerEvent
view' s =
  let windowContents = notebook
        []
        [ pageWithTab
          (imagedLabel
            (IconProps Gtk.IconSizeButton IconName.ApplicationsSystem)
            "Administration"
          )
          (AdminView.adminBox s)
        , pageWithTab
          (imagedLabel (IconProps Gtk.IconSizeButton IconName.PackageXGeneric)
                       "Add/Remove Software"
          )
          (PackagesView.packagesBox s)
        , pageWithTab
          (imagedLabel
            (IconProps Gtk.IconSizeButton IconName.PreferencesOther)
            "Configure your system"
          )
          (ServicesView.servicesBox s)
        ]
  in  bin Gtk.Window windowAttributes windowContents
