{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.View.Root
  ( view'
  )
where

import           NixManager.View.Packages       ( packagesBox )
import           NixManager.View.Services       ( servicesBox )
import           NixManager.View.Admin          ( adminBox )
import           NixManager.Nix                 ( )
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
  [ #title := "nix-manager 1.0"
  , on #deleteEvent (const (True, ManagerEventClosed))
  , #widthRequest := 1024
  , #heightRequest := 768
  ]

view' :: ManagerState -> AppView Gtk.Window ManagerEvent
view' s =
  let windowContents = notebook
        []
        [ page "Packages" (packagesBox s)
        , page "Services" (servicesBox s)
        , page "Admin"    (adminBox s)
        ]
  in  bin Gtk.Window windowAttributes windowContents
