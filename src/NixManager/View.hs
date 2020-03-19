{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.View
  ( view'
  )
where

import           NixManager.PackageView         ( packagesBox )
import           NixManager.ServiceView         ( servicesBox )
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
import           NixManager.ManagerState        ( ManagerState )
import           NixManager.ManagerEvent        ( ManagerEvent
                                                  ( ManagerEventClosed
                                                  )
                                                )

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
        [page "Packages" (packagesBox s), page "Services" (servicesBox s)]
  in  bin Gtk.Window windowAttributes windowContents
