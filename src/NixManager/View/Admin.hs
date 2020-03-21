{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.View.Admin
  ( adminBox
  )
where

import           GI.Gtk.Declarative             ( bin
                                                , onM
                                                , on
                                                , pane
                                                , paned
                                                , classes
                                                , fill
                                                , expand
                                                , defaultPaneProperties
                                                , BoxChild(BoxChild)
                                                , defaultBoxChildProperties
                                                , FromWidget
                                                , Bin
                                                , widget
                                                , Attribute((:=))
                                                , container
                                                )
import qualified GI.Gtk                        as Gtk
import           NixManager.ManagerEvent        ( ManagerEvent )
import           NixManager.ManagerState        ( ManagerState )
import           GI.Gtk.Declarative.Widget      ( Widget )


adminBox :: ManagerState -> Widget ManagerEvent
adminBox _ = container Gtk.Box [] []
