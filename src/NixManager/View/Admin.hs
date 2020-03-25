{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.View.Admin
  ( adminBox
  )
where

import           NixManager.Util                ( showText )
import           Data.Text.Encoding             ( decodeUtf8 )
import           NixManager.Process             ( poStdout
                                                , poStderr
                                                , poResult
                                                )
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
import           NixManager.ManagerEvent        ( ManagerEvent
                                                  ( ManagerEventAdmin
                                                  )
                                                )
import           NixManager.AdminEvent          ( AdminEvent(AdminEventRebuild)
                                                )
import           NixManager.ManagerState        ( ManagerState
                                                , msAdminState
                                                )
import           GI.Gtk.Declarative.Widget      ( Widget )
import           NixManager.AdminState          ( AdminState
                                                  ( AdminStateNothing
                                                  , AdminStateBuilding
                                                  )
                                                , absProcessOutput
                                                )
import           Control.Lens                   ( (^.)
                                                , to
                                                )
import           Data.Monoid                    ( getFirst )

adminBox :: ManagerState -> Widget ManagerEvent
adminBox s = container
  Gtk.Box
  []
  [ BoxChild (defaultBoxChildProperties { expand = True, fill = True })
             (adminBox' s (s ^. msAdminState))
  ]

baseUi po = container
  Gtk.Box
  [#orientation := Gtk.OrientationVertical]
  [ BoxChild defaultBoxChildProperties $ widget
    Gtk.Button
    [ #label := "Apply changes and rebuild"
    , on #clicked (ManagerEventAdmin AdminEventRebuild)
    ]
  , BoxChild defaultBoxChildProperties $ widget
    Gtk.Label
    [ #label
        := (  "Previous return value:"
           <> (po ^. poResult . to getFirst . to showText)
           )
    ]
  , BoxChild defaultBoxChildProperties
    $ widget Gtk.Label [#label := "Standard output:"]
  , BoxChild (defaultBoxChildProperties { expand = True, fill = True })
  $ bin Gtk.ScrolledWindow []
  $ widget Gtk.Label [#label := (po ^. poStdout . to decodeUtf8)]
  , BoxChild defaultBoxChildProperties
    $ widget Gtk.Label [#label := "Standard error:"]
  , BoxChild (defaultBoxChildProperties { expand = True, fill = True })
  $ bin Gtk.ScrolledWindow []
  $ widget Gtk.Label [#label := (po ^. poStderr . to decodeUtf8), #wrap := True]
  ]

adminBox' ms (AdminStateNothing  po) = baseUi po
adminBox' ms (AdminStateBuilding bs) = baseUi (bs ^. absProcessOutput)
