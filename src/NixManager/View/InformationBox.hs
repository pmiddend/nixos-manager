{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.View.InformationBox
  ( informationBox
  )
where

import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative.Container   ( Children )
import           GI.Gtk.Declarative             ( container
                                                , widget
                                                , classes
                                                , BoxChild(BoxChild)
                                                , Attribute((:=))
                                                , FromWidget
                                                , Container
                                                )
import qualified NixManager.View.IconName      as IconName
import           NixManager.View.Icon           ( icon
                                                , IconProps(IconProps)
                                                )
import           Data.Default                   ( def )
import Data.Text(Text)

informationBox
  :: FromWidget (Container Gtk.Box (Children BoxChild)) target
  => IconName.IconName
  -> Text
  -> target event
informationBox iconName message = container
  Gtk.Box
  [ #orientation := Gtk.OrientationHorizontal
  , #spacing := 15
  , #halign := Gtk.AlignCenter
  ]
  [ BoxChild def $ icon [] (IconProps Gtk.IconSizeDialog iconName)
  , BoxChild def $ widget
    Gtk.Label
    [ #label := message
    , #wrap := True
    , #halign := Gtk.AlignCenter
    , classes ["nixos-manager-italic"]
    ]
  ]
