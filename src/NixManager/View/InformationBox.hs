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

-- | Display a box with an icon and a descriptive text next to it, possibly using Pango markup
informationBox
  :: FromWidget (Container Gtk.Box (Children BoxChild)) target
  => Bool -- ^ Whether to use pango markup
  -> IconName.IconName -- ^ Icon to display
  -> Text -- ^ Message to display
  -> target event
informationBox useMarkup iconName message = container
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
    , #useMarkup := useMarkup
    , #halign := Gtk.AlignCenter
    , classes ["nixos-manager-italic"]
    ]
  ]
