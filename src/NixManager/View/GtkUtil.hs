{-|
  Description: Various GTK-related utilities

Various GTK-related utilities
  -}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.View.GtkUtil where

import           GI.Gtk.Declarative             ( bin
                                                , padding
                                                , defaultBoxChildProperties
                                                , on
                                                , expand
                                                , container
                                                , fill
                                                , widget
                                                , Attribute((:=))
                                                , classes
                                                , container
                                                , BoxChild(BoxChild)
                                                , BoxChildProperties
                                                , on
                                                )
import qualified GI.Gtk                        as Gtk


-- | Add some padding around a widget
paddedAround spacing =
  container Gtk.Box [#orientation := Gtk.OrientationVertical]
    . pure
    . BoxChild defaultBoxChildProperties { padding = spacing
                                         , expand  = True
                                         , fill    = True
                                         }
    . container Gtk.Box []
    . pure
    . BoxChild defaultBoxChildProperties { padding = spacing
                                         , expand  = True
                                         , fill    = True
                                         }

-- | A shortcut for a box child that has both the expand and fill flag
expandAndFill :: BoxChildProperties
expandAndFill = defaultBoxChildProperties { expand = True, fill = True }

-- | A shortcut for a box child that has the fill, but not the expand flag
fillNoExpand :: BoxChildProperties
fillNoExpand = defaultBoxChildProperties { expand = False, fill = True }

