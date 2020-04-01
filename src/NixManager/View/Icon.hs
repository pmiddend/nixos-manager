{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.View.Icon
  ( icon
  )
where

import           GI.Gtk.Declarative.Attributes.Internal
                                                ( addSignalHandler )
import           Data.Vector                    ( Vector )
import           GI.Gtk.Declarative             ( Widget(Widget)
                                                , CustomWidget(CustomWidget)
                                                , customWidget
                                                , customCreate
                                                , Attribute
                                                , customPatch
                                                , customSubscribe
                                                , customAttributes
                                                , customParams
                                                , CustomPatch
                                                  ( CustomKeep
                                                  , CustomModify
                                                  )
                                                )
import qualified GI.Gtk                        as Gtk
import           NixManager.View.IconName       ( IconName
                                                , nameToGtk
                                                )

icon :: Vector (Attribute Gtk.Image e) -> IconName -> Widget e
icon customAttributes customParams = Widget
  (CustomWidget { customWidget
                , customCreate
                , customPatch
                , customSubscribe
                , customAttributes
                , customParams
                }
  )
 where
  customWidget = Gtk.Image
  customCreate :: IconName -> IO (Gtk.Image, ())
  customCreate iconName = do
    -- Taken from https://hackage.haskell.org/package/gi-gtk-3.0.32/docs/src/GI.Gtk.Enums.html#IconSize
    w <- Gtk.imageNewFromIconName (Just (nameToGtk iconName))
                                  (fromIntegral (fromEnum Gtk.IconSizeButton))
    pure (w, ())
  customSubscribe _params _currentImage widget cb =
    foldMap (addSignalHandler cb widget) customAttributes
  customPatch before after _internalState
    | before == after = CustomKeep
    | otherwise = CustomModify $ \w -> do
      Gtk.imageSetFromIconName w
                               (Just (nameToGtk after))
                               (fromIntegral (fromEnum Gtk.IconSizeButton))
      pure ()

