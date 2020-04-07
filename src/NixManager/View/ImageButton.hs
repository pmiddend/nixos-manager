{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.View.ImageButton
  ( imageButton
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

imageButton :: Vector (Attribute Gtk.Button e) -> IconName -> Widget e
imageButton customAttributes customParams = Widget
  (CustomWidget { customWidget
                , customCreate
                , customPatch
                , customSubscribe
                , customAttributes
                , customParams
                }
  )
 where
  customWidget = Gtk.Button
  customCreate :: IconName -> IO (Gtk.Button, Gtk.Image)
  customCreate iconName = do
    w     <- Gtk.new Gtk.Button []
    -- Taken from https://hackage.haskell.org/package/gi-gtk-3.0.32/docs/src/GI.Gtk.Enums.html#IconSize
    image <- Gtk.imageNewFromIconName
      (Just (nameToGtk iconName))
      (fromIntegral (fromEnum Gtk.IconSizeButton))
    Gtk.buttonSetImage w (Just image)
    pure (w, image)
  customSubscribe _params _currentImage widget cb =
    foldMap (addSignalHandler cb widget) customAttributes
  customPatch before after currentImage
    | before == after = CustomKeep
    | otherwise = CustomModify $ \w -> do
      Gtk.widgetDestroy currentImage
      newImage <- Gtk.imageNewFromIconName
        (Just (nameToGtk after))
        (fromIntegral (fromEnum Gtk.IconSizeButton))
      Gtk.buttonSetImage w (Just newImage)
      pure newImage
