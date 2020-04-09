{-|
  Description: An icon widget

An icon widget
  -}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.View.Icon
  ( icon
  , IconProps(IconProps)
  )
where

import           Control.Lens                   ( makeLenses
                                                , (^.)
                                                )
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

-- | An icon’s properties
data IconProps = IconProps {
    _ipIconSize :: Gtk.IconSize -- ^ The icon’s desired size
  , _ipIconName :: IconName -- ^ What icon to display
  } deriving(Eq)

makeLenses ''IconProps

-- | Create an icon widget
icon :: Vector (Attribute Gtk.Image e) -> IconProps -> Widget e
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
  customCreate :: IconProps -> IO (Gtk.Image, ())
  customCreate iconProps = do
    -- Taken from https://hackage.haskell.org/package/gi-gtk-3.0.32/docs/src/GI.Gtk.Enums.html#IconSize
    w <- Gtk.imageNewFromIconName
      (Just (nameToGtk (iconProps ^. ipIconName)))
      (fromIntegral (fromEnum (iconProps ^. ipIconSize)))
    pure (w, ())
  customSubscribe _params _currentImage widget cb =
    foldMap (addSignalHandler cb widget) customAttributes
  customPatch before after _internalState
    | before == after = CustomKeep
    | otherwise = CustomModify $ \w -> do
      Gtk.imageSetFromIconName w
                               (Just (nameToGtk (after ^. ipIconName)))
                               (fromIntegral (fromEnum (after ^. ipIconSize)))
      pure ()

