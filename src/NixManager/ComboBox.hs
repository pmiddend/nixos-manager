{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module NixManager.ComboBox
  ( comboBox
  , ComboBoxProperties(ComboBoxProperties)
  )
where

import           Data.Vector                    ( Vector )
import           GI.Gtk.Declarative.EventSource ( fromCancellation )
import qualified GI.GObject                    as GI
import           Control.Monad                  ( when
                                                , forM_
                                                , void
                                                )
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
import           Data.Text                      ( Text )
import qualified GI.Gtk                        as Gtk
import           Control.Lens                   ( makeLenses
                                                , (^.)
                                                , to
                                                )
import           Data.Int                       ( Int32 )

data ComboBoxProperties = ComboBoxProperties {
    _cbpValues :: [Text]
  , _cbpActive :: Maybe Int32
  } deriving(Eq)

makeLenses ''ComboBoxProperties

fromActive :: Int32 -> Maybe Int32
fromActive x | x < 0     = Nothing
             | otherwise = Just x

toActive :: Maybe Int32 -> Int32
toActive Nothing  = -1
toActive (Just x) = x

newtype ComboBoxChangeEvent = ComboBoxChangeEvent (Maybe Int32)

comboBox
  :: Vector (Attribute Gtk.ComboBoxText ComboBoxChangeEvent)
  -> ComboBoxProperties
  -> Widget ComboBoxChangeEvent
comboBox customAttributes customParams = Widget
  (CustomWidget { customWidget
                , customCreate
                , customPatch
                , customSubscribe
                , customAttributes
                , customParams
                }
  )
 where
  customWidget = Gtk.ComboBoxText
  customCreate :: ComboBoxProperties -> IO (Gtk.ComboBoxText, ())
  customCreate props = do
    box <- Gtk.new Gtk.ComboBoxText []
    forM_ (props ^. cbpValues) $ Gtk.comboBoxTextInsert box (-1) Nothing
    pure (box, ())
  customSubscribe _params _ widget cb = do
    h <-
      Gtk.on widget #changed
      $   cb
      .   ComboBoxChangeEvent
      .   fromActive
      =<< Gtk.comboBoxGetActive widget
    pure (fromCancellation (GI.signalHandlerDisconnect widget h))
  customPatch oldParams newParams _
    | oldParams == newParams = CustomKeep
    | otherwise = CustomModify $ \widget -> do
      when ((oldParams ^. cbpValues) /= (newParams ^. cbpValues)) $ do
        Gtk.comboBoxTextRemoveAll widget
        forM_ (newParams ^. cbpValues)
          $ Gtk.comboBoxTextInsert widget (-1) Nothing
      when ((oldParams ^. cbpActive) /= (newParams ^. cbpActive)) $ void
        (Gtk.comboBoxSetActive widget (newParams ^. cbpActive . to toActive))

