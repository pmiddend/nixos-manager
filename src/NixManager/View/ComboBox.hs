{-|
  Description: Declarative @ComboBoxText@ wrapper

Declarative @ComboBoxText@ wrapper.
  -}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module NixManager.View.ComboBox
  ( comboBox
  , ComboBoxChangeEvent(ComboBoxChangeEvent)
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

-- | The ComboBox properties
data ComboBoxProperties = ComboBoxProperties {
    _cbpValues :: [Text] -- ^ The possible values (note that, sadly, this isn't a list of pairs (T, Text) or something, just texts; patches welcome!)
  , _cbpActive :: Int -- ^ The active index in the combobox.
  } deriving(Eq)

makeLenses ''ComboBoxProperties

-- ^ Triggered when the combobox changes its value to a new index
newtype ComboBoxChangeEvent = ComboBoxChangeEvent Int

-- ^ Build a Combobox from Gtk attributes and some user-defined ones
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
    Gtk.comboBoxSetActive box (props ^. cbpActive . to fromIntegral)
    pure (box, ())
  customSubscribe _params _internalState widget cb = do
    h <-
      Gtk.on widget #changed
      $   cb
      .   ComboBoxChangeEvent
      .   fromIntegral
      =<< Gtk.comboBoxGetActive widget
    pure (fromCancellation (GI.signalHandlerDisconnect widget h))
  customPatch oldParams newParams _
    | oldParams == newParams = CustomKeep
    | otherwise = CustomModify $ \widget -> do
      when ((oldParams ^. cbpValues) /= (newParams ^. cbpValues)) $ do
        Gtk.comboBoxTextRemoveAll widget
        forM_ (newParams ^. cbpValues)
          $ Gtk.comboBoxTextInsert widget (-1) Nothing
      when ((oldParams ^. cbpActive) /= (newParams ^. cbpActive))
        $ void
            (Gtk.comboBoxSetActive widget
                                   (newParams ^. cbpActive . to fromIntegral)
            )

