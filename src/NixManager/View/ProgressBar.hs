{-# LANGUAGE NamedFieldPuns #-}
module NixManager.View.ProgressBar
  ( progressBar
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


progressBar :: Vector (Attribute Gtk.ProgressBar e) -> Int -> Widget e
progressBar customAttributes customParams = Widget
  (CustomWidget { customWidget
                , customCreate
                , customPatch
                , customSubscribe
                , customAttributes
                , customParams
                }
  )
 where
  customWidget = Gtk.ProgressBar
  customCreate :: Int -> IO (Gtk.ProgressBar, ())
  customCreate _ = do
    widget <- Gtk.new Gtk.ProgressBar []
    Gtk.progressBarPulse widget
    pure (widget, ())
  customSubscribe _params _internalState widget cb =
    foldMap (addSignalHandler cb widget) customAttributes
  customPatch before after _internalState
    | before == after = CustomKeep
    | otherwise = CustomModify $ \widget -> do
      Gtk.progressBarPulse widget
      pure ()
