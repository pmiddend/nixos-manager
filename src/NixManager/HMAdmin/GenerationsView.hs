{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
  Description: Contains the "subwidget" for the home-manager generations view
Contains the "subwidget" for the home-manager generations view
  -}
module NixManager.HMAdmin.GenerationsView
  ( generationsView
  , updateEvent
  , Event(EventReload)
  )
where

import           GI.Gtk.Declarative.App.Simple  ( Transition(Transition) )
import           Data.Default                   ( def )
import           Data.Vector.Lens               ( toVectorOf )
import           GI.Gtk.Declarative             ( bin
                                                , container
                                                , onM
                                                , Attribute((:=))
                                                , widget
                                                , BoxChild(BoxChild)
                                                , on
                                                )
import           NixManager.View.GtkUtil        ( expandAndFill )
import qualified GI.Gtk                        as Gtk
import           Control.Lens                   ( has
                                                , (^?)
                                                , (&)
                                                , (?~)
                                                , (.~)
                                                , (^.)
                                                , traversed
                                                , to
                                                , set
                                                )
import           NixManager.Util                ( decodeUtf8
                                                , surroundSimple
                                                , TextualError
                                                )
import           NixManager.View.ImageButton    ( imageButton )
import qualified NixManager.View.IconName      as IconName
import           NixManager.Message             ( infoMessage
                                                , messageWidget
                                                , Message
                                                )
import           NixManager.HMAdmin.GenerationsData
                                                ( gdGenerations
                                                , gdMessage
                                                , gdSelectedGenerationIdx
                                                , gdSelectedGeneration
                                                )
import           NixManager.HMAdmin.GenerationsState
                                                ( GenerationsState
                                                  ( InvalidGenerationsState
                                                  , ValidGenerationsState
                                                  )
                                                , _ValidGenerationsState
                                                )
import           NixManager.HMGenerations       ( GenerationLine
                                                , readGenerations
                                                , glId
                                                , glDatePretty
                                                , activateGeneration
                                                , removeGeneration
                                                )
import           Data.Text                      ( Text )

-- | All events that the generations view needs
data Event = EventActivate -- ^ Is emitted when the user clicks on the activate button
           | EventRemove -- ^ Is emitted when the user clicks on the remove button
           | EventActivationFinished Text [GenerationLine] -- ^ Is emitted when a generation has been successfully activated (contains the new generations)
           | EventRemoveFinished Text [GenerationLine] -- ^ Is emitted when a generation has been successfully removed (contains the new generations)
           | EventDisplayMessage Message -- ^ Is emitted when a message needs to be displayed (for example about a successful activation)
           | EventGenerationsInvalid Text -- ^ Is emitted when the generations list couldn't be loaded and contains a sensible error message
           | EventSelectedChanged (Maybe Int) -- ^ Is emitted when the user selects a different generation
           | EventReload -- ^ Is emitted when we need to reload the generations list for some reason (for example, because we called @home-manager switch@ from the UI)
           | EventReloadFinished (TextualError [GenerationLine]) -- ^ Is emitted when the results of the aforementioned reload are in

-- | Given a generations line, construct the corresponding row in the UI
buildRow gl =
  let labelText =
          (  (surroundSimple "b" $ "Generation " <> (gl ^. glId . decodeUtf8))
          <> "\n"
          <> (surroundSimple "i" $ (gl ^. glDatePretty))
          )
      label        = widget Gtk.Label [#label := labelText, #useMarkup := True]
      rowContainer = container Gtk.Box
                               [#orientation := Gtk.OrientationHorizontal]
                               [BoxChild def label]
  in  bin Gtk.ListBoxRow [] rowContainer

-- | Handles a row selection event
rowSelectionHandler :: Maybe Gtk.ListBoxRow -> Gtk.ListBox -> IO Event
rowSelectionHandler (Just row) _ = Gtk.listBoxRowGetIndex row >>= \case
  -1 -> pure (EventSelectedChanged Nothing)
  selectedIndex ->
    pure (EventSelectedChanged (Just (fromIntegral selectedIndex)))
rowSelectionHandler _ _ = pure (EventSelectedChanged Nothing)

-- | Constructs the real generations view (after we made sure that we have valid data)
generationsValidView generationsData =
  let rows =
          toVectorOf (gdGenerations . traversed . to buildRow) generationsData
      listBox =
          container Gtk.ListBox [onM #rowSelected rowSelectionHandler] rows
      itemSelected   = has gdSelectedGeneration generationsData
      activateButton = imageButton
        [ #label := "Activate"
        , #alwaysShowImage := True
        , on #clicked EventActivate
        , #sensitive := itemSelected
        , #hexpand := True
        , #halign := Gtk.AlignFill
        ]
        IconName.EmblemDefault
      removeButton = imageButton
        [ #label := "Remove"
        , #alwaysShowImage := True
        , on #clicked EventRemove
        , #sensitive := itemSelected
        , #hexpand := True
        , #halign := Gtk.AlignFill
        ]
        IconName.EditDelete
      buttonRow = container
        Gtk.Box
        [#orientation := Gtk.OrientationHorizontal, #spacing := 5]
        [ BoxChild expandAndFill activateButton
        , BoxChild expandAndFill removeButton
        ]
      possibleMessage =
          maybe [] (pure . messageWidget) (generationsData ^. gdMessage)
  in  container Gtk.Box
                [#orientation := Gtk.OrientationVertical, #spacing := 5]
                ([buttonRow] <> possibleMessage <> [listBox])

-- | Constructs the generations view, which is possibly in an invalid state 
generationsView (InvalidGenerationsState message) =
  widget Gtk.Label [#label := ("Couldn't read generations data: " <> message)]
generationsView (ValidGenerationsState generationsData) =
  generationsValidView generationsData

-- | Logic for updating the state given an event
updateEvent :: GenerationsState -> Event -> Transition GenerationsState Event
updateEvent gd (EventSelectedChanged i) = Transition
  (set (_ValidGenerationsState . gdSelectedGenerationIdx) i gd)
  (pure Nothing)
updateEvent _ (EventGenerationsInvalid errorMessage) =
  Transition (InvalidGenerationsState errorMessage) (pure Nothing)
updateEvent gd (EventActivationFinished genId newGenerations) = Transition
  (set (_ValidGenerationsState . gdGenerations) newGenerations gd)
  (pure
    (Just
      (EventDisplayMessage
        (infoMessage ("Generation " <> genId <> " activated!"))
      )
    )
  )
updateEvent gd (EventRemoveFinished genId newGenerations) = Transition
  (  gd
  &  _ValidGenerationsState
  .  gdGenerations
  .~ newGenerations
  &  _ValidGenerationsState
  .  gdSelectedGenerationIdx
  .~ Nothing
  )
  (pure
    (Just
      (EventDisplayMessage (infoMessage ("Generation " <> genId <> " removed!"))
      )
    )
  )
updateEvent gd (EventDisplayMessage m) = Transition
  (  gd
  &  _ValidGenerationsState
  .  gdMessage
  ?~ m
  &  _ValidGenerationsState
  .  gdSelectedGenerationIdx
  .~ Nothing
  )
  (pure Nothing)
updateEvent gd EventActivate = Transition
  gd
  do
    case gd ^? _ValidGenerationsState . gdSelectedGeneration of
      Nothing      -> pure Nothing
      Just genLine -> do
        activateGeneration genLine
        readGenerations >>= \case
          Left  e              -> pure (Just (EventGenerationsInvalid e))
          Right newGenerations -> pure
            (Just
              (EventActivationFinished (genLine ^. glId . decodeUtf8)
                                       newGenerations
              )
            )
updateEvent gd EventRemove = Transition
  gd
  do
    case gd ^? _ValidGenerationsState . gdSelectedGeneration of
      Nothing      -> pure Nothing
      Just genLine -> do
        removeGeneration genLine
        readGenerations >>= \case
          Left e -> pure (Just (EventGenerationsInvalid e))
          Right newGenerations ->
            pure
              (Just
                (EventRemoveFinished (genLine ^. glId . decodeUtf8)
                                     newGenerations
                )
              )
updateEvent gd EventReload =
  Transition gd (pure . EventReloadFinished <$> readGenerations)
updateEvent gd (EventReloadFinished newGenerations') = case newGenerations' of
  Left  e              -> Transition (InvalidGenerationsState e) (pure Nothing)
  Right newGenerations -> Transition
    (  gd
    &  _ValidGenerationsState
    .  gdGenerations
    .~ newGenerations
    &  _ValidGenerationsState
    .  gdSelectedGenerationIdx
    .~ Nothing
    )
    (pure Nothing)
