{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.View.ProgressBar
  ( progressBar
  )
where

import           Control.Monad                  ( when )
import           Data.Vector                    ( Vector )
import           GI.Gtk.Declarative             ( Attribute )
import qualified GI.Gtk                        as Gtk
import           Data.Typeable                  ( eqT
                                                , (:~:)(Refl)
                                                )
import           GI.Gtk.Declarative.Patch       ( Patchable(create, patch)
                                                , Patch(Replace, Modify)
                                                )
import           GI.Gtk.Declarative.State       ( SomeState(SomeState)
                                                , stateTreeCustomState
                                                , stateTreeCollectedAttributes
                                                , StateTree(StateTreeWidget)
                                                , StateTreeNode(StateTreeNode)
                                                , stateTreeNode
                                                , stateTreeNodeWidget
                                                , stateTreeStyleContext
                                                )
import           GI.Gtk.Declarative.EventSource ( EventSource
                                                , subscribe
                                                , fromCancellation
                                                )
import           GI.Gtk.Declarative.Attributes.Collected
                                                ( collectAttributes
                                                , collectedClasses
                                                , collectedProperties
                                                , constructProperties
                                                , updateProperties
                                                , canBeModifiedTo
                                                , updateClasses
                                                )
import           GI.Gtk.Declarative.Widget      ( Widget(Widget) )

data ProgressBar e = ProgressBar (Vector (Attribute Gtk.ProgressBar e)) Int
  deriving(Functor)

instance EventSource ProgressBar where
  subscribe _ _ _ = pure (fromCancellation (pure ()))

instance Patchable ProgressBar where
  create (ProgressBar props _) = do
    let collected = collectAttributes props
    widget <- Gtk.new Gtk.ProgressBar (constructProperties collected)
    Gtk.progressBarPulse widget
    Gtk.widgetShow widget
    updateProperties widget mempty (collectedProperties collected)
    styleContext <- Gtk.widgetGetStyleContext widget
    pure
      (SomeState
        (StateTreeWidget (StateTreeNode widget styleContext collected ()))
      )
  patch (SomeState (stateTree :: StateTree st w e c cs)) (ProgressBar _ cold) new@(ProgressBar newAttrs cnew)
    = case eqT @w @Gtk.ProgressBar of
      Just Refl ->
        let
          oldCollected = stateTreeCollectedAttributes (stateTreeNode stateTree)
          newCollected = collectAttributes newAttrs
          oldCollectedProps = collectedProperties oldCollected
          newCollectedProps = collectedProperties newCollected
          canBeModified = oldCollectedProps `canBeModifiedTo` newCollectedProps
        in
          if not canBeModified
            then Replace (create new)
            else Modify $ do
              let widget' = stateTreeNodeWidget stateTree
              updateProperties widget' oldCollectedProps newCollectedProps
              updateClasses (stateTreeStyleContext (stateTreeNode stateTree))
                            (collectedClasses oldCollected)
                            (collectedClasses newCollected)
              let node = stateTreeNode stateTree
              casted <- Gtk.unsafeCastTo Gtk.ProgressBar widget'
              when (cold /= cnew) (Gtk.progressBarPulse casted)
              pure
                (SomeState
                  (StateTreeWidget node
                    { stateTreeCustomState         = ()
                    , stateTreeCollectedAttributes = newCollected
                    }
                  )
                )
      Nothing -> Replace (create new)

progressBar :: Vector (Attribute Gtk.ProgressBar e) -> Int -> Widget e
progressBar attrs counter = Widget (ProgressBar attrs counter)
