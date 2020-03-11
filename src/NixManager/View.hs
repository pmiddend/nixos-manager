{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.View where

import           Prelude                 hiding ( length )
import           Data.Text                      ( pack
                                                , toLower
                                                , isInfixOf
                                                , length
                                                , Text
                                                , unpack
                                                )
import           NixManager.Nix
import           GI.Gtk.Declarative             ( bin
                                                , padding
                                                , defaultBoxChildProperties
                                                , expand
                                                , fill
                                                , FromWidget
                                                , Bin
                                                , widget
                                                , Attribute((:=))
                                                , container
                                                , BoxChild(BoxChild)
                                                , on
                                                , onM
                                                )
import           GI.Gtk.Declarative.App.Simple  ( AppView
                                                , App(App)
                                                , view
                                                , update
                                                , Transition(Transition, Exit)
                                                , inputs
                                                , initialState
                                                , run
                                                )
import           Data.Vector.Lens               ( toVectorOf )
import qualified GI.Gtk                        as Gtk
import           Control.Lens                   ( (^.)
                                                , to
                                                , (^..)
                                                , (&)
                                                , (.~)
                                                , folded
                                                , filtered
                                                )
import           NixManager.ManagerState
import           NixManager.ManagerEvent

packageMatches :: Text -> NixPackage -> Bool
packageMatches t p = toLower t `isInfixOf` (p ^. npName . to toLower)

buildResultRow
  :: FromWidget (Bin Gtk.ListBoxRow) target => NixPackage -> target event
buildResultRow pkg =
  bin Gtk.ListBoxRow [] (widget Gtk.Label [#label := (pkg ^. npName)])

view' :: ManagerState -> AppView Gtk.Window ManagerEvent
view' s =
  let
    searchLabel = widget
      Gtk.Label
      [#label := "Search in packages:", #halign := Gtk.AlignEnd]
    processSearchChange w = ManagerEventSearchChanged <$> Gtk.getEntryText w
    searchField = widget
      Gtk.SearchEntry
      [ #placeholderText := "Enter a package name or part of a name..."
      , #maxWidthChars := 50
      , onM #searchChanged processSearchChange
      , #halign := Gtk.AlignFill
      ]
    searchBox = container
      Gtk.Box
      [#orientation := Gtk.OrientationHorizontal, #spacing := 10]
      [ BoxChild (defaultBoxChildProperties { expand = True, fill = True })
                 searchLabel
      , BoxChild (defaultBoxChildProperties { expand = True, fill = True })
                 searchField
      ]
    searchValid = (s ^. msSearchString . to length) >= 2
    resultRows  = toVectorOf
      ( msPackageCache
      . folded
      . filtered (packageMatches (s ^. msSearchString))
      . to buildResultRow
      )
      s
    packageButtonRow = container
      Gtk.Box
      [#orientation := Gtk.OrientationHorizontal, #spacing := 10]
      [ BoxChild (defaultBoxChildProperties { expand = True, fill = True })
                 (widget Gtk.Button [#label := "Try without installing"])
      , BoxChild (defaultBoxChildProperties { expand = True, fill = True })
                 (widget Gtk.Button [#label := "Install"])
      , BoxChild (defaultBoxChildProperties { expand = True, fill = True })
                 (widget Gtk.Button [#label := "Remove"])
      ]
    resultBox = if searchValid
      then bin Gtk.ScrolledWindow [] (container Gtk.ListBox [] resultRows)
      else widget
        Gtk.Label
        [ #label := "Please enter a search term with at least two characters"
        , #expand := True
        ]
  in
    bin
        Gtk.Window
        [ #title := "nix-manager 1.0"
        , on #deleteEvent (const (True, ManagerEventClosed))
        , #widthRequest := 1024
        , #heightRequest := 768
        ]
      $ container
          Gtk.Box
          [#orientation := Gtk.OrientationVertical, #spacing := 10]
          [ BoxChild (defaultBoxChildProperties { padding = 5 }) searchBox
          , widget Gtk.HSeparator []
          , packageButtonRow
          , widget Gtk.HSeparator []
          , BoxChild
            (defaultBoxChildProperties { expand = True, fill = True })
            resultBox
          ]
