{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.View where

import           Data.Semigroup                 ( Any(Any)
                                                , getAny
                                                )
import           Data.Maybe                     ( isJust )
import           Prelude                 hiding ( length )
import           Data.Text                      ( length
                                                , Text
                                                , toLower
                                                , isInfixOf
                                                )
import           NixManager.Nix
import           GI.Gtk.Declarative             ( bin
                                                , Widget
                                                , padding
                                                , defaultBoxChildProperties
                                                , expand
                                                , fill
                                                , FromWidget
                                                , Bin
                                                , widget
                                                , Attribute((:=))
                                                , classes
                                                , container
                                                , BoxChild(BoxChild)
                                                , on
                                                , onM
                                                )
import           GI.Gtk.Declarative.App.Simple  ( AppView )
import           Data.Vector.Lens               ( toVectorOf )
import qualified GI.Gtk                        as Gtk
import           Control.Lens                   ( (^.)
                                                , to
                                                , has
                                                , folded
                                                )
import           NixManager.ManagerState
import           NixManager.ManagerEvent
import           NixManager.Util
import           NixManager.Message

buildResultRow
  :: FromWidget (Bin Gtk.ListBoxRow) target => NixPackage -> target event
buildResultRow pkg = bin
  Gtk.ListBoxRow
  [classes (mwhen (pkg ^. npInstalled) ["package-row-installed"])]
  (widget Gtk.Label [#label := (pkg ^. npName)])

processSearchChange w = ManagerEventSearchChanged <$> Gtk.getEntryText w

searchLabel :: Widget event
searchLabel =
  widget Gtk.Label [#label := "Search in packages:", #halign := Gtk.AlignEnd]

searchField :: Widget ManagerEvent
searchField = widget
  Gtk.SearchEntry
  [ #placeholderText := "Enter a package name or part of a name..."
  , #maxWidthChars := 50
  , onM #searchChanged processSearchChange
  , #halign := Gtk.AlignFill
  ]

searchBox :: Widget ManagerEvent
searchBox = container
  Gtk.Box
  [#orientation := Gtk.OrientationHorizontal, #spacing := 10]
  [ BoxChild (defaultBoxChildProperties { expand = True, fill = True })
             searchLabel
  , BoxChild (defaultBoxChildProperties { expand = True, fill = True })
             searchField
  ]

rowSelectionHandler :: Maybe Gtk.ListBoxRow -> Gtk.ListBox -> IO ManagerEvent
rowSelectionHandler (Just row) _ = do
  selectedIndex <- Gtk.listBoxRowGetIndex row
  if selectedIndex == -1
    then pure (ManagerEventPackageSelected Nothing)
    else pure (ManagerEventPackageSelected (Just (fromIntegral selectedIndex)))
rowSelectionHandler _ _ = pure (ManagerEventPackageSelected Nothing)

windowAttributes =
  [ #title := "nix-manager 1.0"
  , on #deleteEvent (const (True, ManagerEventClosed))
  , #widthRequest := 1024
  , #heightRequest := 768
  ]

packageMatches :: Text -> NixPackage -> Bool
packageMatches t p = toLower t `isInfixOf` (p ^. npName . to toLower)


view' :: ManagerState -> AppView Gtk.Window ManagerEvent
view' s =
  let
    searchValid     = (s ^. msSearchString . to length) >= 2
    resultRows      = toVectorOf (msSearchResult . folded . to buildResultRow) s
    packageSelected = isJust (s ^. msSelectedPackage)
    currentPackageInstalled =
      getAny (s ^. msSelectedPackage . folded . npInstalled . to Any)
    packageButtonRow = container
      Gtk.Box
      [#orientation := Gtk.OrientationHorizontal, #spacing := 10]
      [ BoxChild
        (defaultBoxChildProperties { expand = True, fill = True })
        (widget
          Gtk.Button
          [ #label := "Try without installing"
          , #sensitive := (packageSelected && not currentPackageInstalled)
          , classes ["try-install-button"]
          , on #clicked ManagerEventTryInstall
          ]
        )
      , BoxChild
        (defaultBoxChildProperties { expand = True, fill = True })
        (widget
          Gtk.Button
          [ #label := "Install"
          , #sensitive := (packageSelected && not currentPackageInstalled)
          , classes ["install-button"]
          , on #clicked ManagerEventInstall
          ]
        )
      , BoxChild
        (defaultBoxChildProperties { expand = True, fill = True })
        (widget
          Gtk.Button
          [ #label := "Remove"
          , #sensitive := (packageSelected && currentPackageInstalled)
          , classes ["remove-button"]
          , on #clicked ManagerEventUninstall
          ]
        )
      ]
    resultBox = if searchValid
      then bin
        Gtk.ScrolledWindow
        []
        (container Gtk.ListBox [onM #rowSelected rowSelectionHandler] resultRows
        )
      else widget
        Gtk.Label
        [ #label := "Please enter a search term with at least two characters"
        , #expand := True
        ]
    windowBox = container
      Gtk.Box
      [#orientation := Gtk.OrientationVertical, #spacing := 10]
      (  [ BoxChild (defaultBoxChildProperties { padding = 5 }) searchBox
         , widget Gtk.HSeparator []
         ]
      <> foldMap
           (\e ->
             [ BoxChild
                 defaultBoxChildProperties
                 (widget
                   Gtk.Label
                   [ #label := (e ^. messageText)
                   , classes
                     [ if has (messageType . _ErrorMessage) e
                         then "error-message"
                         else "info-message"
                     ]
                   ]
                 )
             ]
           )
           (s ^. msLatestMessage)

      <> [ packageButtonRow
         , widget Gtk.HSeparator []
         , BoxChild
           (defaultBoxChildProperties { expand = True, fill = True })
           resultBox
         ]
      )
  in
    bin Gtk.Window windowAttributes windowBox
