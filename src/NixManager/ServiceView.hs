{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.ServiceView
  ( servicesBox
  )
where

import           NixManager.ComboBox
import           Data.Fix                       ( cata
                                                , Fix(Fix)
                                                )
import qualified Data.Vector                   as Vector
import           NixManager.PackageView         ( packagesBox )
import           Data.Text                      ( intercalate
                                                , Text
                                                , replace
                                                )
import           NixManager.Nix
import           NixManager.Util
import           NixManager.NixServiceOption
import           GI.Gtk.Declarative             ( bin
                                                , onM
                                                , pane
                                                , paned
                                                , classes
                                                , notebook
                                                , fill
                                                , expand
                                                , page
                                                , defaultPaneProperties
                                                , BoxChild(BoxChild)
                                                , defaultBoxChildProperties
                                                , FromWidget
                                                , Bin
                                                , widget
                                                , Attribute((:=))
                                                , container
                                                , on
                                                )
import           GI.Gtk.Declarative.App.Simple  ( AppView )
import           Data.Vector.Lens               ( toVectorOf )
import qualified GI.Gtk                        as Gtk
import           Control.Lens                   ( (^.)
                                                , ix
                                                , to
                                                , folded
                                                , (^..)
                                                , (^?!)
                                                )
import           NixManager.ManagerState
import           NixManager.ManagerEvent


buildServiceRow
  :: FromWidget (Bin Gtk.ListBoxRow) target => NixService -> target event
buildServiceRow svc = bin
  Gtk.ListBoxRow
  []
  (widget Gtk.Label
          [#label := (svc ^. serviceLoc . to tail . to (intercalate "."))]
  )

rowSelectionHandler :: Maybe Gtk.ListBoxRow -> Gtk.ListBox -> IO ManagerEvent
rowSelectionHandler (Just row) _ = do
  selectedIndex <- Gtk.listBoxRowGetIndex row
  if selectedIndex == -1
    then pure (ManagerEventServiceSelected Nothing)
    else pure (ManagerEventServiceSelected (Just (fromIntegral selectedIndex)))
rowSelectionHandler _ _ = pure (ManagerEventServiceSelected Nothing)


serviceRows s = toVectorOf (msServiceCache . folded . to buildServiceRow) s

servicesLeftPane s = bin
  Gtk.ScrolledWindow
  []
  (container Gtk.ListBox [onM #rowSelected rowSelectionHandler] (serviceRows s))

isStringy :: NixServiceOptionType -> Bool
isStringy NixServiceOptionString    = True
isStringy NixServiceOptionPath      = True
isStringy NixServiceOptionSubmodule = True
isStringy NixServiceOptionPackage   = True
isStringy NixServiceOptionInteger   = True
isStringy (NixServiceOptionOr l r)  = isStringy l && isStringy r
isStringy _                         = False

buildOptionValueCell serviceOption = case serviceOption ^. optionType of
  Left e -> widget
    Gtk.Label
    [#label := ("Option value \"" <> e <> "\" not implemented yet")]
  Right NixServiceOptionBoolean     -> widget Gtk.Switch []
  Right NixServiceOptionUnspecified -> widget
    Gtk.Label
    [ classes ["unspecified-label"]
    , #label := "Type not specified, cannot edit."
    ]
  Right (NixServiceOptionOneOf values) ->
    const ManagerEventDiscard
      <$> comboBox [] (ComboBoxProperties values Nothing)
  Right v -> if isStringy v
    then widget Gtk.Entry []
    else widget
      Gtk.Label
      [#label := ("Option value \"" <> showText v <> "\" not implemented yet")]

convertMarkup :: Text -> Text
convertMarkup =
  replaceTag "filename" "tt"
    . replaceTag "literal" "tt"
    . replaceTag "command" "tt"
    . replaceTag "option"  "tt"
    . replaceTag "varname" "tt"
    . removeTag "refentrytitle"
    . removeTag "manvolnum"
    . removeTag "link"
    . replaceTag "citerefentry" "tt"

buildOptionRows serviceOption =
  let optionBox = container
        Gtk.Box
        [ #orientation := Gtk.OrientationHorizontal
        , #spacing := 10
        , #margin := 15
        ]
        [ BoxChild (defaultBoxChildProperties { expand = True, fill = True })
          $ widget
              Gtk.Label
              [ classes ["service-option-title"]
              , #label
                := (serviceOption ^. optionLoc . to (intercalate "." . drop 2))
              , #halign := Gtk.AlignStart
              ]
        , BoxChild defaultBoxChildProperties
          $ buildOptionValueCell serviceOption
        ]
      rootBox = container
        Gtk.Box
        [#orientation := Gtk.OrientationVertical, #spacing := 5]
        [ optionBox
        , widget
          Gtk.Label
          [ classes ["service-option-description"]
          , #label := (serviceOption ^. optionDescription . to convertMarkup)
          , #wrap := True
          , #useMarkup := True
          ]
        ]
  in  BoxChild defaultBoxChildProperties rootBox

servicesRightPane s = case s ^. msSelectedServiceIdx of
  Nothing ->
    widget Gtk.Label [#label := "Please select a service from the left pane"]
  Just idx ->
    let
      svc      = s ^?! msServiceCache . ix idx
      svcLabel = svc ^. serviceLoc . to (intercalate "." . tail)
      optBox =
        container Gtk.Box
                  [#orientation := Gtk.OrientationVertical, #spacing := 10]
          $ Vector.fromList
              ( BoxChild
                  defaultBoxChildProperties
                  (widget Gtk.Label
                          [classes ["service-headline"], #label := svcLabel]
                  )
              : (svc ^.. serviceOptions . folded . to buildOptionRows)
              )
    in
      bin Gtk.ScrolledWindow [] optBox

servicesBox s = paned []
                      (pane defaultPaneProperties (servicesLeftPane s))
                      (pane defaultPaneProperties (servicesRightPane s))
