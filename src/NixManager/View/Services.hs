{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.View.Services
  ( servicesBox
  )
where

import           NixManager.Docbook             ( parseDocbook
                                                , docbookToPango
                                                )
import           Data.List                      ( elemIndex )
import           NixManager.NixExpr             ( NixExpr
                                                  ( NixBoolean
                                                  , NixString
                                                  , NixSymbol
                                                  )
                                                , _NixFunctionDecl
                                                , nfExpr
                                                , prettyPrintSingleLine
                                                , _NixSet
                                                , parseNixString
                                                , _NixBoolean
                                                , _NixString
                                                , _NixSymbol
                                                )
import           NixManager.View.ComboBox       ( comboBox
                                                , ComboBoxIndexType
                                                , ComboBoxChangeEvent
                                                  ( ComboBoxChangeEvent
                                                  )
                                                , ComboBoxProperties
                                                  ( ComboBoxProperties
                                                  )
                                                )
import qualified Data.Vector                   as Vector
import           Data.Text                      ( intercalate
                                                , Text
                                                )
import           NixManager.Util                ( showText
                                                , replaceTag
                                                , removeTag
                                                , MaybeError(Error, Success)
                                                )
import           NixManager.NixServiceOption    ( optionType
                                                , NixServiceOption
                                                , optionLoc
                                                , optionDescription
                                                )
import           GI.Gtk.Declarative.Widget      ( Widget )
import           GI.Gtk.Declarative.SingleWidget
                                                ( SingleWidget )
import           GI.Gtk.Declarative             ( bin
                                                , onM
                                                , on
                                                , pane
                                                , paned
                                                , classes
                                                , fill
                                                , expand
                                                , defaultPaneProperties
                                                , BoxChild(BoxChild)
                                                , defaultBoxChildProperties
                                                , FromWidget
                                                , Bin
                                                , widget
                                                , Attribute((:=))
                                                , container
                                                )
import           Data.Vector.Lens               ( toVectorOf )
import qualified GI.Gtk                        as Gtk
import           Control.Lens                   ( (^.)
                                                , re
                                                , Traversal'
                                                , pre
                                                , traversed
                                                , set
                                                , ix
                                                , non
                                                , (^?)
                                                , to
                                                , folded
                                                , at
                                                , (^..)
                                                , (^?!)
                                                )
import           NixManager.ManagerState        ( msServiceCache
                                                , ManagerState
                                                , msServiceExpression
                                                , msSelectedServiceIdx
                                                )
import           NixManager.ManagerEvent        ( ManagerEvent
                                                  ( ManagerEventDiscard
                                                  , ManagerEventServiceSelected
                                                  , ManagerEventSettingChanged
                                                  )
                                                )
import           NixManager.NixService          ( NixService
                                                , serviceLoc
                                                , serviceOptions
                                                )
import           NixManager.NixServiceOptionType
                                                ( NixServiceOptionType
                                                  ( NixServiceOptionString
                                                  , NixServiceOptionPath
                                                  , NixServiceOptionSubmodule
                                                  , NixServiceOptionBoolean
                                                  , NixServiceOptionUnspecified
                                                  , NixServiceOptionOneOfString
                                                  , NixServiceOptionOneOfNumeric
                                                  , NixServiceOptionPackage
                                                  , NixServiceOptionInteger
                                                  , NixServiceOptionOr
                                                  )
                                                )


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

serviceRows :: ManagerState -> Vector.Vector (Bin Gtk.ListBoxRow event)
serviceRows = toVectorOf (msServiceCache . folded . to buildServiceRow)

servicesLeftPane
  :: FromWidget (Bin Gtk.ScrolledWindow) target
  => ManagerState
  -> target ManagerEvent
servicesLeftPane s = bin
  Gtk.ScrolledWindow
  []
  (container Gtk.ListBox [onM #rowSelected rowSelectionHandler] (serviceRows s))

optionLens' :: Text -> Traversal' NixExpr (Maybe NixExpr)
optionLens' optionPath = _NixFunctionDecl . nfExpr . _NixSet . at optionPath

buildOptionValueCell :: NixExpr -> NixServiceOption -> Widget ManagerEvent
buildOptionValueCell serviceExpression serviceOption =
  let
    optionPath :: Text
    optionPath = serviceOption ^. optionLoc . to (intercalate ".")
    optionValue :: Maybe NixExpr
    optionValue = serviceExpression ^? optionLens' optionPath . folded
    rawChangeEvent :: Text -> ManagerEvent
    rawChangeEvent "" =
      ManagerEventSettingChanged (set (optionLens' optionPath) Nothing)
    rawChangeEvent v = case parseNixString v of
      Error _ -> ManagerEventDiscard
      Success e ->
        ManagerEventSettingChanged (set (optionLens' optionPath) (Just e))
    changeEvent v =
      ManagerEventSettingChanged (set (optionLens' optionPath) (Just v))
    textLikeEntry inL outL = widget
      Gtk.Entry
      [ #text := (optionValue ^. pre (traversed . outL) . non "")
      , onM #changed ((changeEvent . inL <$>) . Gtk.entryGetText)
      ]
  in
    case serviceOption ^. optionType of
      Left e -> widget
        Gtk.Label
        [#label := ("Option value \"" <> e <> "\" not implemented yet")]
      Right NixServiceOptionBoolean ->
        let changeCallback :: Bool -> (Bool, ManagerEvent)
            changeCallback newValue =
                (False, changeEvent (NixBoolean newValue))
        in  widget
              Gtk.Switch
              [ #active
                := (optionValue ^. pre (traversed . _NixBoolean) . non False)
              , on #stateSet changeCallback
              ]
      -- Right NixServiceOptionUnspecified -> widget
      --   Gtk.Label
      --   [ classes ["unspecified-label"]
      --   , #label := "Type not specified, cannot edit."
      --   ]
      Right (NixServiceOptionOneOfString values) ->
        let activeIndex :: Maybe ComboBoxIndexType
            activeIndex =
                optionValue
                  ^? folded
                  .  _NixString
                  .  to (`elemIndex` values)
                  .  folded
                  .  to fromIntegral
            changeCallback :: ComboBoxChangeEvent -> ManagerEvent
            changeCallback (ComboBoxChangeEvent Nothing) =
                ManagerEventSettingChanged $ set (optionLens' optionPath) Nothing
            changeCallback (ComboBoxChangeEvent (Just idx)) =
                ManagerEventSettingChanged $ set
                  (optionLens' optionPath)
                  (Just (values ^?! ix (fromIntegral idx) . re _NixString))
        in  changeCallback
              <$> comboBox [] (ComboBoxProperties values activeIndex)
      Right NixServiceOptionPackage   -> textLikeEntry NixSymbol _NixSymbol
      Right NixServiceOptionSubmodule -> textLikeEntry NixSymbol _NixSymbol
      Right NixServiceOptionPath      -> textLikeEntry NixSymbol _NixSymbol
      Right NixServiceOptionString    -> textLikeEntry NixString _NixString
      Right v                         -> container
        Gtk.Box
        [#orientation := Gtk.OrientationVertical]
        [ widget
          Gtk.Entry
          [ #text
            := (optionValue ^. pre (traversed . to prettyPrintSingleLine) . non
                 ""
               )
          , onM #changed ((rawChangeEvent <$>) . Gtk.entryGetText)
          ]
        , widget
          Gtk.Label
          [ #label := ("type: " <> (showText v))
          , #wrap := True
          , classes ["option-type-description"]
          ]
        ]

convertMarkup :: Text -> Text
convertMarkup t = case parseDocbook t of
  Error   e -> "error parsing description: " <> e
  Success v -> docbookToPango v

buildOptionRows :: NixExpr -> NixServiceOption -> BoxChild ManagerEvent
buildOptionRows serviceExpression serviceOption =
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
          $ buildOptionValueCell serviceExpression serviceOption
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

servicesRightPane
  :: ( FromWidget (SingleWidget Gtk.Label) target
     , FromWidget (Bin Gtk.ScrolledWindow) target
     )
  => NixManager.ManagerState.ManagerState
  -> target ManagerEvent

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
              : (svc ^.. serviceOptions . folded . to
                  (buildOptionRows (s ^. msServiceExpression))
                )
              )
    in
      bin Gtk.ScrolledWindow [] optBox

servicesBox :: ManagerState -> Widget ManagerEvent
servicesBox s = paned []
                      (pane defaultPaneProperties (servicesLeftPane s))
                      (pane defaultPaneProperties (servicesRightPane s))
