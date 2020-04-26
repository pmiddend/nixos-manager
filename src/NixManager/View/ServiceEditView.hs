{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
  Description: Contains the “widget” for the two-paned “Edit services” tab
Contains the “widget” for the two-paned “Edit services” tab
  -}
module NixManager.View.ServiceEditView
  ( editView
  , EditViewEvent(..)
  , updateEvent
  )
where

import           Data.Validation                ( Validation(Success, Failure) )
import           NixManager.Constants           ( globalOptionsMagicString )
import           NixManager.View.GtkUtil        ( paddedAround
                                                , expandAndFill
                                                )
import           NixManager.NixLocation         ( NixLocation
                                                , flattenedTail
                                                , flattened
                                                , flattenLocation
                                                , firstComponent
                                                , isSingleton
                                                , locationComponents
                                                , locationDropComponents
                                                )
import           NixManager.Services.ServiceCategory
                                                ( ServiceCategory
                                                , serviceCategories
                                                , categoryToText
                                                , categoryToNixPrefix
                                                , serviceCategoryIdx
                                                )
import           Data.Maybe                     ( fromMaybe )
import           NixManager.Docbook             ( parseDocbook
                                                , docbookToPango
                                                )
import           Data.List                      ( elemIndex )
import           NixManager.NixExpr             ( NixExpr
                                                  ( NixBoolean
                                                  , NixString
                                                  , NixSymbol
                                                  )
                                                , prettyPrintSingleLine
                                                , parseNixString
                                                )
import           NixManager.View.ComboBox       ( comboBox
                                                , ComboBoxChangeEvent
                                                  ( ComboBoxChangeEvent
                                                  )
                                                , ComboBoxProperties
                                                  ( ComboBoxProperties
                                                  )
                                                )
import qualified Data.Vector                   as Vector
import           Data.Text                      ( isInfixOf
                                                , Text
                                                )
import           NixManager.Util                ( showText
                                                , predAnd
                                                , surroundSimple
                                                , Endo
                                                )
import           NixManager.Services.StateData  ( StateData )
import           NixManager.NixServiceOption    ( NixServiceOption )
import           GI.Gtk.Declarative.Widget      ( Widget )
import           GI.Gtk.Declarative.SingleWidget
                                                ( SingleWidget )
import           GI.Gtk.Declarative             ( bin
                                                , onM
                                                , on
                                                , pane
                                                , paned
                                                , classes
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
                                                , from
                                                , non
                                                , re
                                                , Traversal'
                                                , filtered
                                                , view
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
                                                , (&)
                                                , (.~)
                                                , (^?!)
                                                )
import           NixManager.NixService          ( NixService )
import           NixManager.NixServiceOptionType
                                                ( NixServiceOptionType
                                                  ( NixServiceOptionString
                                                  , NixServiceOptionPath
                                                  , NixServiceOptionSubmodule
                                                  , NixServiceOptionBoolean
                                                  , NixServiceOptionOneOfString
                                                  , NixServiceOptionPackage
                                                  )
                                                )
import           NixManager.ManagerState        ( ManagerState )

data EditViewEvent = EditViewSelected (Maybe Int)  -- ^ Triggered when the current service selection changes
                   | EditViewSearchChanged Text -- ^ Triggered when the search string changes
                   | EditViewSettingChanged (Endo NixExpr)  -- ^ Triggered whenever we change a service setting. Contains an endomorphism that changes the service Nix expression
                   | EditViewCategoryIdxChanged Int -- ^ Triggered when the service category combobox changes
                   | EditViewDiscard

updateEvent :: EditViewEvent -> Endo StateData
updateEvent (EditViewCategoryIdxChanged newCategory) sd =
  sd & #categoryIdx .~ newCategory & #selectedIdx .~ Nothing
updateEvent (EditViewSearchChanged t) sd =
  sd & #searchString .~ t & #selectedIdx .~ Nothing
updateEvent (EditViewSelected i) sd = sd & #selectedIdx .~ i
updateEvent _                    sd = sd

-- | Create a list box row widget from a service
buildServiceRow
  :: FromWidget (Bin Gtk.ListBoxRow) target => NixService -> target event
buildServiceRow svc =
  let markedUp = if isSingleton (svc ^. #serviceLoc)
        then surroundSimple "b" globalOptionsMagicString
        else svc ^. #serviceLoc . flattenedTail
  in  bin Gtk.ListBoxRow
          []
          (widget Gtk.Label [#label := markedUp, #useMarkup := True])

-- | Handles a row selection event
rowSelectionHandler :: Maybe Gtk.ListBoxRow -> Gtk.ListBox -> IO EditViewEvent
rowSelectionHandler (Just row) _ = do
  selectedIndex <- Gtk.listBoxRowGetIndex row
  if selectedIndex == -1
    then pure (EditViewSelected Nothing)
    else pure (EditViewSelected (Just (fromIntegral selectedIndex)))
rowSelectionHandler _ _ = pure (EditViewSelected Nothing)

-- | Check if a service matches a cateogry
categoryMatches :: ServiceCategory -> NixLocation -> Bool
categoryMatches c loc = categoryToNixPrefix c == firstComponent loc

-- | We need to filter some option categories, for example something like @service.<name>.bar@ or @service.*.bar@. At least until we can handle that, too.
filterPredicate :: StateData -> NixService -> Bool
filterPredicate sd =
  (         (((sd ^. #searchString) `isInfixOf`) . flattenLocation)
    `predAnd` ((not . ("<" `isInfixOf`)) . flattenLocation)
    `predAnd` ((not . ("*" `isInfixOf`)) . flattenLocation)
    `predAnd` categoryMatches (sd ^. #categoryIdx . from serviceCategoryIdx)
    )
    . view #serviceLoc


-- | The list of service rows (the left half of the tab, minus the search)
serviceRows :: StateData -> Vector.Vector (Bin Gtk.ListBoxRow event)
serviceRows sd = toVectorOf
  (#cache . folded . filtered (filterPredicate sd) . to buildServiceRow)
  sd

-- | The left half of the tab
servicesLeftPane sd _ =
  let searchField = widget
        Gtk.SearchEntry
        [ #placeholderText := "Search..."
        , #maxWidthChars := 50
        , onM #searchChanged ((EditViewSearchChanged <$>) . Gtk.getEntryText)
        , #halign := Gtk.AlignFill
        ]
      serviceList = container Gtk.ListBox
                              [onM #rowSelected rowSelectionHandler]
                              (serviceRows sd)
      changeCallback :: ComboBoxChangeEvent -> EditViewEvent
      changeCallback (ComboBoxChangeEvent i) = EditViewCategoryIdxChanged i
      categoryCombo = changeCallback <$> comboBox
        []
        (ComboBoxProperties (categoryToText <$> serviceCategories) 0)
  in  paddedAround 5 $ container
        Gtk.Box
        [#orientation := Gtk.OrientationVertical, #spacing := 3]
        [ BoxChild defaultBoxChildProperties $ container
          Gtk.Box
          [#orientation := Gtk.OrientationHorizontal, #spacing := 5]
          [ BoxChild defaultBoxChildProperties
            $ widget Gtk.Label [#label := "Category:"]
          , BoxChild expandAndFill categoryCombo
          ]
        , BoxChild defaultBoxChildProperties searchField
        , BoxChild expandAndFill $ bin Gtk.ScrolledWindow [] serviceList
        ]

-- | Given an option path, return a traversal for the corresponding attribute set element
optionLens' :: Text -> Traversal' NixExpr (Maybe NixExpr)
optionLens' optionPath =
  #_NixFunctionDecl . #functionExpr . #_NixSet . at optionPath

-- | Given the whole services Nix expression and a concrete service option, construct the edit widget for that option. This does some case analysis on the type, see 'NixManager.NixServiceOptionType'
buildOptionValueCell :: NixExpr -> NixServiceOption -> Widget EditViewEvent
buildOptionValueCell serviceExpression serviceOption =
  let
    optionPath :: Text
    optionPath = serviceOption ^. #optionLoc . flattened
    optionValue :: Maybe NixExpr
    optionValue = serviceExpression ^? optionLens' optionPath . folded
    rawChangeEvent :: Text -> EditViewEvent
    rawChangeEvent "" =
      EditViewSettingChanged (set (optionLens' optionPath) Nothing)
    rawChangeEvent v = case parseNixString v of
      Failure _ -> EditViewDiscard
      Success e ->
        EditViewSettingChanged (set (optionLens' optionPath) (Just e))
    changeEvent v =
      EditViewSettingChanged (set (optionLens' optionPath) (Just v))
    textLikeEntry inL outL = widget
      Gtk.Entry
      [ #text := (optionValue ^. pre (traversed . outL) . non "")
      , onM #changed ((changeEvent . inL <$>) . Gtk.entryGetText)
      ]
  in
    case serviceOption ^. #optionType of
      Failure e -> widget
        Gtk.Label
        [#label := ("Option value \"" <> e <> "\" not implemented yet")]
      Success NixServiceOptionBoolean ->
        let changeCallback :: Bool -> (Bool, EditViewEvent)
            changeCallback newValue =
                (False, changeEvent (NixBoolean newValue))
        in  widget
              Gtk.Switch
              [ #active
                := (optionValue ^. pre (traversed . #_NixBoolean) . non False)
              , on #stateSet changeCallback
              ]
      Success (NixServiceOptionOneOfString values) ->
        let activeIndex :: Maybe Int
            activeIndex =
                optionValue
                  ^? folded
                  .  #_NixString
                  .  to (`elemIndex` values)
                  .  folded
            changeCallback :: ComboBoxChangeEvent -> EditViewEvent
            changeCallback (ComboBoxChangeEvent 0) =
                EditViewSettingChanged $ set (optionLens' optionPath) Nothing
            changeCallback (ComboBoxChangeEvent idx) = EditViewSettingChanged
              (set (optionLens' optionPath)
                   (Just (values ^?! ix (fromIntegral idx) . re #_NixString))
              )
        in  changeCallback <$> comboBox
              []
              (ComboBoxProperties ("<no value>" : values)
                                  (fromMaybe 0 activeIndex)
              )
      Success NixServiceOptionPackage   -> textLikeEntry NixSymbol #_NixSymbol
      Success NixServiceOptionSubmodule -> textLikeEntry NixSymbol #_NixSymbol
      Success NixServiceOptionPath      -> textLikeEntry NixSymbol #_NixSymbol
      Success NixServiceOptionString    -> textLikeEntry NixString #_NixString
      Success v                         -> container
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
          [ #label := ("type: " <> showText v)
          , #wrap := True
          , classes ["option-type-description"]
          ]
        ]

-- | Convert the docbook documentation markup to GTK (pango) markup
convertMarkup :: Text -> Text
convertMarkup t = case parseDocbook t of
  Failure e -> "error parsing description: " <> e
  Success v -> docbookToPango v

-- | Build all the option rows for a selected service, given the whole services Nix expression
buildOptionRows :: NixExpr -> NixServiceOption -> BoxChild EditViewEvent
buildOptionRows serviceExpression serviceOption =
  let
    formatOptionName :: NixLocation -> Text
    formatOptionName loc = flattenLocation
      (locationDropComponents (if locationComponents loc == 2 then 1 else 2) loc
      )
    optionBox = container
      Gtk.Box
      [#orientation := Gtk.OrientationHorizontal, #spacing := 10, #margin := 15]
      [ BoxChild expandAndFill $ widget
        Gtk.Label
        [ classes ["service-option-title"]
        , #label := (serviceOption ^. #optionLoc . to formatOptionName)
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
        , #label := (serviceOption ^. #optionDescription . to convertMarkup)
        , #wrap := True
        , #useMarkup := True
        ]
      ]
  in
    BoxChild defaultBoxChildProperties rootBox

-- | The right half of the services tab
servicesRightPane
  :: ( FromWidget (SingleWidget Gtk.Label) target
     , FromWidget (Bin Gtk.ScrolledWindow) target
     )
  => StateData
  -> target EditViewEvent

servicesRightPane sd = case sd ^. #selectedIdx of
  Nothing ->
    widget Gtk.Label [#label := "Please select a service from the left pane"]
  Just idx ->
    let
      svc = (sd ^.. #cache . folded . filtered (filterPredicate sd)) ^?! ix idx
      svcLabel = svc ^. #serviceLoc . flattenedTail
      optBox =
        container Gtk.Box
                  [#orientation := Gtk.OrientationVertical, #spacing := 10]
          $ Vector.fromList
              ( BoxChild
                  defaultBoxChildProperties
                  (widget Gtk.Label
                          [classes ["service-headline"], #label := svcLabel]
                  )
              : (svc ^.. #serviceOptions . folded . to
                  (buildOptionRows (sd ^. #expression))
                )
              )
    in
      bin Gtk.ScrolledWindow [] optBox

editView :: StateData -> ManagerState -> Widget EditViewEvent
editView sd s = paned []
                      (pane defaultPaneProperties (servicesLeftPane sd s))
                      (pane defaultPaneProperties (servicesRightPane sd))
