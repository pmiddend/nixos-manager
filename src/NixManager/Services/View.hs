{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.Services.View
  ( servicesBox
  )
where

import           Data.Maybe                     ( fromMaybe )
import           NixManager.View.GtkUtil        ( expandAndFill )
import           NixManager.View.ProgressBar    ( progressBar )
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
import           Data.Text                      ( isInfixOf
                                                , Text
                                                )
import           NixManager.Util                ( showText
                                                , MaybeError(Error, Success)
                                                , predAnd
                                                )
import           NixManager.Services.StateData  ( StateData
                                                , sdCache
                                                , sdSelectedIdx
                                                , sdSearchString
                                                , sdExpression
                                                )
import           NixManager.NixServiceOption    ( optionType
                                                , NixServiceOption
                                                , optionLoc
                                                , flattenLocation
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
                                                , (^?!)
                                                )
import           NixManager.Services.Event      ( Event
                                                  ( EventSelected
                                                  , EventSettingChanged
                                                  , EventSearchChanged
                                                  , EventDownloadStart
                                                  , EventStateReload
                                                  , EventDownloadCancel
                                                  )
                                                )
import           NixManager.Services.State      ( State
                                                  ( StateInvalidOptions
                                                  , StateInvalidExpr
                                                  , StateDone
                                                  , StateDownloading
                                                  )
                                                , sddCounter
                                                )
import           NixManager.ManagerState        ( ManagerState
                                                , msServiceState
                                                )
import           NixManager.ManagerEvent        ( ManagerEvent
                                                  ( ManagerEventDiscard
                                                  , ManagerEventServices
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
                                                  , NixServiceOptionOneOfString
                                                  , NixServiceOptionPackage
                                                  )
                                                )


buildServiceRow
  :: FromWidget (Bin Gtk.ListBoxRow) target => NixService -> target event
buildServiceRow svc = bin
  Gtk.ListBoxRow
  []
  (widget Gtk.Label
          [#label := (svc ^. serviceLoc . to tail . to flattenLocation)]
  )

rowSelectionHandler :: Maybe Gtk.ListBoxRow -> Gtk.ListBox -> IO ManagerEvent
rowSelectionHandler (Just row) _ = do
  selectedIndex <- Gtk.listBoxRowGetIndex row
  if selectedIndex == -1
    then pure (ManagerEventServices (EventSelected Nothing))
    else
      pure
        (ManagerEventServices
          (EventSelected (Just (fromIntegral selectedIndex)))
        )
rowSelectionHandler _ _ = pure (ManagerEventServices (EventSelected Nothing))

filterPredicate :: StateData -> NixService -> Bool
filterPredicate sd =
  (((sd ^. sdSearchString) `isInfixOf`) `predAnd` (not . ("<" `isInfixOf`)))
    . flattenLocation
    . view serviceLoc


serviceRows :: StateData -> Vector.Vector (Bin Gtk.ListBoxRow event)
serviceRows sd = toVectorOf
  (sdCache . folded . filtered (filterPredicate sd) . to buildServiceRow)
  sd

servicesLeftPane sd _ =
  let searchField = widget
        Gtk.SearchEntry
        [ #placeholderText := "Search service..."
        , #maxWidthChars := 50
        , onM
          #searchChanged
          ((ManagerEventServices . EventSearchChanged <$>) . Gtk.getEntryText)
        , #halign := Gtk.AlignFill
        ]
      serviceList = container Gtk.ListBox
                              [onM #rowSelected rowSelectionHandler]
                              (serviceRows sd)
  in  container
        Gtk.Box
        [#orientation := Gtk.OrientationVertical]
        [ BoxChild defaultBoxChildProperties searchField
        , BoxChild expandAndFill $ bin Gtk.ScrolledWindow [] serviceList
        ]

optionLens' :: Text -> Traversal' NixExpr (Maybe NixExpr)
optionLens' optionPath = _NixFunctionDecl . nfExpr . _NixSet . at optionPath

buildOptionValueCell :: NixExpr -> NixServiceOption -> Widget ManagerEvent
buildOptionValueCell serviceExpression serviceOption =
  let
    optionPath :: Text
    optionPath = serviceOption ^. optionLoc . to flattenLocation
    optionValue :: Maybe NixExpr
    optionValue = serviceExpression ^? optionLens' optionPath . folded
    rawChangeEvent :: Text -> ManagerEvent
    rawChangeEvent "" = ManagerEventServices
      (EventSettingChanged (set (optionLens' optionPath) Nothing))
    rawChangeEvent v = case parseNixString v of
      Error   _ -> ManagerEventDiscard
      Success e -> ManagerEventServices
        (EventSettingChanged (set (optionLens' optionPath) (Just e)))
    changeEvent v = ManagerEventServices
      (EventSettingChanged (set (optionLens' optionPath) (Just v)))
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
      Right (NixServiceOptionOneOfString values) ->
        let
          activeIndex :: Maybe ComboBoxIndexType
          activeIndex =
            optionValue
              ^? folded
              .  _NixString
              .  to (`elemIndex` values)
              .  folded
              .  to fromIntegral
          changeCallback :: ComboBoxChangeEvent -> ManagerEvent
          changeCallback (ComboBoxChangeEvent Nothing) = ManagerEventServices
            (EventSettingChanged $ set (optionLens' optionPath) Nothing)
          changeCallback (ComboBoxChangeEvent (Just 0)) = ManagerEventServices
            (EventSettingChanged $ set (optionLens' optionPath) Nothing)
          changeCallback (ComboBoxChangeEvent (Just idx)) =
            ManagerEventServices
              (EventSettingChanged
                (set (optionLens' optionPath)
                     (Just (values ^?! ix (fromIntegral idx) . re _NixString))
                )
              )
        in
          changeCallback <$> comboBox
            []
            (ComboBoxProperties ("<no value>" : values)
                                (Just (fromMaybe 0 activeIndex))
            )
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
          [ #label := ("type: " <> showText v)
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
        [ BoxChild expandAndFill $ widget
          Gtk.Label
          [ classes ["service-option-title"]
          , #label
            := (serviceOption ^. optionLoc . to (flattenLocation . drop 2))
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
  => StateData
  -> NixManager.ManagerState.ManagerState
  -> target ManagerEvent

servicesRightPane sd _ = case sd ^. sdSelectedIdx of
  Nothing ->
    widget Gtk.Label [#label := "Please select a service from the left pane"]
  Just idx ->
    let
      svc =
        (sd ^.. sdCache . folded . filtered (filterPredicate sd)) ^?! ix idx
      svcLabel = svc ^. serviceLoc . to (flattenLocation . tail)
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
                  (buildOptionRows (sd ^. sdExpression))
                )
              )
    in
      bin Gtk.ScrolledWindow [] optBox

--servicesBox :: ManagerState -> Widget ManagerEvent
-- This extra container is there to circumvent a bug that switches to the next page when one page is replaced.
servicesBox s = container
  Gtk.Box
  []
  [BoxChild expandAndFill (servicesBox' (s ^. msServiceState) s)]

invalidOptionsMessage :: Maybe Text -> Text
invalidOptionsMessage (Just e) =
  "Service definition file is invalid, possibly because of a corrupt download. You should try again. The error is:\n\n"
    <> e
invalidOptionsMessage Nothing =
  "Service definitions need to be downloaded first.\nPress the button below to start the download. It'll only take a few seconds, depending on your internet speed."

noticeBox buttonEvent buttonText message = container
  Gtk.Box
  [#orientation := Gtk.OrientationVertical, #spacing := 10]
  [ BoxChild defaultBoxChildProperties
             (widget Gtk.Label [#label := message, #wrap := True])
  , BoxChild
    defaultBoxChildProperties
    (container
      Gtk.Box
      [#orientation := Gtk.OrientationHorizontal, #halign := Gtk.AlignCenter]
      [widget Gtk.Button [#label := buttonText, on #clicked buttonEvent]]
    )
  ]

servicesBox' (StateDownloading ssdd) _ = container
  Gtk.Box
  [#orientation := Gtk.OrientationVertical, #spacing := 10]
  [ BoxChild defaultBoxChildProperties
             (widget Gtk.Label [#label := "Downloading services..."])
  , BoxChild defaultBoxChildProperties (progressBar [] (ssdd ^. sddCounter))
  , BoxChild
    defaultBoxChildProperties
    (container
      Gtk.Box
      [#orientation := Gtk.OrientationHorizontal, #halign := Gtk.AlignCenter]
      [ widget
          Gtk.Button
          [ #label := "Cancel"
          , on #clicked (ManagerEventServices EventDownloadCancel)
          ]
      ]
    )
  ]
servicesBox' (StateInvalidExpr e) _ = noticeBox
  (ManagerEventServices EventStateReload)
  "Reload service state"
  ("Your service expression file is not valid. Maybe you have edited it by hand and it's become corrupted?\nPlease fix the error and then press the button below. The error is:\n"
  <> e
  )
servicesBox' (StateInvalidOptions possibleError) _ = noticeBox
  (ManagerEventServices EventDownloadStart)
  "Start Download"
  (invalidOptionsMessage possibleError)
servicesBox' (StateDone sd) s = paned
  []
  (pane defaultPaneProperties (servicesLeftPane sd s))
  (pane defaultPaneProperties (servicesRightPane sd s))
