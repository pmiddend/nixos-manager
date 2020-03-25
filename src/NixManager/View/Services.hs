{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.View.Services
  ( servicesBox
  )
where

import           NixManager.View.ProgressBar    ( progressBar )
import           NixManager.View.GtkUtil        ( paddedAround )
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
                                                , MaybeError(Error, Success)
                                                )
import           NixManager.ServiceStateData    ( ServiceStateData
                                                , ssdServiceCache
                                                , ssdSelectedServiceIdx
                                                , ssdServiceExpression
                                                )
import           NixManager.NixServiceOption    ( optionType
                                                , NixServiceOption
                                                , optionLoc
                                                , optionDescription
                                                )
import           GI.Gtk.Declarative.Widget      ( Widget )
import           GI.Gtk.Declarative.Container.Class
                                                ( Children )
import           GI.Gtk.Declarative.Container   ( Container )
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
import           NixManager.ServicesEvent       ( ServicesEvent
                                                  ( ServicesEventSelected
                                                  , ServicesEventSettingChanged
                                                  , ServicesEventDownloadStart
                                                  , ServicesEventStateReload
                                                  , ServicesEventDownloadCancel
                                                  )
                                                )
import           NixManager.ServiceState        ( ServiceState
                                                  ( ServiceStateInvalidOptions
                                                  , ServiceStateInvalidExpr
                                                  , ServiceStateDone
                                                  , ServiceStateDownloading
                                                  )
                                                , ssddCounter
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
          [#label := (svc ^. serviceLoc . to tail . to (intercalate "."))]
  )

rowSelectionHandler :: Maybe Gtk.ListBoxRow -> Gtk.ListBox -> IO ManagerEvent
rowSelectionHandler (Just row) _ = do
  selectedIndex <- Gtk.listBoxRowGetIndex row
  if selectedIndex == -1
    then pure (ManagerEventServices (ServicesEventSelected Nothing))
    else pure
      (ManagerEventServices
        (ServicesEventSelected (Just (fromIntegral selectedIndex)))
      )
rowSelectionHandler _ _ =
  pure (ManagerEventServices (ServicesEventSelected Nothing))

serviceRows :: ServiceStateData -> Vector.Vector (Bin Gtk.ListBoxRow event)
serviceRows = toVectorOf (ssdServiceCache . folded . to buildServiceRow)

servicesLeftPane
  :: FromWidget (Bin Gtk.ScrolledWindow) target
  => ServiceStateData
  -> ManagerState
  -> target ManagerEvent
servicesLeftPane sd _ = bin
  Gtk.ScrolledWindow
  []
  (container Gtk.ListBox [onM #rowSelected rowSelectionHandler] (serviceRows sd)
  )

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
    rawChangeEvent "" = ManagerEventServices
      (ServicesEventSettingChanged (set (optionLens' optionPath) Nothing))
    rawChangeEvent v = case parseNixString v of
      Error   _ -> ManagerEventDiscard
      Success e -> ManagerEventServices
        (ServicesEventSettingChanged (set (optionLens' optionPath) (Just e)))
    changeEvent v = ManagerEventServices
      (ServicesEventSettingChanged (set (optionLens' optionPath) (Just v)))
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
            (ServicesEventSettingChanged $ set (optionLens' optionPath) Nothing)
          changeCallback (ComboBoxChangeEvent (Just idx)) =
            ManagerEventServices
              (ServicesEventSettingChanged
                (set (optionLens' optionPath)
                     (Just (values ^?! ix (fromIntegral idx) . re _NixString))
                )
              )
        in
          changeCallback <$> comboBox [] (ComboBoxProperties values activeIndex)
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
  => ServiceStateData
  -> NixManager.ManagerState.ManagerState
  -> target ManagerEvent

servicesRightPane sd _ = case sd ^. ssdSelectedServiceIdx of
  Nothing ->
    widget Gtk.Label [#label := "Please select a service from the left pane"]
  Just idx ->
    let
      svc      = sd ^?! ssdServiceCache . ix idx
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
                  (buildOptionRows (sd ^. ssdServiceExpression))
                )
              )
    in
      bin Gtk.ScrolledWindow [] optBox

--servicesBox :: ManagerState -> Widget ManagerEvent
-- This extra container is there to circumvent a bug that switches to the next page when one page is replaced.
servicesBox s = container
  Gtk.Box
  []
  [ BoxChild (defaultBoxChildProperties { expand = True, fill = True })
             (servicesBox' (s ^. msServiceState) s)
  ]

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

servicesBox' (ServiceStateDownloading ssdd) _ = container
  Gtk.Box
  [#orientation := Gtk.OrientationVertical, #spacing := 10]
  [ BoxChild defaultBoxChildProperties
             (widget Gtk.Label [#label := "Downloading services..."])
  , BoxChild defaultBoxChildProperties (progressBar [] (ssdd ^. ssddCounter))
  , BoxChild
    defaultBoxChildProperties
    (container
      Gtk.Box
      [#orientation := Gtk.OrientationHorizontal, #halign := Gtk.AlignCenter]
      [ widget
          Gtk.Button
          [ #label := "Cancel"
          , on #clicked (ManagerEventServices ServicesEventDownloadCancel)
          ]
      ]
    )
  ]
servicesBox' (ServiceStateInvalidExpr e) _ = noticeBox
  (ManagerEventServices ServicesEventStateReload)
  "Reload service state"
  ("Your service expression file is not valid. Maybe you have edited it by hand and it's become corrupted?\nPlease fix the error and then press the button below. The error is:\n"
  <> e
  )
servicesBox' (ServiceStateInvalidOptions possibleError) _ = noticeBox
  (ManagerEventServices ServicesEventDownloadStart)
  "Start Download"
  (invalidOptionsMessage possibleError)
servicesBox' (ServiceStateDone sd) s = paned
  []
  (pane defaultPaneProperties (servicesLeftPane sd s))
  (pane defaultPaneProperties (servicesRightPane sd s))
