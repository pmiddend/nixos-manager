{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.Admin.View
  ( adminBox
  )
where

import           NixManager.Util                ( showText )
import           System.Exit                    ( ExitCode(ExitSuccess) )
import qualified NixManager.View.IconName      as IconName
import           NixManager.View.Icon           ( icon
                                                , IconProps(IconProps)
                                                )
import           NixManager.View.GtkUtil        ( expandAndFill
                                                , fillNoExpand
                                                , paddedAround
                                                )
import           NixManager.View.ImageButton    ( imageButton )
import           NixManager.View.ProgressBar    ( progressBar )
import           Data.List                      ( elemIndex )
import           NixManager.View.ComboBox       ( comboBox
                                                , ComboBoxProperties
                                                  ( ComboBoxProperties
                                                  )
                                                , ComboBoxChangeEvent
                                                  ( ComboBoxChangeEvent
                                                  )
                                                )
import           Data.Text.Encoding             ( decodeUtf8 )
import           NixManager.Process             ( poStdout
                                                , poStderr
                                                , poResult
                                                )
import           GI.Gtk.Declarative             ( bin
                                                , on
                                                , classes
                                                , onM
                                                , fill
                                                , expand
                                                , BoxChild(BoxChild)
                                                , widget
                                                , Attribute((:=))
                                                , container
                                                )
import           GI.Gtk.Declarative.Container.Grid
                                                ( GridChild(GridChild)
                                                , GridChildProperties
                                                  ( width
                                                  , leftAttach
                                                  , topAttach
                                                  )
                                                )
import qualified GI.Gtk                        as Gtk
import           NixManager.ManagerEvent        ( ManagerEvent
                                                  ( ManagerEventAdmin
                                                  )
                                                )
import           NixManager.Changes             ( ChangeType(Changes) )
import           NixManager.Admin.Event         ( Event
                                                  ( EventRebuild
                                                  , EventUpdateChanged
                                                  , EventRebuildCancel
                                                  , EventRebuildModeChanged
                                                  , EventChangeDetails
                                                  )
                                                )
import           NixManager.ManagerState        ( ManagerState
                                                , msAdminState
                                                )
import           GI.Gtk.Declarative.Widget      ( Widget )
import           NixManager.Admin.State         ( asActiveRebuildMode
                                                , asProcessOutput
                                                , asChanges
                                                , absCounter
                                                , asBuildState
                                                , asDetailsState
                                                , asUpdate
                                                , detailsBool
                                                )
import           Control.Lens                   ( (^.)
                                                , has
                                                , _Nothing
                                                , view
                                                , from
                                                , folded
                                                , to
                                                , (^?!)
                                                , ix
                                                )
import           Data.Monoid                    ( getFirst )
import           NixManager.Admin.ValidRebuildTypes
                                                ( rebuildTypes
                                                , descriptionForRebuildType
                                                )
import           Data.Default                   ( def )

adminBox :: ManagerState -> Widget ManagerEvent
adminBox s = container Gtk.Box [] [BoxChild expandAndFill (adminBox' s)]

rebuildGrid as =
  let
    changes     = (as ^. asChanges) == Changes
    applyButton = imageButton
      [ #label
        := (if changes then "Apply Changes" else "No changes to apply (rebuild)"
           )
      , on #clicked (ManagerEventAdmin EventRebuild)
      , #valign := Gtk.AlignCenter
      , #alwaysShowImage := True
      ]
      IconName.ViewRefresh
    lastLine = maybe applyButton buildingBox (as ^. asBuildState)
    changeBuildType :: ComboBoxChangeEvent -> ManagerEvent
    changeBuildType (ComboBoxChangeEvent (Just idx)) = ManagerEventAdmin
      (EventRebuildModeChanged
        (rebuildTypes ^?! ix (fromIntegral idx) . to showText)
      )
    changeUpdate newValue =
      (False, ManagerEventAdmin (EventUpdateChanged newValue))
    gridProperties = [#rowSpacing := 5, #columnSpacing := 5]
    buildTypeLabel =
      widget Gtk.Label [#label := "Build type: ", #valign := Gtk.AlignCenter]
    buildTypeCombo = changeBuildType <$> comboBox
      [#valign := Gtk.AlignCenter]
      (ComboBoxProperties
        (showText <$> rebuildTypes)
        (fromIntegral <$> ((as ^. asActiveRebuildMode) `elemIndex` rebuildTypes)
        )
      )
    buildTypeDescription = inBox def $ widget
      Gtk.Label
      [ #label
        := (descriptionForRebuildType (as ^. asActiveRebuildMode) ^?! folded)
      , #wrap := True
      , #hexpand := True
      , #halign := Gtk.AlignFill
      , classes ["nixos-manager-italic"]
      ]
    buildTypeRow = 0
    updateRow    = 1
    updateLabel  = widget
      Gtk.Label
      [#label := "Download updates:", #valign := Gtk.AlignCenter]
    inBox props w = container Gtk.Box [] [BoxChild props w]
    updateSwitch = inBox def $ widget
      Gtk.Switch
      [ #active := (as ^. asUpdate)
      , on #stateSet changeUpdate
      , #valign := Gtk.AlignCenter
      , #vexpand := False
      ]
    updateDescription = inBox def $ widget
      Gtk.Label
      [ #label
        := "Whether to apply changes to the system (if any) and also update to the latest NixOS version"
      , #wrap := True
      , #hexpand := True
      , #halign := Gtk.AlignFill
      , classes ["nixos-manager-italic"]
      ]
    applyRow = 3
  in
    container
      Gtk.Grid
      gridProperties
      [ GridChild (def { leftAttach = 0, topAttach = buildTypeRow })
                  buildTypeLabel
      , GridChild (def { leftAttach = 1, topAttach = buildTypeRow })
                  buildTypeCombo
      , GridChild (def { leftAttach = 2, topAttach = buildTypeRow })
                  buildTypeDescription
      , GridChild (def { leftAttach = 0, topAttach = updateRow }) updateLabel
      , GridChild (def { leftAttach = 1, topAttach = updateRow }) updateSwitch
      , GridChild (def { leftAttach = 2, topAttach = updateRow })
                  updateDescription
      , GridChild (def { width = 3, topAttach = applyRow }) lastLine
      ]

rebuildBox as =
  let
    statusLabel ExitSuccess = widget
      Gtk.Label
      [ #label := "Build finished successfully!"
      , #useMarkup := True
      , classes ["info-message"]
      ]
    statusLabel _ = widget
      Gtk.Label
      [ #label
        := "Build failed! Please check the build details below to find out what's wrong."
      , #useMarkup := True
      , classes ["error-message"]
      ]
    processExpanded w = do
      expandedState <- Gtk.getExpanderExpanded w
      pure
        ( ManagerEventAdmin
        . EventChangeDetails
        . view (from detailsBool)
        . not
        $ expandedState
        )
    lastStatusRow =
      case
          ( has (asBuildState . _Nothing) as
          , as ^. asProcessOutput . poResult . to getFirst
          )
        of
          (True, Just v) -> [BoxChild def (paddedAround 5 (statusLabel v))]
          _              -> []
    rebuildDetails =
      [ BoxChild expandAndFill
          $ bin
              Gtk.Expander
              [ #label := "Build details"
              , #expanded := (as ^. asDetailsState . detailsBool)
              , onM #activate processExpanded
              ]
          $ container
              Gtk.Box
              [#orientation := Gtk.OrientationVertical]
              [ BoxChild fillNoExpand $ widget Gtk.Separator []
              , BoxChild def
                $ widget Gtk.Label [#label := "Build command standard output:"]
              , BoxChild expandAndFill
              $ bin Gtk.ScrolledWindow
                    [classes ["nixos-manager-grey-background"]]
              $ widget
                  Gtk.Label
                  [ #label := (as ^. asProcessOutput . poStdout . to decodeUtf8)
                  , classes ["nixos-manager-monospace"]
                  , #valign := Gtk.AlignStart
                  ]
              , BoxChild def
                $ widget Gtk.Label [#label := "Build command standard error:"]
              , BoxChild expandAndFill
              $ bin Gtk.ScrolledWindow
                    [classes ["nixos-manager-grey-background"]]
              $ widget
                  Gtk.Label
                  [ #label := (as ^. asProcessOutput . poStderr . to decodeUtf8)
                  , #wrap := True
                  , #valign := Gtk.AlignStart
                  , classes ["nixos-manager-monospace"]
                  ]
              ]
      ]
    frameMargin = 60
  in
    bin
      Gtk.Frame
      [ #label := "Applying changes"
      , #marginLeft := frameMargin
      , #marginRight := frameMargin
      ]
    $ paddedAround 10
    $ container
        Gtk.Box
        [#orientation := Gtk.OrientationVertical, #spacing := 5]
        ([BoxChild def (rebuildGrid as)] <> lastStatusRow <> rebuildDetails)

buildingBox buildState = container
  Gtk.Box
  [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  [ BoxChild fillNoExpand $ imageButton
    [ #label := "Cancel"
    , on #clicked (ManagerEventAdmin EventRebuildCancel)
    , #valign := Gtk.AlignCenter
    , #alwaysShowImage := True
    ]
    IconName.ProcessStop
  , BoxChild expandAndFill $ progressBar
    [#showText := True, #text := "Rebuilding..."]
    (buildState ^. absCounter)
  ]

adminBox' ms =
  let
    headlineItems =
      [ BoxChild fillNoExpand $ widget
        Gtk.Label
        [ #label := "Welcome to NixOS-Manager"
        , classes ["nixos-manager-headline"]
        ]
      , BoxChild def $ bin Gtk.Frame [#halign := Gtk.AlignCenter] $ container
        Gtk.Box
        [ #orientation := Gtk.OrientationHorizontal
        , #spacing := 10
        , #halign := Gtk.AlignCenter
        , classes ["info-message"]
        ]
        [ BoxChild def
          $ icon [] (IconProps Gtk.IconSizeDialog IconName.EmblemImportant)
        , BoxChild def $ widget
          Gtk.Label
          [ #label
            := "Select the “Packages” and “Services” tabs above to make changes to your system.\nOnce you're done with that, apply the changes using the form below."
          , #wrap := True
          , #halign := Gtk.AlignCenter
          , classes ["nixos-manager-italic"]
          ]
        ]
      ]
  in  container
          Gtk.Box
          [ #orientation := Gtk.OrientationVertical
          , #spacing := 3
          , #marginLeft := 5
          , #marginRight := 5
          , #marginTop := 5
          , #marginBottom := 5
          ]
        $  headlineItems
        <> [rebuildBox (ms ^. msAdminState)]
