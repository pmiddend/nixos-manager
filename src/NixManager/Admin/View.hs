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

import           NixManager.View.InformationBox      ( informationBox)
import           NixManager.NixRebuildMode      ( rebuildModeToText
                                                , rebuildModeIdx
                                                )
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
                                                  , EventGarbage
                                                  , EventGarbageCancel
                                                  , EventRebuildDoUpdateChanged
                                                  , EventGarbageChangeDetails
                                                  , EventGarbageOlderGenerationsChanged
                                                  , EventRebuildDoRollbackChanged
                                                  , EventRebuildCancel
                                                  , EventRebuildModeIdxChanged
                                                  , EventRebuildChangeDetails
                                                  )
                                                )
import           NixManager.ManagerState        ( ManagerState
                                                , msAdminState
                                                )
import           GI.Gtk.Declarative.Widget      ( Widget )
import           NixManager.Admin.State         ( asChanges
                                                , asRebuildData
                                                , asGarbageData
                                                )
import           NixManager.Admin.GarbageData   ( gdDetailsState
                                                , gdBuildState
                                                , gdProcessOutput
                                                , gdOlderGenerations
                                                )
import           Control.Lens                   ( (^.)
                                                , has
                                                , _Nothing
                                                , view
                                                , from
                                                , folded
                                                , to
                                                , (^?!)
                                                )
import           Data.Monoid                    ( getFirst )
import           NixManager.Admin.ValidRebuildModes
                                                ( validRebuildModes
                                                , descriptionForValidRebuildMode
                                                , validRebuildModeIdx
                                                )
import           Data.Default                   ( def )
import           NixManager.Admin.DetailsState  ( detailsBool )
import           NixManager.Admin.RebuildData   ( rdBuildState
                                                , rdActiveRebuildModeIdx
                                                , rdDoRollback
                                                , rdDoUpdate
                                                , rdDetailsState
                                                , rdProcessOutput
                                                )
import           NixManager.Admin.BuildState    ( bsCounter )

adminBox :: ManagerState -> Widget ManagerEvent
adminBox s = container Gtk.Box [] [BoxChild expandAndFill (adminBox' s)]

rebuildGrid as =
  let
    changes     = (as ^. asChanges) == Changes
    applyButton = imageButton
      [ #label
        := (if changes then "Apply Changes" else "Rebuild (no changes to apply)"
           )
      , on #clicked (ManagerEventAdmin EventRebuild)
      , #valign := Gtk.AlignCenter
      , #alwaysShowImage := True
      ]
      IconName.ViewRefresh
    lastLine =
      maybe applyButton (buildingBox EventRebuildCancel) (as ^. asRebuildData . rdBuildState)
    changeBuildType :: ComboBoxChangeEvent -> ManagerEvent
    changeBuildType (ComboBoxChangeEvent idx) =
      ManagerEventAdmin (EventRebuildModeIdxChanged idx)
    gridProperties = [#rowSpacing := 10, #columnSpacing := 10]
    buildTypeLabel =
      widget Gtk.Label [#label := "Build type: ", #valign := Gtk.AlignCenter]
    buildTypeCombo = changeBuildType <$> comboBox
      [#valign := Gtk.AlignCenter]
      (ComboBoxProperties (rebuildModeToText <$> validRebuildModes)
                          (as ^. asRebuildData . rdActiveRebuildModeIdx)
      )
    buildTypeDescription = inBox def $ widget
      Gtk.Label
      [ #label
        := (   descriptionForValidRebuildMode
               (  as
               ^. asRebuildData
               .  rdActiveRebuildModeIdx
               .  from validRebuildModeIdx
               )
           ^?! folded
           )
      , #wrap := True
      , #hexpand := True
      , #halign := Gtk.AlignFill
      , classes ["nixos-manager-italic"]
      ]
    buildTypeRow = 0
    inBox props w = container Gtk.Box [] [BoxChild props w]
    updateRow   = 1
    updateLabel = widget
      Gtk.Label
      [#label := "Download updates:", #valign := Gtk.AlignCenter]
    updateRadio = inBox def $ widget
      Gtk.Switch
      [ #active := (as ^. asRebuildData . rdDoUpdate)
      , on #stateSet
           (\b -> (False, ManagerEventAdmin (EventRebuildDoUpdateChanged b)))
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
    rollbackRow = 2
    rollbackLabel =
      widget Gtk.Label [#label := "Rollback:", #valign := Gtk.AlignCenter]
    rollbackRadio = inBox def $ widget
      Gtk.Switch
      [ #active := (as ^. asRebuildData . rdDoRollback)
      , on
        #stateSet
        (\b -> (False, ManagerEventAdmin (EventRebuildDoRollbackChanged b)))
      , #valign := Gtk.AlignCenter
      , #vexpand := False
      ]
    rollbackDescription = inBox def $ widget
      Gtk.Label
      [ #label := "Whether to rollback to the previous NixOS version"
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
      , GridChild (def { leftAttach = 1, topAttach = updateRow }) updateRadio
      , GridChild (def { leftAttach = 2, topAttach = updateRow })
                  updateDescription
      , GridChild (def { leftAttach = 0, topAttach = rollbackRow })
                  rollbackLabel
      , GridChild (def { leftAttach = 1, topAttach = rollbackRow })
                  rollbackRadio
      , GridChild (def { leftAttach = 2, topAttach = rollbackRow })
                  rollbackDescription
      , GridChild (def { width = 3, topAttach = applyRow }) lastLine
      ]

garbageGrid as =
  let
    applyButton = imageButton
      [ #label := "Collect Garbage"
      , on #clicked (ManagerEventAdmin EventGarbage)
      , #valign := Gtk.AlignCenter
      , #alwaysShowImage := True
      ]
      IconName.UserTrash
    lastLine =
      maybe applyButton (buildingBox EventGarbageCancel) (as ^. asGarbageData . gdBuildState)
    gridProperties = [#rowSpacing := 10, #columnSpacing := 10]
    inBox props w = container Gtk.Box [] [BoxChild props w]
    olderGenerationsRow   = 0
    olderGenerationsLabel = widget
      Gtk.Label
      [#label := "Remove old generations:", #valign := Gtk.AlignCenter]
    olderGenerationsRadio = inBox def $ widget
      Gtk.Switch
      [ #active := (as ^. asGarbageData . gdOlderGenerations)
      , on
        #stateSet
        (\b ->
          (False, ManagerEventAdmin (EventGarbageOlderGenerationsChanged b))
        )
      , #valign := Gtk.AlignCenter
      , #vexpand := False
      ]
    olderGenerationsDescription = inBox def $ widget
      Gtk.Label
      [ #label
        := "Whether to delete old NixOS generations (think of every change you applied in the form above); this makes rolling back to a previous state impossible, so use with care."
      , #wrap := True
      , #hexpand := True
      , #halign := Gtk.AlignFill
      , classes ["nixos-manager-italic"]
      ]
    applyRow = 1
  in
    container
      Gtk.Grid
      gridProperties
      [ GridChild (def { leftAttach = 0, topAttach = olderGenerationsRow })
                  olderGenerationsLabel
      , GridChild (def { leftAttach = 1, topAttach = olderGenerationsRow })
                  olderGenerationsRadio
      , GridChild (def { leftAttach = 2, topAttach = olderGenerationsRow })
                  olderGenerationsDescription
      , GridChild (def { width = 3, topAttach = applyRow }) lastLine
      ]


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

detailsBox detailsState processOutput eventF =
  let processExpanded w = do
        expandedState <- Gtk.getExpanderExpanded w
        pure
          ( ManagerEventAdmin
          . eventF
          . view (from detailsBool)
          . not
          $ expandedState
          )
  in
    [ BoxChild expandAndFill
      $ bin
          Gtk.Expander
          [ #label := "Build details"
          , #expanded := (detailsState ^. detailsBool)
          , onM #activate processExpanded
          ]
      $ container
          Gtk.Box
          [#orientation := Gtk.OrientationVertical]
          [ BoxChild fillNoExpand $ widget Gtk.Separator []
          , BoxChild def
            $ widget Gtk.Label [#label := "Build command standard output:"]
          , BoxChild expandAndFill
          $ bin Gtk.ScrolledWindow [classes ["nixos-manager-grey-background"]]
          $ widget
              Gtk.Label
              [ #label := (processOutput ^. poStdout . to decodeUtf8)
              , classes ["nixos-manager-monospace"]
              , #valign := Gtk.AlignStart
              ]
          , BoxChild def
            $ widget Gtk.Label [#label := "Build command standard error:"]
          , BoxChild expandAndFill
          $ bin Gtk.ScrolledWindow [classes ["nixos-manager-grey-background"]]
          $ widget
              Gtk.Label
              [ #label := (processOutput ^. poStderr . to decodeUtf8)
              , #wrap := True
              , #valign := Gtk.AlignStart
              , classes ["nixos-manager-monospace"]
              ]
          ]
    ]


rebuildBox as =
  let lastStatusRow =
          case
              ( has (asRebuildData . rdBuildState . _Nothing) as
              , as ^. asRebuildData . rdProcessOutput . poResult . to getFirst
              )
            of
              (True, Just v) -> [BoxChild def (paddedAround 5 (statusLabel v))]
              _              -> []
      rebuildDetails = detailsBox (as ^. asRebuildData . rdDetailsState)
                                  (as ^. asRebuildData . rdProcessOutput)
                                  EventRebuildChangeDetails
      frameMargin = 60
  in  bin
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

garbageBox as =
  let lastStatusRow =
          case
              ( has (asGarbageData . gdBuildState . _Nothing) as
              , as ^. asGarbageData . gdProcessOutput . poResult . to getFirst
              )
            of
              (True, Just v) -> [BoxChild def (paddedAround 5 (statusLabel v))]
              _              -> []
      details = detailsBox (as ^. asGarbageData . gdDetailsState)
                           (as ^. asGarbageData . gdProcessOutput)
                           EventGarbageChangeDetails
      frameMargin = 60
  in  bin
          Gtk.Frame
          [ #label := "Collecting garbage "
          , #marginLeft := frameMargin
          , #marginRight := frameMargin
          ]
        $ paddedAround 10
        $ container
            Gtk.Box
            [#orientation := Gtk.OrientationVertical, #spacing := 5]
            ([BoxChild def (garbageGrid as)] <> lastStatusRow <> details)


buildingBox cancelEvent buildState = container
  Gtk.Box
  [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  [ BoxChild fillNoExpand $ imageButton
    [ #label := "Cancel"
    , on #clicked (ManagerEventAdmin cancelEvent)
    , #valign := Gtk.AlignCenter
    , #alwaysShowImage := True
    ]
    IconName.ProcessStop
  , BoxChild expandAndFill $ progressBar
    [#showText := True, #text := "Rebuilding..."]
    (buildState ^. bsCounter)
  ]

adminBox' ms =
  let
    headlineItems =
      [ BoxChild fillNoExpand $ widget
        Gtk.Label
        [ #label := "Welcome to NixOS-Manager"
        , classes ["nixos-manager-headline"]
        ]
      , BoxChild def $ informationBox True
        IconName.DialogInformation
        "Select the “Add/Remove Software” and “Configure your system” tabs above to make changes to your system.\nOnce you're done with that, <b>apply the changes</b> using the form below."
      ]
  in
    bin Gtk.ScrolledWindow []
    $  container
         Gtk.Box
         [ #orientation := Gtk.OrientationVertical
         , #spacing := 20
         , #marginLeft := 5
         , #marginRight := 5
         , #marginTop := 5
         , #marginBottom := 5
         ]
    $  headlineItems
    <> [rebuildBox (ms ^. msAdminState)]
    <> [ BoxChild def $ informationBox True
           IconName.UserTrash
           "NixOS doesn't explicitly delete anything once it has been downloaded.\nThis makes reinstalling things faster, but your disk drive will dwindle over time.\nThat’s why you should collect all the garbage regularly using this form."
       ]
    <> [garbageBox (ms ^. msAdminState)]
