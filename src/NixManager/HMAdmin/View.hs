{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
  Description: Contains the actual GUI (widgets) for the home-manager Administation tab
Contains the actual GUI (widgets) for the home-manager Administation tab
  -}
module NixManager.HMAdmin.View
  ( adminBox
  )
where

import           NixManager.View.InformationBox ( informationBox )
import           NixManager.HMRebuildMode       ( rebuildModes
                                                , rebuildModeToText
                                                , rebuildModeIdx
                                                , rebuildModeToDescription
                                                )
import           System.Exit                    ( ExitCode(ExitSuccess) )
import qualified NixManager.View.IconName      as IconName
import           NixManager.View.GtkUtil        ( expandAndFill
                                                , fillNoExpand
                                                , paddedAround
                                                )
import           NixManager.HMAdmin.GenerationsView
                                                ( generationsView )
import           NixManager.View.ImageButton    ( imageButton )
import           NixManager.View.ProgressBar    ( progressBar )
import           NixManager.ChangeType          ( ChangeType(Changes) )
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
                                                  ( ManagerEventHMAdmin
                                                  )
                                                )
import           NixManager.HMAdmin.Event       ( Event
                                                  ( EventRebuild
                                                  , EventGarbage
                                                  , EventGenerations
                                                  , EventGarbageCancel
                                                  , EventGarbageChangeDetails
                                                  , EventRebuildCancel
                                                  , EventRebuildModeIdxChanged
                                                  , EventRebuildChangeDetails
                                                  )
                                                )
import           NixManager.ManagerState        ( ManagerState
                                                , msHMAdminState
                                                )
import           GI.Gtk.Declarative.Widget      ( Widget )
import           NixManager.HMAdmin.State       ( rebuildData
                                                , garbageData
                                                , generationsState
                                                , changes
                                                )
import           NixManager.HMAdmin.GarbageData ( gdDetailsState
                                                , gdBuildState
                                                , gdProcessOutput
                                                )
import           Control.Lens                   ( (^.)
                                                , has
                                                , _Nothing
                                                , view
                                                , from
                                                , to
                                                )
import           Data.Monoid                    ( getFirst )
import           Data.Default                   ( def )
import           NixManager.View.DetailsState   ( detailsBool )
import           NixManager.HMAdmin.RebuildData ( rdBuildState
                                                , rdActiveRebuildModeIdx
                                                , rdDetailsState
                                                , rdProcessOutput
                                                )
import           NixManager.HMAdmin.BuildState  ( bsCounter )


-- | The “root” GUI function
adminBox :: ManagerState -> Widget ManagerEvent
adminBox s = container Gtk.Box [] [BoxChild expandAndFill (adminBox' s)]

-- | The grid for the “rebuild” GUI
rebuildGrid as =
  let hasChanges  = (as ^. changes) == Changes
      applyButton = imageButton
        [ #label
          := (if hasChanges
               then "Apply Changes"
               else "Rebuild (no changes to apply)"
             )
        , on #clicked (ManagerEventHMAdmin EventRebuild)
        , #valign := Gtk.AlignCenter
        , #alwaysShowImage := True
        ]
        IconName.ViewRefresh
      lastLine = maybe applyButton
                       (buildingBox EventRebuildCancel)
                       (as ^. rebuildData . rdBuildState)
      changeBuildType :: ComboBoxChangeEvent -> ManagerEvent
      changeBuildType (ComboBoxChangeEvent idx) =
          ManagerEventHMAdmin (EventRebuildModeIdxChanged idx)
      gridProperties = [#rowSpacing := 10, #columnSpacing := 10]
      buildTypeLabel =
          widget Gtk.Label [#label := "Build type: ", #valign := Gtk.AlignCenter]
      buildTypeCombo = changeBuildType <$> comboBox
        [#valign := Gtk.AlignCenter]
        (ComboBoxProperties (rebuildModeToText <$> rebuildModes)
                            (as ^. rebuildData . rdActiveRebuildModeIdx)
        )
      buildTypeDescription = widget
        Gtk.Label
        [ #label
          := (  as
             ^. rebuildData
             .  rdActiveRebuildModeIdx
             .  from rebuildModeIdx
             .  to rebuildModeToDescription
             )
        , #wrap := True
        , #hexpand := True
        , #halign := Gtk.AlignFill
        , classes ["nixos-manager-italic"]
        ]
      buildTypeRow = 0
      applyRow     = 1
  in  container
        Gtk.Grid
        gridProperties
        [ GridChild (def { leftAttach = 0, topAttach = buildTypeRow })
                    buildTypeLabel
        , GridChild (def { leftAttach = 1, topAttach = buildTypeRow })
                    buildTypeCombo
        , GridChild (def { leftAttach = 2, topAttach = buildTypeRow })
                    buildTypeDescription
        , GridChild (def { width = 3, topAttach = applyRow }) lastLine
        ]

-- | The grid for the “Collect garbage” GUI
garbageGrid as =
  let applyButton = imageButton
        [ #label := "Collect Garbage"
        , on #clicked (ManagerEventHMAdmin EventGarbage)
        , #valign := Gtk.AlignCenter
        , #alwaysShowImage := True
        ]
        IconName.UserTrash
      lastLine = maybe applyButton
                       (buildingBox EventGarbageCancel)
                       (as ^. garbageData . gdBuildState)
  in  lastLine

-- | A descriptive label for a process exit status
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

-- | The details box (the one you can expand/contract). Here in general, for GC and Rebuild
detailsBox detailsState processOutput eventF =
  let processExpanded w = do
        expandedState <- Gtk.getExpanderExpanded w
        pure
          ( ManagerEventHMAdmin
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

-- | The rebuild GUI
rebuildBox as =
  let lastStatusRow =
          case
              ( has (rebuildData . rdBuildState . _Nothing) as
              , as ^. rebuildData . rdProcessOutput . poResult . to getFirst
              )
            of
              (True, Just v) -> [BoxChild def (paddedAround 5 (statusLabel v))]
              _              -> []
      rebuildDetails = detailsBox (as ^. rebuildData . rdDetailsState)
                                  (as ^. rebuildData . rdProcessOutput)
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

-- | The collect garbage GUI
garbageBox as =
  let lastStatusRow =
          case
              ( has (garbageData . gdBuildState . _Nothing) as
              , as ^. garbageData . gdProcessOutput . poResult . to getFirst
              )
            of
              (True, Just v) -> [BoxChild def (paddedAround 5 (statusLabel v))]
              _              -> []
      details = detailsBox (as ^. garbageData . gdDetailsState)
                           (as ^. garbageData . gdProcessOutput)
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

-- | When rebuilding, the Rebuild button changes to this progress bar plus cancel button
buildingBox cancelEvent buildState = container
  Gtk.Box
  [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  [ BoxChild fillNoExpand $ imageButton
    [ #label := "Cancel"
    , on #clicked (ManagerEventHMAdmin cancelEvent)
    , #valign := Gtk.AlignCenter
    , #alwaysShowImage := True
    ]
    IconName.ProcessStop
  , BoxChild expandAndFill $ progressBar
    [#showText := True, #text := "Rebuilding..."]
    (buildState ^. bsCounter)
  ]

generationsBox as =
  let frameMargin = 60
  in  bin
          Gtk.Frame
          [ #label := "Generations"
          , #marginLeft := frameMargin
          , #marginRight := frameMargin
          ]
        $ paddedAround 10
        $ container
            Gtk.Box
            [#orientation := Gtk.OrientationVertical, #spacing := 5]
            [ BoxChild
                def
                (ManagerEventHMAdmin . EventGenerations <$> generationsView
                  (as ^. generationsState)
                )
            ]


-- | The child immediately below the root node, containing the headline and all the other stuff.
adminBox' ms =
  let
    headlineItems =
      [ BoxChild fillNoExpand $ widget
        Gtk.Label
        [#label := "home-manager", classes ["nixos-manager-headline"]]
      , BoxChild def $ informationBox
        True
        IconName.DialogInformation
        "Select the “Add/Remove Software” and “Configure your home” tabs above to make changes to your system.\nOnce you're done with that, <b>apply the changes</b> using the form below."
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
    <> [rebuildBox (ms ^. msHMAdminState)]
    <> [ BoxChild def $ informationBox
           True
           IconName.DriveMultidisk
           "When you apply changes in home-manager, your old configuration state is not lost.\nInstead, a new “generation” is created, so you can go back to older states with ease."
       ]
    <> [generationsBox (ms ^. msHMAdminState)]
    <> [ BoxChild def $ informationBox
           True
           IconName.UserTrash
           "Nix doesn't explicitly delete anything once it has been downloaded.\nThis makes reinstalling things faster, but your disk drive will dwindle over time.\nThat’s why you should collect all the garbage regularly using this form."
       ]
    <> [garbageBox (ms ^. msHMAdminState)]
