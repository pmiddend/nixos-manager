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
                                                , defaultBoxChildProperties
                                                , widget
                                                , Attribute((:=))
                                                , container
                                                )
import qualified GI.Gtk                        as Gtk
import           NixManager.ManagerEvent        ( ManagerEvent
                                                  ( ManagerEventAdmin
                                                  )
                                                )
import           NixManager.Changes             ( ChangeType(Changes) )
import           NixManager.Admin.Event         ( Event
                                                  ( EventRebuild
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

adminBox :: ManagerState -> Widget ManagerEvent
adminBox s = container
  Gtk.Box
  []
  [ BoxChild (defaultBoxChildProperties { expand = True, fill = True })
             (adminBox' s)
  ]

rebuildRow as =
  let changeCallback :: ComboBoxChangeEvent -> ManagerEvent
      changeCallback (ComboBoxChangeEvent (Just idx)) = ManagerEventAdmin
        (EventRebuildModeChanged
          (rebuildTypes ^?! ix (fromIntegral idx) . to showText)
        )
      changes = (as ^. asChanges) == Changes
  in  [ BoxChild defaultBoxChildProperties $ container
          Gtk.Box
          [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
          [ BoxChild fillNoExpand $ imageButton
            [ #label
              := (if changes then "Apply Changes" else "No changes to apply")
            , on #clicked (ManagerEventAdmin EventRebuild)
            , #valign := Gtk.AlignCenter
            , #alwaysShowImage := True
            , #sensitive := changes
            ]
            IconName.ViewRefresh
          , BoxChild fillNoExpand $ widget
            Gtk.Label
            [#label := "Build type: ", #valign := Gtk.AlignCenter]
          , BoxChild fillNoExpand $ changeCallback <$> comboBox
            [#valign := Gtk.AlignCenter]
            (ComboBoxProperties
              (showText <$> rebuildTypes)
              (   fromIntegral
              <$> ((as ^. asActiveRebuildMode) `elemIndex` rebuildTypes)
              )
            )
          , BoxChild fillNoExpand $ widget
            Gtk.Label
            [ #label
              := (   descriptionForRebuildType (as ^. asActiveRebuildMode)
                 ^?! folded
                 )
            , #wrap := True
            , classes ["nixos-manager-italic"]
            ]
          ]
      ]

buildingBox buildState =
  [ BoxChild defaultBoxChildProperties $ container
      Gtk.Box
      [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
      [ BoxChild fillNoExpand $ imageButton
        [ #label := "Cancel"
        , on #clicked (ManagerEventAdmin EventRebuildCancel)
        , #valign := Gtk.AlignCenter
        , #alwaysShowImage := True
        ]
        IconName.ProcessStop
      , BoxChild (defaultBoxChildProperties { fill = True, expand = True })
        $ progressBar [#showText := True, #text := "Rebuilding..."]
                      (buildState ^. absCounter)
      ]
  ]

adminBox' ms =
  let
    headlineItems =
      [ BoxChild fillNoExpand $ widget
        Gtk.Label
        [ #label := "Welcome to NixOS-Manager"
        , classes ["nixos-manager-headline"]
        ]
      , BoxChild fillNoExpand $ widget
        Gtk.Label
        [ #label
          := "Select the “Packages” and “Services” tabs above to make changes to your system.\nOnce you're done with that, apply the changes using the form below."
        , #wrap := True
        , classes ["nixos-manager-italic"]
        ]
      , BoxChild fillNoExpand $ widget Gtk.Separator []
      ]
    rebuildButtons = maybe (rebuildRow (ms ^. msAdminState))
                           buildingBox
                           (ms ^. msAdminState . asBuildState)
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
    lastStatusRow =
      case
          ( has (msAdminState . asBuildState . _Nothing) ms
          , ms ^. msAdminState . asProcessOutput . poResult . to getFirst
          )
        of
          (True, Just v) ->
            [ BoxChild defaultBoxChildProperties
                       (paddedAround 5 (statusLabel v))
            ]
          _ -> []
    processExpanded w = do
      expandedState <- Gtk.getExpanderExpanded w
      pure
        ( ManagerEventAdmin
        . EventChangeDetails
        . view (from detailsBool)
        . not
        $ expandedState
        )
    rebuildDetails =
      [ BoxChild expandAndFill
          $ bin
              Gtk.Expander
              [ #label := "Build details"
              , #expanded := (ms ^. msAdminState . asDetailsState . detailsBool)
              , onM #activate processExpanded
              ]
          $ container
              Gtk.Box
              [#orientation := Gtk.OrientationVertical]
              [ BoxChild fillNoExpand $ widget Gtk.Separator []
              , BoxChild defaultBoxChildProperties
                $ widget Gtk.Label [#label := "Build command standard output:"]
              , BoxChild
                (defaultBoxChildProperties { expand = True, fill = True })
              $ bin Gtk.ScrolledWindow
                    [classes ["nixos-manager-grey-background"]]
              $ widget
                  Gtk.Label
                  [ #label
                    := (  ms
                       ^. msAdminState
                       .  asProcessOutput
                       .  poStdout
                       .  to decodeUtf8
                       )
                  , classes ["nixos-manager-monospace"]
                  , #valign := Gtk.AlignStart
                  ]
              , BoxChild defaultBoxChildProperties
                $ widget Gtk.Label [#label := "Build command standard error:"]
              , BoxChild
                (defaultBoxChildProperties { expand = True, fill = True })
              $ bin Gtk.ScrolledWindow
                    [classes ["nixos-manager-grey-background"]]
              $ widget
                  Gtk.Label
                  [ #label
                    := (  ms
                       ^. msAdminState
                       .  asProcessOutput
                       .  poStderr
                       .  to decodeUtf8
                       )
                  , #wrap := True
                  , #valign := Gtk.AlignStart
                  , classes ["nixos-manager-monospace"]
                  ]
              ]
      ]
  in
    container
      Gtk.Box
      [ #orientation := Gtk.OrientationVertical
      , #spacing := 3
      , #marginLeft := 5
      , #marginRight := 5
      , #marginTop := 5
      , #marginBottom := 5
      ]
    $  headlineItems
    <> rebuildButtons
    <> lastStatusRow
    <> rebuildDetails

