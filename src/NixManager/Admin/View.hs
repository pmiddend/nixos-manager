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

import           System.Exit                    ( ExitCode
                                                  ( ExitFailure
                                                  , ExitSuccess
                                                  )
                                                )
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
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( decodeUtf8 )
import           NixManager.Process             ( poStdout
                                                , poStderr
                                                , poResult
                                                )
import           GI.Gtk.Declarative             ( bin
                                                , on
                                                , classes
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
import           NixManager.Admin.Event         ( Event
                                                  ( EventRebuild
                                                  , EventRebuildCancel
                                                  , EventBuildTypeChanged
                                                  )
                                                )
import           NixManager.ManagerState        ( ManagerState
                                                , msAdminState
                                                )
import           GI.Gtk.Declarative.Widget      ( Widget )
import           NixManager.Admin.State         ( asActiveBuildType
                                                , asProcessOutput
                                                , absCounter
                                                , asBuildState
                                                )
import           Control.Lens                   ( (^.)
                                                , has
                                                , _Nothing
                                                , folded
                                                , to
                                                , (^?!)
                                                , ix
                                                )
import           Data.Monoid                    ( getFirst )

adminBox :: ManagerState -> Widget ManagerEvent
adminBox s = container
  Gtk.Box
  []
  [ BoxChild (defaultBoxChildProperties { expand = True, fill = True })
             (adminBox' s)
  ]

rebuildTypesWithDescription :: [(Text, Text)]
rebuildTypesWithDescription =
  [ ( "switch"
    , "Build and activate the changes immediately. You can go back to previous configurations by rebooting and selecting an older generation."
    )
  , ( "boot"
    , "Build the new configuration and make it the boot default, but do not activate it. That is, the system continues to run the previous configuration until the next reboot."
    )
  , ( "test"
    , "Build and activate the new configuration, but do not add it to the GRUB boot menu. Thus, if you reboot the system (or if it crashes), you will automatically revert to the default configuration (i.e. the configuration resulting from the last rebuild)."
    )
  , ( "dry-build"
    , "Show what store paths would be built or downloaded by any of the operations above, but otherwise do nothing."
    )
  , ( "dry-activate"
    , "Build the new configuration, but instead of activating it, show what changes would be performed by the activation. For instance, this command will print which systemd units would be restarted. The list of changes is not guaranteed to be complete."
    )
  ]

rebuildTypes :: [Text]
rebuildTypes = fst <$> rebuildTypesWithDescription

rebuildRow as =
  let changeCallback :: ComboBoxChangeEvent -> ManagerEvent
      changeCallback (ComboBoxChangeEvent (Just idx)) = ManagerEventAdmin
        (EventBuildTypeChanged (rebuildTypes ^?! ix (fromIntegral idx)))
  in  [ BoxChild defaultBoxChildProperties $ container
          Gtk.Box
          [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
          [ BoxChild fillNoExpand $ imageButton
            [ #label := "Apply Changes"
            , on #clicked (ManagerEventAdmin EventRebuild)
            , #valign := Gtk.AlignCenter
            , #alwaysShowImage := True
            ]
            IconName.ViewRefresh
          , BoxChild fillNoExpand $ widget
            Gtk.Label
            [#label := "Build type: ", #valign := Gtk.AlignCenter]
          , BoxChild fillNoExpand $ changeCallback <$> comboBox
            [#valign := Gtk.AlignCenter]
            (ComboBoxProperties
              rebuildTypes
              (   fromIntegral
              <$> ((as ^. asActiveBuildType) `elemIndex` rebuildTypes)
              )
            )
          , BoxChild fillNoExpand $ widget
            Gtk.Label
            [ #label
              := (   rebuildTypesWithDescription
                 ^?! to (lookup (as ^. asActiveBuildType))
                 .   folded
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
    rebuildDetails =
      [ BoxChild expandAndFill
          $ bin Gtk.Expander [#label := "Build details"]
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

