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

import qualified NixManager.View.IconName      as IconName
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
import           GI.Gtk.Declarative.Container.Box
                                                ( BoxChildProperties )
import           NixManager.Admin.State         ( asActiveBuildType
                                                , asProcessOutput
                                                , absCounter
                                                , asBuildState
                                                )
import           Control.Lens                   ( (^.)
                                                , folded
                                                , to
                                                , (^?!)
                                                , ix
                                                )

adminBox :: ManagerState -> Widget ManagerEvent
adminBox s = container
  Gtk.Box
  []
  [ BoxChild (defaultBoxChildProperties { expand = True, fill = True })
             (adminBox' s)
  ]

fillNoExpand :: BoxChildProperties
fillNoExpand = defaultBoxChildProperties { expand = False, fill = True }

rebuildTypesWithDescription :: [(Text, Text)]
rebuildTypesWithDescription =
  [ ( "switch"
    , "Build and activate the new configuration, and make it the boot default. That is, the configuration is added to the GRUB boot menu as the default menu entry, so that subsequent reboots will boot the system into the new configuration. Previous configurations activated with nixos-rebuild switch or nixos-rebuild boot remain available in the GRUB menu."
    )
  , ( "boot"
    , "Build the new configuration and make it the boot default (as with nixos-rebuild switch), but do not activate it. That is, the system continues to run the previous configuration until the next reboot."
    )
  , ( "test"
    , "Build and activate the new configuration, but do not add it to the GRUB boot menu. Thus, if you reboot the system (or if it crashes), you will automatically revert to the default configuration (i.e. the configuration resulting from the last call to nixos-rebuild switch or nixos-rebuild boot)."
    )
  , ( "dry-build"
    , "Show what store paths would be built or downloaded by any of the operations above, but otherwise do nothing."
    )
  , ( "dry-activate"
    , "Build the new configuration, but instead of activating it, show what changes would be performed by the activation (i.e. by nixos-rebuild test). For instance, this command will print which systemd units would be restarted. The list of changes is not guaranteed to be complete."
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
  container
      Gtk.Box
      [ #orientation := Gtk.OrientationVertical
      , #spacing := 3
      , #marginLeft := 5
      , #marginRight := 5
      , #marginTop := 5
      , #marginBottom := 5
      ]
    $  [ BoxChild fillNoExpand $ widget
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
    <> maybe (rebuildRow (ms ^. msAdminState))
             buildingBox
             (ms ^. msAdminState . asBuildState)
    <> [ BoxChild fillNoExpand $ widget Gtk.Separator []
       , BoxChild defaultBoxChildProperties
         $ widget Gtk.Label [#label := "Build command standard output:"]
       , BoxChild (defaultBoxChildProperties { expand = True, fill = True })
       $ bin Gtk.ScrolledWindow [classes ["nixos-manager-grey-background"]]
       $ widget
           Gtk.Label
           [ #label
             := (ms ^. msAdminState . asProcessOutput . poStdout . to decodeUtf8
                )
           , classes ["nixos-manager-monospace"]
           , #valign := Gtk.AlignStart
           ]
       , BoxChild defaultBoxChildProperties
         $ widget Gtk.Label [#label := "Build command standard error:"]
       , BoxChild (defaultBoxChildProperties { expand = True, fill = True })
       $ bin Gtk.ScrolledWindow [classes ["nixos-manager-grey-background"]]
       $ widget
           Gtk.Label
           [ #label
             := (ms ^. msAdminState . asProcessOutput . poStderr . to decodeUtf8
                )
           , #wrap := True
           , #valign := Gtk.AlignStart
           , classes ["nixos-manager-monospace"]
           ]
       ]

