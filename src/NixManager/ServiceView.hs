{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.ServiceView
  ( servicesBox
  )
where

import qualified Data.Vector                   as Vector
import           NixManager.PackageView         ( packagesBox )
import           Data.Text                      ( intercalate )
import           NixManager.Nix
import           GI.Gtk.Declarative             ( bin
                                                , onM
                                                , pane
                                                , paned
                                                , notebook
                                                , page
                                                , defaultPaneProperties
                                                , BoxChild(BoxChild)
                                                , defaultBoxChildProperties
                                                , FromWidget
                                                , Bin
                                                , widget
                                                , Attribute((:=))
                                                , container
                                                , on
                                                )
import           GI.Gtk.Declarative.App.Simple  ( AppView )
import           Data.Vector.Lens               ( toVectorOf )
import qualified GI.Gtk                        as Gtk
import           Control.Lens                   ( (^.)
                                                , ix
                                                , to
                                                , folded
                                                , (^..)
                                                , (^?!)
                                                )
import           NixManager.ManagerState
import           NixManager.ManagerEvent


buildServiceRow
  :: FromWidget (Bin Gtk.ListBoxRow) target => NixService -> target event
buildServiceRow svc = bin
  Gtk.ListBoxRow
  []
  (widget Gtk.Label [#label := (svc ^. serviceLoc . to (intercalate "."))])

rowSelectionHandler :: Maybe Gtk.ListBoxRow -> Gtk.ListBox -> IO ManagerEvent
rowSelectionHandler (Just row) _ = do
  selectedIndex <- Gtk.listBoxRowGetIndex row
  if selectedIndex == -1
    then pure (ManagerEventServiceSelected Nothing)
    else pure (ManagerEventServiceSelected (Just (fromIntegral selectedIndex)))
rowSelectionHandler _ _ = pure (ManagerEventServiceSelected Nothing)


serviceRows s = toVectorOf (msServiceCache . folded . to buildServiceRow) s

servicesLeftPane s = bin
  Gtk.ScrolledWindow
  []
  (container Gtk.ListBox [onM #rowSelected rowSelectionHandler] (serviceRows s))

servicesRightPane s = case s ^. msSelectedServiceIdx of
  Nothing ->
    widget Gtk.Label [#label := "Please select a service from the left pane"]
  Just idx ->
    let
      svc      = s ^?! msServiceCache . ix idx
      svcLabel = (svc ^. serviceLoc . to (intercalate "."))
      optBox =
        container Gtk.Box
                  [#orientation := Gtk.OrientationVertical, #spacing := 10]
          $ Vector.fromList
              ( BoxChild defaultBoxChildProperties
                         (widget Gtk.Label [#label := svcLabel])
              : (   svc
                ^.. serviceOptions
                .   folded
                .   optionLoc
                .   to (intercalate ".")
                .   to
                      (\l -> BoxChild defaultBoxChildProperties
                        $ widget Gtk.Label [#label := l]
                      )
                )
              )
    in
      optBox

servicesBox s = paned []
                      (pane defaultPaneProperties (servicesLeftPane s))
                      (pane defaultPaneProperties $ (servicesRightPane s))
