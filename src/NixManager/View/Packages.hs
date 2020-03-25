{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.View.Packages
  ( packagesBox
  )
where

import           NixManager.PackagesEvent       ( PackagesEvent
                                                  ( PackagesEventSearchChanged
                                                  , PackagesEventPackageSelected
                                                  , PackagesEventTryInstall
                                                  , PackagesEventInstall
                                                  , PackagesEventUninstall
                                                  )
                                                )
import           NixManager.ManagerEvent        ( ManagerEvent
                                                  ( ManagerEventPackages
                                                  )
                                                )
import           NixManager.NixPackage          ( NixPackage
                                                , npInstalled
                                                , npName
                                                )
import           Data.Semigroup                 ( Any(Any)
                                                , getAny
                                                )
import           Data.Maybe                     ( isJust )
import           Prelude                 hiding ( length )
import           Data.Text                      ( length )
import           GI.Gtk.Declarative.Container.Class
                                                ( Children )
import           GI.Gtk.Declarative             ( bin
                                                , Widget
                                                , padding
                                                , defaultBoxChildProperties
                                                , expand
                                                , Container
                                                , fill
                                                , FromWidget
                                                , Bin
                                                , widget
                                                , Attribute((:=))
                                                , classes
                                                , container
                                                , BoxChild(BoxChild)
                                                , on
                                                , onM
                                                )
import           Data.Vector.Lens               ( toVectorOf )
import qualified GI.Gtk                        as Gtk
import           Data.GI.Base.Overloading       ( IsDescendantOf )
import           Control.Lens                   ( (^.)
                                                , to
                                                , has
                                                , folded
                                                )
import           NixManager.ManagerState        ( msSearchString
                                                , ManagerState
                                                , msSearchResult
                                                , msSelectedPackage
                                                , msLatestMessage
                                                )
import           NixManager.Util                ( mwhen )
import           NixManager.Message             ( messageText
                                                , messageType
                                                , _ErrorMessage
                                                )
import           Control.Monad.IO.Class         ( MonadIO )


processSearchChange
  :: (IsDescendantOf Gtk.Entry o, MonadIO f, Gtk.GObject o)
  => o
  -> f ManagerEvent
processSearchChange w =
  ManagerEventPackages . PackagesEventSearchChanged <$> Gtk.getEntryText w

searchLabel :: Widget event
searchLabel =
  widget Gtk.Label [#label := "Search in packages:", #halign := Gtk.AlignEnd]

searchField :: Widget ManagerEvent
searchField = widget
  Gtk.SearchEntry
  [ #placeholderText := "Enter a package name or part of a name..."
  , #maxWidthChars := 50
  , onM #searchChanged processSearchChange
  , #halign := Gtk.AlignFill
  ]

searchBox :: Widget ManagerEvent
searchBox = container
  Gtk.Box
  [#orientation := Gtk.OrientationHorizontal, #spacing := 10]
  [ BoxChild (defaultBoxChildProperties { expand = True, fill = True })
             searchLabel
  , BoxChild (defaultBoxChildProperties { expand = True, fill = True })
             searchField
  ]


buildResultRow
  :: FromWidget (Bin Gtk.ListBoxRow) target => NixPackage -> target event
buildResultRow pkg = bin
  Gtk.ListBoxRow
  [classes (mwhen (pkg ^. npInstalled) ["package-row-installed"])]
  (widget Gtk.Label [#label := (pkg ^. npName)])

rowSelectionHandler :: Maybe Gtk.ListBoxRow -> Gtk.ListBox -> IO ManagerEvent
rowSelectionHandler (Just row) _ = do
  selectedIndex <- Gtk.listBoxRowGetIndex row
  if selectedIndex == -1
    then pure (ManagerEventPackages (PackagesEventPackageSelected Nothing))
    else pure
      (ManagerEventPackages
        (PackagesEventPackageSelected (Just (fromIntegral selectedIndex)))
      )
rowSelectionHandler _ _ =
  pure (ManagerEventPackages (PackagesEventPackageSelected Nothing))

packagesBox
  :: FromWidget (Container Gtk.Box (Children BoxChild)) target
  => ManagerState
  -> target ManagerEvent
packagesBox s =
  let
    searchValid     = (s ^. msSearchString . to length) >= 2
    resultRows      = toVectorOf (msSearchResult . folded . to buildResultRow) s
    packageSelected = isJust (s ^. msSelectedPackage)
    currentPackageInstalled =
      getAny (s ^. msSelectedPackage . folded . npInstalled . to Any)
    packageButtonRow = container
      Gtk.Box
      [#orientation := Gtk.OrientationHorizontal, #spacing := 10]
      [ BoxChild
        (defaultBoxChildProperties { expand = True, fill = True })
        (widget
          Gtk.Button
          [ #label := "Try without installing"
          , #sensitive := (packageSelected && not currentPackageInstalled)
          , classes ["try-install-button"]
          , on #clicked (ManagerEventPackages PackagesEventTryInstall)
          ]
        )
      , BoxChild
        (defaultBoxChildProperties { expand = True, fill = True })
        (widget
          Gtk.Button
          [ #label := "Install"
          , #sensitive := (packageSelected && not currentPackageInstalled)
          , classes ["install-button"]
          , on #clicked (ManagerEventPackages PackagesEventInstall)
          ]
        )
      , BoxChild
        (defaultBoxChildProperties { expand = True, fill = True })
        (widget
          Gtk.Button
          [ #label := "Remove"
          , #sensitive := (packageSelected && currentPackageInstalled)
          , classes ["remove-button"]
          , on #clicked (ManagerEventPackages PackagesEventUninstall)
          ]
        )
      ]
    resultBox = if searchValid
      then bin
        Gtk.ScrolledWindow
        []
        (container Gtk.ListBox [onM #rowSelected rowSelectionHandler] resultRows
        )
      else widget
        Gtk.Label
        [ #label := "Please enter a search term with at least two characters"
        , #expand := True
        ]
  in
    container
      Gtk.Box
      [#orientation := Gtk.OrientationVertical, #spacing := 10]
      (  [ BoxChild (defaultBoxChildProperties { padding = 5 }) searchBox
         , widget Gtk.HSeparator []
         ]
      <> foldMap
           (\e ->
             [ BoxChild
                 defaultBoxChildProperties
                 (widget
                   Gtk.Label
                   [ #label := (e ^. messageText)
                   , classes
                     [ if has (messageType . _ErrorMessage) e
                         then "error-message"
                         else "info-message"
                     ]
                   ]
                 )
             ]
           )
           (s ^. msLatestMessage)

      <> [ packageButtonRow
         , widget Gtk.HSeparator []
         , BoxChild
           (defaultBoxChildProperties { expand = True, fill = True })
           resultBox
         ]
      )

