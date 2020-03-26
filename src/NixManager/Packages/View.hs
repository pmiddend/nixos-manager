{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.Packages.View
  ( packagesBox
  )
where

import           NixManager.View.ProgressBar    ( progressBar )
import           NixManager.Packages.Event      ( Event
                                                  ( EventSearchChanged
                                                  , EventPackageSelected
                                                  , EventTryInstall
                                                  , EventInstall
                                                  , EventUninstall
                                                  , EventTryInstallCancel
                                                  )
                                                )
import           NixManager.ManagerEvent        ( ManagerEvent
                                                  ( ManagerEventPackages
                                                  )
                                                )
import           NixManager.NixPackage          ( NixPackage
                                                , npInstalled
                                                , npName
                                                , npPath
                                                , npDescription
                                                )
import           Data.Semigroup                 ( Any(Any)
                                                , getAny
                                                )
import           Data.Maybe                     ( isJust
                                                , fromMaybe
                                                )
import           Prelude                 hiding ( length )
import           Data.Text                      ( length
                                                , Text
                                                , stripPrefix
                                                )
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
import qualified Data.Vector                   as Vector
import qualified GI.Gtk                        as Gtk
import           Data.GI.Base.Overloading       ( IsDescendantOf )
import           Control.Lens                   ( (^.)
                                                , (^..)
                                                , to
                                                , has
                                                , folded
                                                )
import           NixManager.ManagerState        ( ManagerState
                                                , msPackagesState
                                                )
import           NixManager.Packages.State      ( psSearchString
                                                , isCounter
                                                , psSearchResult
                                                , psSelectedPackage
                                                , psLatestMessage
                                                , psInstallingPackage
                                                )
import           NixManager.Util                ( mwhen
                                                , replaceHtmlEntities
                                                )
import           NixManager.View.GtkUtil        ( paddedAround )
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
  ManagerEventPackages . EventSearchChanged <$> Gtk.getEntryText w

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

stripPrefixSafe :: Text -> Text -> Text
stripPrefixSafe prefix t = fromMaybe t (stripPrefix prefix t)

formatPkgLabel :: NixPackage -> Text
formatPkgLabel pkg =
  let path = stripPrefixSafe "nixpkgs." (pkg ^. npPath)
      name = pkg ^. npName
      firstLine =
          if path == name then name else name <> (" (<tt>" <> path <> "</tt>)")
  in  firstLine
        <> "\n<i>"
        <> (pkg ^. npDescription . to replaceHtmlEntities)
        <> "</i>"


buildResultRow
  :: FromWidget (Bin Gtk.ListBoxRow) target => Int -> NixPackage -> target event
buildResultRow i pkg = bin
  Gtk.ListBoxRow
  [ classes
      (  (mwhen (pkg ^. npInstalled) ["package-row-installed"])
      <> [if i `mod` 2 == 0 then "package-row-even" else "package-row-odd"]
      )
  ]
  (widget
    Gtk.Label
    [ #useMarkup := True
    , #label := formatPkgLabel pkg
    , #halign := Gtk.AlignStart
    ]
  )

rowSelectionHandler :: Maybe Gtk.ListBoxRow -> Gtk.ListBox -> IO ManagerEvent
rowSelectionHandler (Just row) _ = do
  selectedIndex <- Gtk.listBoxRowGetIndex row
  if selectedIndex == -1
    then pure (ManagerEventPackages (EventPackageSelected Nothing))
    else pure
      (ManagerEventPackages
        (EventPackageSelected (Just (fromIntegral selectedIndex)))
      )
rowSelectionHandler _ _ =
  pure (ManagerEventPackages (EventPackageSelected Nothing))

packagesBox
  :: FromWidget (Container Gtk.Box (Children BoxChild)) target
  => ManagerState
  -> target ManagerEvent
packagesBox s =
  let
    searchValid = (s ^. msPackagesState . psSearchString . to length) >= 2
    resultRows  = Vector.fromList
      (zipWith buildResultRow
               [0 ..]
               (s ^.. msPackagesState . psSearchResult . folded)
      )
    packageSelected         = isJust (s ^. msPackagesState . psSelectedPackage)
    currentPackageInstalled = getAny
      (s ^. msPackagesState . psSelectedPackage . folded . npInstalled . to Any)
    tryInstallCell = case s ^. msPackagesState . psInstallingPackage of
      Nothing -> BoxChild
        (defaultBoxChildProperties { expand = True, fill = True })
        (widget
          Gtk.Button
          [ #label := "Try without installing"
          , #sensitive := (packageSelected && not currentPackageInstalled)
          , classes ["try-install-button"]
          , on #clicked (ManagerEventPackages EventTryInstall)
          ]
        )
      Just is ->
        BoxChild (defaultBoxChildProperties { expand = True, fill = True })
          $ container
              Gtk.Box
              [#orientation := Gtk.OrientationHorizontal, #spacing := 5]
              [ BoxChild defaultBoxChildProperties $ widget
                Gtk.Button
                [ #label := "gtk-cancel"
                , #useStock := True
                , #alwaysShowImage := True
                , on #clicked (ManagerEventPackages EventTryInstallCancel)
                ]
              , BoxChild
                  (defaultBoxChildProperties { fill = True, expand = True })
                $ progressBar [#showText := True, #text := "Downloading..."]
                              (is ^. isCounter)
              ]
    packageButtonRow = container
      Gtk.Box
      [#orientation := Gtk.OrientationHorizontal, #spacing := 10]
      [ tryInstallCell
      , BoxChild
        (defaultBoxChildProperties { expand = True, fill = True })
        (widget
          Gtk.Button
          [ #label := "Install"
          , #sensitive := (packageSelected && not currentPackageInstalled)
          , classes ["install-button"]
          , on #clicked (ManagerEventPackages EventInstall)
          ]
        )
      , BoxChild
        (defaultBoxChildProperties { expand = True, fill = True })
        (widget
          Gtk.Button
          [ #label := "Remove"
          , #sensitive := (packageSelected && currentPackageInstalled)
          , classes ["remove-button"]
          , on #clicked (ManagerEventPackages EventUninstall)
          ]
        )
      ]
    resultBox = if searchValid
      then bin
        Gtk.ScrolledWindow
        []
        (container
          Gtk.ListBox
          [onM #rowSelected rowSelectionHandler, classes ["packages-list"]]
          resultRows
        )
      else widget
        Gtk.Label
        [ #label := "Please enter a search term with at least two characters"
        , #expand := True
        ]
  in
    paddedAround 10 $ container
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
                   , #useMarkup := True
                   , classes
                     [ if has (messageType . _ErrorMessage) e
                         then "error-message"
                         else "info-message"
                     ]
                   ]
                 )
             ]
           )
           (s ^. msPackagesState . psLatestMessage)

      <> [ packageButtonRow
         , widget Gtk.HSeparator []
         , BoxChild
           (defaultBoxChildProperties { expand = True, fill = True })
           resultBox
         ]
      )

