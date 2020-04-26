{-|
  Description: Contains the actual GUI (widgets) for the package edit tabs
Contains the actual GUI (widgets) for the package edit tabs
  -}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.View.PackageEditView
  ( updateEvent
  , packagesBox
  , State(..)
  , InstallationType(..)
  , emptyState
  , selectedPackage
  , CompletionType(..)
  , Event(..)
  , InstallingState(..)
  , initState
  )
where

import           GHC.Generics                   ( Generic )
import           NixManager.Process             ( updateProcess )
import           Data.Monoid                    ( getFirst )
import           NixManager.Message             ( errorMessage
                                                , infoMessage
                                                , messageWidget
                                                , Message
                                                )
import           System.Exit                    ( ExitCode
                                                  ( ExitSuccess
                                                  , ExitFailure
                                                  )
                                                )
import           NixManager.NixPackagesUtil     ( dryInstall
                                                , executablesFromStorePath
                                                , startProgram
                                                )
import           GI.Gtk.Declarative.App.Simple  ( Transition(Transition) )
import           NixManager.Process             ( ProcessData
                                                , ProcessOutput
                                                )
import           NixManager.NixLocation         ( flattenedTail )
import           NixManager.PackageCategory     ( packageCategories
                                                , categoryToText
                                                , PackageCategory
                                                  ( PackageCategoryAll
                                                  , PackageCategoryInstalled
                                                  , PackageCategoryPendingInstall
                                                  , PackageCategoryPendingUninstall
                                                  )
                                                )
import qualified NixManager.PackageCategory    as PC
import           NixManager.View.ComboBox       ( comboBox
                                                , ComboBoxProperties
                                                  ( ComboBoxProperties
                                                  )
                                                , ComboBoxChangeEvent
                                                  ( ComboBoxChangeEvent
                                                  )
                                                )
import           NixManager.View.ProgressBar    ( progressBar )
import qualified NixManager.View.IconName      as IconName
import           NixManager.NixPackageStatus    ( NixPackageStatus
                                                  ( NixPackageNothing
                                                  , NixPackageInstalled
                                                  , NixPackagePendingInstall
                                                  , NixPackagePendingUninstall
                                                  )
                                                )
import           NixManager.NixPackage          ( NixPackage )
import           Prelude                 hiding ( length
                                                , null
                                                , unlines
                                                , init
                                                , putStrLn
                                                )
import           Data.Text                      ( length
                                                , Text
                                                , toLower
                                                , isInfixOf
                                                , init
                                                , unlines
                                                , null
                                                )
import           Data.Text.IO                   ( putStrLn )
import           GI.Gtk.Declarative.Container.Class
                                                ( Children )
import           Data.Default                   ( def )
import           GI.Gtk.Declarative             ( bin
                                                , Widget
                                                , padding
                                                , Container
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
import           System.FilePath                ( (</>) )
import           Control.Lens                   ( (^.)
                                                , Lens'
                                                , Traversal'
                                                , (^?)
                                                , (?~)
                                                , (+~)
                                                , (^?!)
                                                , traversed
                                                , (.~)
                                                , filtered
                                                , (&)
                                                , (^..)
                                                , from
                                                , to
                                                , has
                                                , folded
                                                )
import           NixManager.Util                ( replaceHtmlEntities
                                                , indirectIndexTraversal
                                                , threadDelayMillis
                                                , decodeUtf8
                                                , showText
                                                , surroundSimple
                                                )
import           NixManager.View.GtkUtil        ( paddedAround
                                                , expandAndFill
                                                )
import           NixManager.View.ImageButton    ( imageButton )
import           Control.Monad.IO.Class         ( MonadIO )


-- | (Boolean) enum signifying if, after we made some change to the packages (say we marked a package for installation), it’s time to re-evaluate if we have changes to apply. If we mark a package for installation, we, of course, have changes to apply. A case for the opposite is marking a package for installation and immediately unmarking it again.
data CompletionType = CompletionReload | CompletionPass

-- | Here, two concepts clash a bit. We have the operation “Mark for installation”, which actually stands for two things, depending on the current state of a package. If the package is currently marked for uninstallation, installing it actually just cancels the uninstallation. If it’s not marked for uninstallation, installing it, well, marks it for installation. Depending on which of the two we wanted to do when we pressed the corresponding button, this enum is passed to certain events.
data InstallationType = Cancelled | Uncancelled

data Event = EventSearchChanged Text -- ^ Triggered whenever the search entry changes
           | EventPackageSelected (Maybe Int) -- ^ Triggered whenever the currently selected package changes
           | EventTryInstallStarted NixPackage ProcessData -- ^ Triggered just after the trial installation has started. This initializes the state and goes on to watch the installation process using the 'EventTryInstallWatch' event.
           | EventTryInstallFailed Message -- ^ Triggered when the trial installation failed for some reason.
           | EventTryInstallSuccess -- ^ Triggered when the trial installation succeeded
           | EventTryInstallWatch ProcessData ProcessOutput -- ^ Triggered regularly while the trial installation is running. We cumulate the process output and keep the process data.
           | EventInstall InstallationType  -- ^ Triggered when the user clicks on the installation button (the passed enum determines what to do and what to show when the operation completes)
           | EventUninstall InstallationType  -- ^ Triggered when the user clicks on the uninstallation button (the passed enum determines what to do and what to show when the operation completes)
           | EventTryInstall -- ^ Triggered when the user presses the “Try install” button. The next event will be 'EventTryInstallStarted'
           | EventTryInstallCancel -- ^ Triggered when the user presses the Cancel button on a trial installation
           | EventCategoryChanged Int -- ^ Triggered when the category combobox changes


-- | This is only used when the “Try install” operation is in progress and cumulates all the state pertaining to that
data InstallingState = InstallingState {
    package :: NixPackage -- ^ Which package is being try-installed
  , counter :: Int  -- ^ This field is necessary to “pulse” the GTK progress bar while installing, see "NixManager.View.ProgressBar" for details
  , processData :: ProcessData -- ^ The process data
  } deriving(Generic)

data State = State {
    packageCache :: [NixPackage] -- ^ Cache of all available Nix packages
  , searchString :: Text -- ^ Current search string
  , selectedIdx :: Maybe Int -- ^ Currently selected index
  , installingPackage :: Maybe InstallingState -- ^ Only set if “Try install” is in progress
  , latestMessage :: Maybe Message -- ^ The latest message to display, if any (“Install successful” and stuff)
  , categoryIdx :: Int -- ^ Currently selected category
  } deriving(Generic)

emptyState = State { packageCache      = mempty
                   , searchString      = mempty
                   , selectedIdx       = Nothing
                   , installingPackage = Nothing
                   , latestMessage     = Nothing
                   , categoryIdx       = 0
                   }

initState :: [NixPackage] -> State
initState cache = State { packageCache      = cache
                        , searchString      = mempty
                        , selectedIdx       = Nothing
                        , installingPackage = Nothing
                        , latestMessage     = Nothing
                        , categoryIdx       = 0
                        }

-- | Isomorphism between a category and its index in the list of all categories (needed for the combobox logic) 
category :: Lens' State PackageCategory
category = #categoryIdx . from PC.categoryIdx

-- | Whether a package matches the given search string
packageMatches :: Text -> NixPackage -> Bool
packageMatches t p = toLower t `isInfixOf` (p ^. #name . to toLower)

-- | Whether a package matches the given category
packageMatchesCategory :: PackageCategory -> NixPackage -> Bool
packageMatchesCategory PackageCategoryAll _ = True
packageMatchesCategory PackageCategoryInstalled pkg =
  (pkg ^. #status) == NixPackageInstalled
packageMatchesCategory PackageCategoryPendingInstall pkg =
  (pkg ^. #status) == NixPackagePendingInstall
packageMatchesCategory PackageCategoryPendingUninstall pkg =
  (pkg ^. #status) == NixPackagePendingUninstall

-- | Getter for the filtered search result
searchResult :: Traversal' State NixPackage
searchResult f s =
  ( #packageCache
    . traversed
    . filtered (packageMatches (s ^. #searchString))
    . filtered (packageMatchesCategory (s ^. category))
    )
    f
    s

-- | Getter for the selected package (if any)
selectedPackage :: Traversal' State NixPackage
selectedPackage = indirectIndexTraversal #selectedIdx searchResult

-- | Callback for the search entry widget
processSearchChange
  :: (IsDescendantOf Gtk.Entry o, MonadIO f, Gtk.GObject o) => o -> f Event
processSearchChange w = EventSearchChanged <$> Gtk.getEntryText w

-- | Label for the search entry
searchLabel :: Widget event
searchLabel =
  widget Gtk.Label [#label := "Search in packages:", #halign := Gtk.AlignEnd]

-- | The actual search entry widget
searchField :: Widget Event
searchField = widget
  Gtk.SearchEntry
  [ #placeholderText := "Enter a package name or part of a name..."
  , #maxWidthChars := 50
  , onM #searchChanged processSearchChange
  , #halign := Gtk.AlignFill
  ]

-- | The box containing the search field, label, and combobox
searchBox :: State -> Widget Event
searchBox s =
  let changeCallback (ComboBoxChangeEvent idx) =
          EventCategoryChanged (fromIntegral idx)
  in  container
        Gtk.Box
        [#orientation := Gtk.OrientationHorizontal, #spacing := 10]
        [ BoxChild expandAndFill searchLabel
        , BoxChild expandAndFill searchField
        , BoxChild def $ changeCallback <$> comboBox
          []
          (ComboBoxProperties (categoryToText <$> packageCategories)
                              (s ^. #categoryIdx)
          )
        ]

-- | Given a package, format its corresponding list box row
formatPkgLabel :: NixPackage -> Text
formatPkgLabel pkg =
  let
    path      = pkg ^. #path . flattenedTail
    name      = pkg ^. #name
    firstLine = ["<span size=\"x-large\">" <> name <> "</span>"]
    descriptionLine
      | null (pkg ^. #description)
      = []
      | otherwise
      = [ "Description: " <> pkg ^. #description . to
            (surroundSimple "i" . replaceHtmlEntities)
        ]
    pathLine = [ "Full Nix path: " <> surroundSimple "tt" path | path /= name ]
    formatStatus NixPackageNothing          = []
    formatStatus NixPackageInstalled        = ["Installed"]
    formatStatus NixPackagePendingInstall   = ["Marked for installation"]
    formatStatus NixPackagePendingUninstall = ["Marked for uninstallation"]
  in
    init
    . unlines
    $ (  firstLine
      <> pathLine
      <> descriptionLine
      <> (surroundSimple "b" <$> formatStatus (pkg ^. #status))
      )


-- | Create a list box row widget from a package (the first argument is the index, used for alternatively coloring rows)
buildResultRow
  :: FromWidget (Bin Gtk.ListBoxRow) target => Int -> NixPackage -> target event
buildResultRow i pkg = bin
  Gtk.ListBoxRow
  [classes [if i `mod` 2 == 0 then "package-row-even" else "package-row-odd"]]
  (widget
    Gtk.Label
    [ #useMarkup := True
    , #label := formatPkgLabel pkg
    , #halign := Gtk.AlignStart
    ]
  )

-- | Handles a row selection event
rowSelectionHandler :: Maybe Gtk.ListBoxRow -> Gtk.ListBox -> IO Event
rowSelectionHandler (Just row) _ = do
  selectedIndex <- Gtk.listBoxRowGetIndex row
  if selectedIndex == -1
    then pure (EventPackageSelected Nothing)
    else pure (EventPackageSelected (Just (fromIntegral selectedIndex)))

rowSelectionHandler _ _ = pure (EventPackageSelected Nothing)

-- | The package list
packagesBox
  :: FromWidget (Container Gtk.Box (Children BoxChild)) target
  => State
  -> target Event
packagesBox s =
  let
    searchValid =
      ((s ^. category) /= PackageCategoryAll)
        || (s ^. #searchString . to length)
        >= 2
    resultRows =
      Vector.fromList (zipWith buildResultRow [0 ..] (s ^.. searchResult))
    packageSelected = has selectedPackage s
    currentPackageStatus :: Traversal' State NixPackageStatus
    currentPackageStatus = selectedPackage . #status
    currentPackageInstalled =
      has (currentPackageStatus . #_NixPackageInstalled) s
    currentPackagePendingInstall =
      has (currentPackageStatus . #_NixPackagePendingInstall) s
    currentPackagePendingUninstall =
      has (currentPackageStatus . #_NixPackagePendingUninstall) s
    tryInstallCell = case s ^. #installingPackage of
      Nothing -> BoxChild
        expandAndFill
        (imageButton
          [ #label := "Try without installing"
          , #sensitive
            := (  packageSelected
               && not currentPackageInstalled
               && not currentPackagePendingUninstall
               )
          , classes ["try-install-button"]
          , on #clicked EventTryInstall
          , #alwaysShowImage := True
          ]
          IconName.SystemRun
        )
      Just is -> BoxChild expandAndFill $ container
        Gtk.Box
        [#orientation := Gtk.OrientationHorizontal, #spacing := 5]
        [ BoxChild def $ imageButton
          [ #alwaysShowImage := True
          , on #clicked EventTryInstallCancel
          , #label := "Cancel"
          ]
          IconName.ProcessStop
        , BoxChild expandAndFill $ progressBar
          [#showText := True, #text := "Downloading..."]
          (is ^. #counter)
        ]
    packageButtonRow = container
      Gtk.Box
      [#orientation := Gtk.OrientationHorizontal, #spacing := 10]
      [ tryInstallCell
      , BoxChild
        expandAndFill
        (imageButton
          [ #label
            := (if currentPackagePendingUninstall
                 then "Cancel uninstall"
                 else "Mark for installation"
               )
          , #sensitive
            := (  packageSelected
               && not currentPackageInstalled
               && not currentPackagePendingInstall
               )
          , classes ["install-button"]
          , on
            #clicked
            (if currentPackagePendingUninstall
              then EventInstall Cancelled
              else EventInstall Uncancelled
            )
          , #alwaysShowImage := True
          ]
          IconName.SystemSoftwareInstall
        )
      , BoxChild
        expandAndFill
        (imageButton
          [ #label
            := (if currentPackageInstalled
                 then "Mark for removal"
                 else "Cancel installation"
               )
          , #sensitive
            := (  packageSelected
               && (currentPackageInstalled || currentPackagePendingInstall)
               )
          , classes ["remove-button"]
          , #alwaysShowImage := True
          , on
            #clicked
            (if currentPackageInstalled
              then EventUninstall Uncancelled
              else EventUninstall Cancelled
            )
          ]
          IconName.EditDelete
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
      ([BoxChild (def { padding = 5 }) (searchBox s), widget Gtk.HSeparator []]
      <> foldMap (\e -> [BoxChild def (messageWidget e)])
                 (s ^. #latestMessage)

      <> [ packageButtonRow
         , widget Gtk.HSeparator []
         , BoxChild expandAndFill resultBox
         ]
      )

pureTransition s = Transition s (pure Nothing)

-- | The actual update function
updateEvent :: State -> Event -> Transition State Event
updateEvent s (EventCategoryChanged newCategory) =
  pureTransition (s & #categoryIdx .~ newCategory)
updateEvent s (EventPackageSelected i) = pureTransition (s & #selectedIdx .~ i)
updateEvent s (EventSearchChanged t) =
  pureTransition (s & #searchString .~ t & #selectedIdx .~ Nothing)
updateEvent s EventTryInstall = case s ^? selectedPackage of
  Nothing       -> pureTransition s
  Just selected -> Transition
    s
    do
      pd <- dryInstall selected
      pure (Just (EventTryInstallStarted selected pd))
updateEvent s (EventTryInstallStarted pkg pd) = Transition
  (s & #installingPackage ?~ InstallingState pkg 0 pd)
  (pure (Just (EventTryInstallWatch pd mempty)))
updateEvent s (EventTryInstallFailed e) =
  pureTransition (s & #latestMessage ?~ e & #installingPackage .~ Nothing)
updateEvent s EventTryInstallSuccess = pureTransition
  (  s
  &  #latestMessage
  ?~ infoMessage
       "Downloaded and started the application!\nIf nothing happens, it's probably a terminal application and cannot be started from NixOS manager."
  &  #installingPackage
  .~ Nothing
  )
updateEvent s (EventTryInstallWatch pd po) = Transition
  (s & #installingPackage . traversed . #counter +~ 1)
  do
        -- See the readme about an explanation of why we do this “watch” event stuff
    newOutput <- (po <>) <$> updateProcess pd
    case newOutput ^. #result . to getFirst of
      Nothing -> do
        threadDelayMillis 500
        pure (Just (EventTryInstallWatch pd newOutput))
      Just ExitSuccess ->
        executablesFromStorePath
            (s ^?! #installingPackage . folded . #package)
            (newOutput ^. #stdout)
          >>= \case
                (_, []) -> pure
                  (Just
                    (EventTryInstallFailed
                      (errorMessage "No binaries found in this package!")
                    )
                  )

                (bp, [singleBinary]) -> do
                  startProgram (bp </> singleBinary)
                  pure (Just EventTryInstallSuccess)
                multipleBinaries -> do
                  putStrLn $ "found more bins: " <> showText multipleBinaries
                  pure
                    (Just
                      (EventTryInstallFailed
                        (errorMessage "Multiple binaries found in this package!"
                        )
                      )
                    )

      Just (ExitFailure code) -> pure
        (Just
          (EventTryInstallFailed
            (errorMessage
              (  "Installing failed, exit code: "
              <> showText code
              <> ", standard error:\n<tt>"
              <> replaceHtmlEntities (newOutput ^. #stderr . decodeUtf8)
              <> "</tt>"
              )
            )
          )
        )
updateEvent s _ = pureTransition s
