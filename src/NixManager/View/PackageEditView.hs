{-|
  Description: Contains the actual GUI (widgets) for the package edit tabs
Contains the actual GUI (widgets) for the package edit tabs
  -}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.View.PackageEditView
  ( updateEvent
  , packagesBox
  , State(..)
  , InstallationType(..)
  , emptyState
  , CompletionType(..)
  , Event(..)
  , InstallingState(..)
  , initState
  , psSearchString
  , psSearchResult
  , psSelectedPackage
  , psLatestMessage
  , isPackage
  , isCounter
  , isProcessData
  , psInstallingPackage
  , psPackageCache
  , psCategory
  , psSelectedIdx
  , psCategoryIdx
  )
where


import           NixManager.Process             ( updateProcess
                                                , poResult
                                                , poStderr
                                                , poStdout
                                                )
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
                                                , categoryIdx
                                                , PackageCategory
                                                  ( PackageCategoryAll
                                                  , PackageCategoryInstalled
                                                  , PackageCategoryPendingInstall
                                                  , PackageCategoryPendingUninstall
                                                  )
                                                )
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
import           NixManager.NixPackageStatus    ( _NixPackageInstalled
                                                , _NixPackagePendingInstall
                                                , _NixPackagePendingUninstall
                                                , NixPackageStatus
                                                  ( NixPackageNothing
                                                  , NixPackageInstalled
                                                  , NixPackagePendingInstall
                                                  , NixPackagePendingUninstall
                                                  )
                                                )
import           NixManager.NixPackage          ( NixPackage
                                                , npStatus
                                                , npName
                                                , npPath
                                                , npDescription
                                                )
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
                                                , Traversal'
                                                , makeLenses
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
                                                , Lens'
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
    _isPackage :: NixPackage -- ^ Which package is being try-installed
  , _isCounter :: Int  -- ^ This field is necessary to “pulse” the GTK progress bar while installing, see "NixManager.View.ProgressBar" for details
  , _isProcessData :: ProcessData -- ^ The process data
  }

makeLenses ''InstallingState

data State = State {
    _psPackageCache :: [NixPackage] -- ^ Cache of all available Nix packages
  , _psSearchString :: Text -- ^ Current search string
  , _psSelectedIdx :: Maybe Int -- ^ Currently selected index
  , _psInstallingPackage :: Maybe InstallingState -- ^ Only set if “Try install” is in progress
  , _psLatestMessage :: Maybe Message -- ^ The latest message to display, if any (“Install successful” and stuff)
  , _psCategoryIdx :: Int -- ^ Currently selected category
  }

makeLenses ''State

emptyState = State { _psPackageCache      = mempty
                   , _psSearchString      = mempty
                   , _psSelectedIdx       = Nothing
                   , _psInstallingPackage = Nothing
                   , _psLatestMessage     = Nothing
                   , _psCategoryIdx       = 0
                   }

initState :: [NixPackage] -> State
initState cache = State { _psPackageCache      = cache
                        , _psSearchString      = mempty
                        , _psSelectedIdx       = Nothing
                        , _psInstallingPackage = Nothing
                        , _psLatestMessage     = Nothing
                        , _psCategoryIdx       = 0
                        }

-- | Isomorphism between a category and its index in the list of all categories (needed for the combobox logic) 
psCategory :: Lens' State PackageCategory
psCategory = psCategoryIdx . from categoryIdx

-- | Whether a package matches the given search string
packageMatches :: Text -> NixPackage -> Bool
packageMatches t p = toLower t `isInfixOf` (p ^. npName . to toLower)

-- | Whether a package matches the given category
packageMatchesCategory :: PackageCategory -> NixPackage -> Bool
packageMatchesCategory PackageCategoryAll _ = True
packageMatchesCategory PackageCategoryInstalled pkg =
  (pkg ^. npStatus) == NixPackageInstalled
packageMatchesCategory PackageCategoryPendingInstall pkg =
  (pkg ^. npStatus) == NixPackagePendingInstall
packageMatchesCategory PackageCategoryPendingUninstall pkg =
  (pkg ^. npStatus) == NixPackagePendingUninstall

-- | Getter for the filtered search result
psSearchResult :: Traversal' State NixPackage
-- psSearchResult = to
--   (\s ->
--     s
--       ^.. psPackageCache
--       .   folded
--       .   filtered (packageMatches (s ^. psSearchString))
--       .   filtered (packageMatchesCategory (s ^. psCategory))
--   )
psSearchResult f s =
  ( psPackageCache
    . traversed
    . filtered (packageMatches (s ^. psSearchString))
    . filtered (packageMatchesCategory (s ^. psCategory))
    )
    f
    s

-- | Getter for the selected package (if any)
psSelectedPackage :: Traversal' State NixPackage
psSelectedPackage = indirectIndexTraversal psSelectedIdx psSearchResult

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
                              (s ^. psCategoryIdx)
          )
        ]

-- | Given a package, format its corresponding list box row
formatPkgLabel :: NixPackage -> Text
formatPkgLabel pkg =
  let
    path      = pkg ^. npPath . flattenedTail
    name      = pkg ^. npName
    firstLine = ["<span size=\"x-large\">" <> name <> "</span>"]
    descriptionLine
      | null (pkg ^. npDescription)
      = []
      | otherwise
      = [ "Description: " <> pkg ^. npDescription . to
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
      <> (surroundSimple "b" <$> formatStatus (pkg ^. npStatus))
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
      ((s ^. psCategory) /= PackageCategoryAll)
        || (s ^. psSearchString . to length)
        >= 2
    resultRows =
      Vector.fromList (zipWith buildResultRow [0 ..] (s ^.. psSearchResult))
    packageSelected      = has psSelectedPackage s
    currentPackageStatus = psSelectedPackage . npStatus
    currentPackageInstalled =
      has (currentPackageStatus . _NixPackageInstalled) s
    currentPackagePendingInstall =
      has (currentPackageStatus . _NixPackagePendingInstall) s
    currentPackagePendingUninstall =
      has (currentPackageStatus . _NixPackagePendingUninstall) s
    tryInstallCell = case s ^. psInstallingPackage of
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
          (is ^. isCounter)
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
                 (s ^. psLatestMessage)

      <> [ packageButtonRow
         , widget Gtk.HSeparator []
         , BoxChild expandAndFill resultBox
         ]
      )

pureTransition s = Transition s (pure Nothing)

-- | The actual update function
updateEvent :: State -> Event -> Transition State Event
updateEvent s (EventCategoryChanged newCategory) =
  pureTransition (s & psCategoryIdx .~ newCategory)
updateEvent s (EventPackageSelected i) =
  pureTransition (s & psSelectedIdx .~ i)
updateEvent s (EventSearchChanged t) =
  pureTransition (s & psSearchString .~ t & psSelectedIdx .~ Nothing)
updateEvent s EventTryInstall = case s ^? psSelectedPackage of
  Nothing       -> pureTransition s
  Just selected -> Transition
    s
    do
      pd <- dryInstall selected
      pure (Just (EventTryInstallStarted selected pd))
updateEvent s (EventTryInstallStarted pkg pd) = Transition
  (s & psInstallingPackage ?~ InstallingState pkg 0 pd)
  (pure (Just (EventTryInstallWatch pd mempty)))
updateEvent s (EventTryInstallFailed e) =
  pureTransition (s & psLatestMessage ?~ e & psInstallingPackage .~ Nothing)
updateEvent s EventTryInstallSuccess = pureTransition
  (  s
  &  psLatestMessage
  ?~ infoMessage
       "Downloaded and started the application!\nIf nothing happens, it's probably a terminal application and cannot be started from NixOS manager."
  &  psInstallingPackage
  .~ Nothing
  )
updateEvent s (EventTryInstallWatch pd po) = Transition
  (s & psInstallingPackage . traversed . isCounter +~ 1)
  do
        -- See the readme about an explanation of why we do this “watch” event stuff
    newOutput <- (po <>) <$> updateProcess pd
    case newOutput ^. poResult . to getFirst of
      Nothing -> do
        threadDelayMillis 500
        pure (Just (EventTryInstallWatch pd newOutput))
      Just ExitSuccess ->
        executablesFromStorePath
            (s ^?! psInstallingPackage . folded . isPackage)
            (newOutput ^. poStdout)
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
              <> replaceHtmlEntities (newOutput ^. poStderr . decodeUtf8)
              <> "</tt>"
              )
            )
          )
        )
updateEvent s _ = pureTransition s
