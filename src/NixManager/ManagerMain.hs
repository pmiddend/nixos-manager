{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.ManagerMain where

import           Data.Vector.Lens               ( toVectorOf )
import           Control.Lens                   ( (^.)
                                                , to
                                                , (^..)
                                                , (&)
                                                , (.~)
                                                , folded
                                                , filtered
                                                )
import           NixManager.ManagerState
import           NixManager.Nix
import           GI.Gtk.Declarative             ( bin
                                                , padding
                                                , defaultBoxChildProperties
                                                , expand
                                                , fill
                                                , widget
                                                , Attribute((:=))
                                                , container
                                                , BoxChild(BoxChild)
                                                , on
                                                , onM
                                                )
import           GI.Gtk.Declarative.App.Simple  ( AppView
                                                , App(App)
                                                , view
                                                , update
                                                , Transition(Transition, Exit)
                                                , inputs
                                                , initialState
                                                , run
                                                )
import           System.Process                 ( createProcess
                                                , std_out
                                                , proc
                                                , StdStream(CreatePipe)
                                                )
import           Data.Map.Strict                ( Map
                                                , elems
                                                )
import           Data.ByteString.Lazy           ( ByteString
                                                , hGetContents
                                                )
import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , Value(Object)
                                                , (.:)
                                                , eitherDecode
                                                )
import           Data.IORef                     ( IORef
                                                , newIORef
                                                , readIORef
                                                , modifyIORef
                                                )
import           Control.Monad                  ( forM_
                                                , mzero
                                                , void
                                                )
import qualified GI.Gtk                        as Gtk
import           Data.GI.Base                   ( new )
import           System.Environment             ( getArgs )
import           Data.Text                      ( pack
                                                , toLower
                                                , isInfixOf
                                                , length
                                                , Text
                                                , unpack
                                                )
import           Prelude                 hiding ( length )

-- createRow t = do
--   rowBox      <- Gtk.boxNew Gtk.OrientationHorizontal 0
--   rowBoxLabel <- Gtk.labelNew (Just t)
--   #add rowBox rowBoxLabel
--   resultingRow <- Gtk.listBoxRowNew
--   #add resultingRow rowBox
--   pure resultingRow

-- createSearchBox changeHandler = do
--   searchLabel <- Gtk.labelNew (Just "Search:")
--   searchBox   <- Gtk.boxNew Gtk.OrientationHorizontal 4
--   Gtk.boxPackStart searchBox searchLabel False False 6
--   searchInput <- Gtk.entryNew
--   void $ on searchInput #changed $ do
--     currentText <- Gtk.entryGetText searchInput
--     changeHandler currentText
--   Gtk.boxPackStart searchBox searchInput False False 6
--   pure searchBox

-- createPackageList packageGetter = do
--   packageList <- Gtk.listBoxNew
--   packages    <- packageGetter
--   forM_ packages $ \i -> do
--     r <- createRow i
--     #add packageList r
--   pure packageList

-- createPackages packageGetter searchChangeHandler = do
--   searchBox   <- createSearchBox searchChangeHandler
--   packageList <- createPackageList packageGetter
--   packagesBox <- Gtk.boxNew Gtk.OrientationVertical 6
--   #add packagesBox searchBox
--   #add packagesBox packageList
--   packagesScrolled <- new Gtk.ScrolledWindow []
--   #add packagesScrolled packagesBox
--   pure packagesScrolled

type Endo a = a -> a

data AppState = AppState {
  _appStateSearchText :: Text
  , _appStatePackages :: [NixPackage]
  }

type AppStateRef = IORef AppState

createAppState :: AppState -> IO AppStateRef
createAppState = newIORef

readAppState :: AppStateRef -> IO AppState
readAppState = readIORef

modifyAppState :: AppStateRef -> Endo AppState -> IO ()
modifyAppState = modifyIORef

data ManagerEvent = ManagerEventClosed | ManagerEventSearchChanged Text

packageMatches :: Text -> NixPackage -> Bool
packageMatches t p = toLower t `isInfixOf` (p ^. npName . to toLower)

mwhen :: Monoid m => Bool -> m -> m
mwhen True  v = v
mwhen False _ = mempty

view' :: ManagerState -> AppView Gtk.Window ManagerEvent
view' s =
  let buildResultRow pkg =
          bin Gtk.ListBoxRow [] (widget Gtk.Label [#label := (pkg ^. npName)])
      searchLabel = widget Gtk.Label [#label := "Search in packages:"]
      processSearchChange w = ManagerEventSearchChanged <$> Gtk.getEntryText w
      searchField = widget
        Gtk.SearchEntry
        [ #placeholderText := "Enter a package name or part of a name..."
        , #maxWidthChars := 50
        , onM #searchChanged processSearchChange
        ]
      searchBox = container
        Gtk.Box
        [#orientation := Gtk.OrientationHorizontal, #spacing := 10]
        [ searchLabel
        , BoxChild (defaultBoxChildProperties { fill = True }) searchField
        ]
      searchValid = (s ^. msSearchString . to length) >= 2
      resultRows  = mwhen searchValid $ toVectorOf
        ( msPackageCache
        . folded
        . filtered (packageMatches (s ^. msSearchString))
        . to buildResultRow
        )
        s
      resultBox = if searchValid
        then container Gtk.ListBox [] resultRows
        else widget
          Gtk.Label
          [ #label := "Please enter a search term with at least two characters"
          , #expand := True
          ]
  in  bin
          Gtk.Window
          [ #title := "nix-manager 1.0"
          , on #deleteEvent (const (True, ManagerEventClosed))
          ]
        $ container Gtk.Box
                    [#orientation := Gtk.OrientationVertical, #spacing := 10]
                    [searchBox, widget Gtk.HSeparator [], resultBox]

update' :: ManagerState -> ManagerEvent -> Transition ManagerState ManagerEvent
update' _ ManagerEventClosed = Exit
update' s (ManagerEventSearchChanged t) =
  Transition (s & msSearchString .~ t) (pure Nothing)

nixMain :: IO ()
nixMain = do
  putStrLn "Reading cache..."
  cache <- nixSearchUnsafe ""
  putStrLn "Starting..."
  void $ run App
    { view         = view'
    , update       = update'
    , inputs       = []
    , initialState = ManagerState { _msPackageCache = cache
                                  , _msSearchString = ""
                                  }
    }
  -- sr <- nixSearch "kde"
  -- case sr of
  --   Left  e -> error e
  --   Right v -> mapM_ (putStrLn . unpack . _npName) v
  -- argv             <- getArgs
  -- (initSuccess, _) <- Gtk.initCheck (Just (pack <$> argv))
  -- if initSuccess
  --   then do
  --     win          <- new Gtk.Window [#title := "nix-manager"]
  --     _            <- on win #destroy Gtk.mainQuit
  --     mainNotebook <- Gtk.notebookNew
  --     #add win mainNotebook
  --     packages <- nixSearchUnsafe ""
  --     appState <- createAppState (AppState "" packages)
  --     let packageGetter = do
  --           as <- readAppState appState
  --           if length (_appStateSearchText as) < 2
  --             then pure []
  --             else do
  --               let matchingPackages = filter
  --                     (((_appStateSearchText as `isInfixOf`)) . _npName)
  --                     (_appStatePackages as)
  --               pure ((_npName) <$> matchingPackages)
  --         searchChangeHandler t =
  --           modifyAppState appState (\as -> as { _appStateSearchText = t })
  --     packagesScrolled <- createPackages packageGetter searchChangeHandler
  --     packagesLabel    <- Gtk.labelNew (Just "Packages")
  --     iRun             <- Gtk.notebookAppendPage mainNotebook
  --                                                packagesScrolled
  --                                                (Just packagesLabel)
  --     #showAll win
  --     Gtk.main
  --   else putStrLn "Init failed"
