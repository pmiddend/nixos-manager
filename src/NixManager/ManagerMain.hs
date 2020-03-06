{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.ManagerMain where

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
import           Data.GI.Base                   ( new
                                                , on
                                                , AttrOp((:=))
                                                )
import           System.Environment             ( getArgs )
import           Data.Text                      ( pack
                                                , isInfixOf
                                                , length
                                                , Text
                                                , unpack
                                                )
import           Prelude                 hiding ( length )

createRow t = do
  rowBox      <- Gtk.boxNew Gtk.OrientationHorizontal 0
  rowBoxLabel <- Gtk.labelNew (Just t)
  #add rowBox rowBoxLabel
  resultingRow <- Gtk.listBoxRowNew
  #add resultingRow rowBox
  pure resultingRow

createSearchBox changeHandler = do
  searchLabel <- Gtk.labelNew (Just "Search:")
  searchBox   <- Gtk.boxNew Gtk.OrientationHorizontal 4
  Gtk.boxPackStart searchBox searchLabel False False 6
  searchInput <- Gtk.entryNew
  void $ on searchInput #changed $ do
    currentText <- Gtk.entryGetText searchInput
    changeHandler currentText
  Gtk.boxPackStart searchBox searchInput False False 6
  pure searchBox

createPackageList packageGetter = do
  packageList <- Gtk.listBoxNew
  packages    <- packageGetter
  forM_ packages $ \i -> do
    r <- createRow i
    #add packageList r
  pure packageList

createPackages packageGetter searchChangeHandler = do
  searchBox   <- createSearchBox searchChangeHandler
  packageList <- createPackageList packageGetter
  packagesBox <- Gtk.boxNew Gtk.OrientationVertical 6
  #add packagesBox searchBox
  #add packagesBox packageList
  packagesScrolled <- new Gtk.ScrolledWindow []
  #add packagesScrolled packagesBox
  pure packagesScrolled

type Endo a = a -> a

data AppState = AppState {
  _appStateSearchText :: Text
  , _appStatePackages :: [NixPackage]
  }

type AppStateRef = IORef AppState

createAppState :: AppState -> IO AppStateRef
createAppState as = newIORef as

readAppState :: AppStateRef -> IO AppState
readAppState = readIORef

modifyAppState :: AppStateRef -> Endo AppState -> IO ()
modifyAppState = modifyIORef

data NixPackage = NixPackage {
    _npName :: Text
  , _npVersion :: Text
  , _npDescription :: Text
  }

instance FromJSON NixPackage where
  parseJSON (Object v) =
    NixPackage <$> v .: "pkgName" <*> v .: "version" <*> v .: "description"
  parseJSON _ = mzero

decodeNixSearchResult :: ByteString -> Either String (Map Text NixPackage)
decodeNixSearchResult = eitherDecode

nixSearch :: Text -> IO (Either String [NixPackage])
nixSearch t = do
  (_, Just hout, _, _) <- createProcess
    (proc "nix" ["search", unpack t, "--json"]) { std_out = CreatePipe }
  out <- hGetContents hout
  pure (elems <$> decodeNixSearchResult out)

nixSearchUnsafe :: Text -> IO [NixPackage]
nixSearchUnsafe t = do
  result <- nixSearch t
  case result of
    Left  e -> error e
    Right v -> pure v

nixMain :: IO ()
nixMain = do
  putStrLn "Starting..."
  sr <- nixSearch "kde"
  case sr of
    Left  e -> error e
    Right v -> mapM_ (putStrLn . unpack . _npName) v
  argv             <- getArgs
  (initSuccess, _) <- Gtk.initCheck (Just (pack <$> argv))
  if initSuccess
    then do
      win          <- new Gtk.Window [#title := "nix-manager"]
      _            <- on win #destroy Gtk.mainQuit
      mainNotebook <- Gtk.notebookNew
      #add win mainNotebook
      packages <- nixSearchUnsafe ""
      appState <- createAppState (AppState "" packages)
      let packageGetter = do
            as <- readAppState appState
            if length (_appStateSearchText as) < 2
              then pure []
              else do
                let matchingPackages = filter
                      (((_appStateSearchText as `isInfixOf`)) . _npName)
                      (_appStatePackages as)
                pure ((_npName) <$> matchingPackages)
          searchChangeHandler t =
            modifyAppState appState (\as -> as { _appStateSearchText = t })
      packagesScrolled <- createPackages packageGetter searchChangeHandler
      packagesLabel    <- Gtk.labelNew (Just "Packages")
      iRun             <- Gtk.notebookAppendPage mainNotebook
                                                 packagesScrolled
                                                 (Just packagesLabel)
      #showAll win
      Gtk.main
    else putStrLn "Init failed"
