{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module NixManager.Nix where

import qualified Data.Set                      as Set
import           Data.Bifunctor                 ( first )
import           Data.Text.IO                   ( putStrLn )
import           Prelude                 hiding ( putStrLn
                                                , readFile
                                                )
import           Data.Fix                       ( Fix(Fix)
                                                , cata
                                                )
import           Data.List                      ( intercalate
                                                , isPrefixOf
                                                , find
                                                , inits
                                                )
import           System.FilePath                ( (</>) )
import           System.Directory               ( listDirectory )
import           Control.Exception              ( catch
                                                , IOException
                                                )
import           Data.Map.Strict                ( Map
                                                , toList
                                                , insertWith
                                                , elems
                                                )
import           Data.ByteString.Lazy           ( ByteString
                                                , hGetContents
                                                , readFile
                                                )
import           Data.ByteString.Lazy.Lens      ( unpackedChars )
import           System.Process                 ( createProcess
                                                , proc
                                                , std_out
                                                , StdStream(CreatePipe)
                                                )
import qualified Data.Text                     as Text
import           Data.Text                      ( Text
                                                , toLower
                                                , strip
                                                )
import           Data.Aeson                     ( FromJSON
                                                , Value(Object)
                                                , parseJSON
                                                , (.:)
                                                , (.:?)
                                                , eitherDecode
                                                )
import           Control.Lens                   ( (^.)
                                                , _Right
                                                , (.~)
                                                , makeLenses
                                                , (^?)
                                                , ix
                                                , Traversal'
                                                , view
                                                , hasn't
                                                , only
                                                , (<>~)
                                                , (&)
                                                , to
                                                , (%~)
                                                )
import           Data.Text.Lens                 ( unpacked
                                                , packed
                                                )
import           Control.Monad                  ( mzero
                                                , void
                                                )
import           NixManager.NixParser
import           NixManager.Util


data NixPackage = NixPackage {
    _npName :: Text
  , _npVersion :: Text
  , _npDescription :: Text
  , _npInstalled :: Bool
  } deriving(Eq,Show)

makeLenses ''NixPackage

instance FromJSON NixPackage where
  parseJSON (Object v) =
    NixPackage
      <$> v
      .:  "pkgName"
      <*> v
      .:  "version"
      <*> v
      .:  "description"
      <*> pure False
  parseJSON _ = mzero

decodeNixSearchResult :: ByteString -> Either String (Map Text NixPackage)
decodeNixSearchResult = eitherDecode

nixSearch :: Text -> IO (Either String [NixPackage])
nixSearch t = do
  (_, Just hout, _, _) <- createProcess
    (proc "nix" ["search", t ^. unpacked, "--json"]) { std_out = CreatePipe }
  out <- hGetContents hout
  pure (elems <$> decodeNixSearchResult out)

nixSearchUnsafe :: Text -> IO [NixPackage]
nixSearchUnsafe t = do
  result <- nixSearch t
  case result of
    Left  e -> error e
    Right v -> pure v

type OptionLocation = [Text]

data NixServiceOption = NixServiceOption {
    _optionDefault :: Maybe Value
  , _optionDescription :: Text
  , _optionLoc :: OptionLocation
  , _optionType :: Text
  } deriving(Show, Eq)

makeLenses ''NixServiceOption

instance FromJSON NixServiceOption where
  parseJSON (Object v) =
    NixServiceOption
      <$> v
      .:? "default"
      <*> v
      .:  "description"
      <*> v
      .:  "loc"
      <*> v
      .:  "type"
  parseJSON _ = mzero


decodeOptions :: ByteString -> Either ErrorMessage (Map Text NixServiceOption)
decodeOptions = first errorMessageFromString . eitherDecode

readOptionsFile
  :: FilePath -> IO (Either ErrorMessage (Map Text NixServiceOption))
readOptionsFile fp = decodeOptions <$> readFile fp

data NixService = NixService {
    _serviceLoc :: OptionLocation
  , _serviceOptions :: [NixServiceOption]
  } deriving(Show, Eq)

makeLenses ''NixService

makeServices :: Map Text NixServiceOption -> [NixService]
makeServices options' =
  let
    options = elems options'
    servicePaths :: Set.Set OptionLocation
    servicePaths = Set.fromList
      (init <$> filter ((== "enable") . last) (view optionLoc <$> options))
    serviceForOption :: NixServiceOption -> Maybe OptionLocation
    serviceForOption opt = case Set.lookupLT (opt ^. optionLoc) servicePaths of
      Nothing -> Nothing
      Just result ->
        if result `isPrefixOf` (opt ^. optionLoc) then Just result else Nothing
    transducer
      :: NixServiceOption -> Endo (Map OptionLocation [NixServiceOption])
    transducer opt m = case serviceForOption opt of
      Nothing          -> m
      Just serviceLoc' -> insertWith (<>) serviceLoc' [opt] m
    serviceMap = foldr transducer mempty options
  in
    uncurry NixService <$> toList serviceMap

matchName :: String -> [FilePath] -> Maybe FilePath
matchName pkgName bins =
  let undashed :: [String]
      undashed = splitRepeat '-' pkgName
      parts :: [String]
      parts = intercalate "-" <$> reverse (inits undashed)
  in  find (`elem` bins) parts

-- TODO: Error handling
getExecutables :: NixPackage -> IO (FilePath, [FilePath])
getExecutables pkg = do
  (_, Just hout, _, _) <- createProcess
    (proc "nix-build"
          ["-A", pkg ^. npName . unpacked, "--no-out-link", "<nixpkgs>"]
    ) { std_out = CreatePipe
      }
  packagePath <- view (unpackedChars . packed . to strip . unpacked)
    <$> hGetContents hout
  let binPath = packagePath </> "bin"
  bins <- listDirectory binPath `catch` \(_ :: IOException) -> pure []
  let normalizedName = pkg ^. npName . to toLower . unpacked
  case matchName normalizedName bins of
    Nothing      -> pure (binPath, bins)
    Just matched -> pure (binPath, [matched])

startProgram :: FilePath -> IO ()
startProgram fn = void $ createProcess (proc fn [])

packageLens :: Traversal' NixExpr NixExpr
packageLens =
  _NixFunctionDecl' . nfExpr . _NixSet' . ix "environment.systemPackages"

parsePackages :: IO (Either Text NixExpr)
parsePackages = parseFile "packages.nix"

writePackages :: NixExpr -> IO ()
writePackages = writeExprFile "packages.nix"

readInstalledPackages :: IO [Text]
readInstalledPackages = do
  expr <- parsePackages
  case expr ^? _Right . packageLens of
    Just packages -> pure (Text.drop 5 <$> cata evalSymbols packages)
    Nothing       -> do
      putStrLn "parse error: couldn't find packages node"
      -- FIXME: Better error handling
      error "parse error"

packagePrefix :: Text
packagePrefix = "pkgs."

installPackage :: Text -> IO (Maybe Text)
installPackage p = do
  expr' <- parsePackages
  case expr' of
    Left  e    -> pure (Just e)
    Right expr -> do
      writePackages
        (   expr
        &   packageLens
        .   _NixList'
        <>~ [Fix (NixSymbol (packagePrefix <> p))]
        )
      pure Nothing

uninstallPackage :: Text -> IO (Maybe Text)
uninstallPackage p = do
  expr' <- parsePackages
  case expr' of
    Left  e    -> pure (Just e)
    Right expr -> do
      writePackages
        (expr & packageLens . _NixList' %~ filter
          (hasn't (_NixSymbol' . only (packagePrefix <> p)))
        )
      pure Nothing

readCache :: IO [NixPackage]
readCache = do
  cache             <- nixSearchUnsafe ""
  installedPackages <- readInstalledPackages
  pure
    $   (\ip -> ip & npInstalled .~ ((ip ^. npName) `elem` installedPackages))
    <$> cache

