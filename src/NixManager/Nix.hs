{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module NixManager.Nix where

import qualified Data.Set                      as Set
import           Prelude                 hiding ( readFile )
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

decodeNixSearchResult :: ByteString -> MaybeError (Map Text NixPackage)
decodeNixSearchResult = fromEither . eitherDecode

nixSearch :: Text -> IO (MaybeError [NixPackage])
nixSearch t = do
  (_, Just hout, _, _) <- createProcess
    (proc "nix" ["search", t ^. unpacked, "--json"]) { std_out = CreatePipe }
  out <- hGetContents hout
  pure
    (addToError
      "Error parsing output of \"nix search\" command. This could be due to changes in this command in a later version (and doesn't fix itself). Please open an issue in the nixos-manager repository. The error was: "
      (elems <$> decodeNixSearchResult out)
    )

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


decodeOptions :: ByteString -> MaybeError (Map Text NixServiceOption)
decodeOptions =
  ( addToError "Couldn't read the options JSON file. The error was: "
    . fromEither
    )
    . eitherDecode

readOptionsFile :: FilePath -> IO (MaybeError (Map Text NixServiceOption))
readOptionsFile fp = decodeOptions <$> readFile fp

data NixService = NixService {
    _serviceLoc :: OptionLocation
  , _serviceOptions :: [NixServiceOption]
  } deriving(Show, Eq)

makeLenses ''NixService

canBeEnabled :: OptionLocation -> Bool
canBeEnabled = (== "enable") . last

isService :: OptionLocation -> Bool
isService = (== "services") . head

predAnd :: (t -> Bool) -> (t -> Bool) -> t -> Bool
predAnd a b x = a x && b x

makeServices :: Map Text NixServiceOption -> [NixService]
makeServices options' =
  let
    options = elems options'
    servicePaths :: Set.Set OptionLocation
    servicePaths = Set.fromList
      (init <$> filter (canBeEnabled `predAnd` isService)
                       (view optionLoc <$> options)
      )
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

parsePackages :: IO (MaybeError NixExpr)
parsePackages =
  addToError
      "Error parsing the packages.nix file. This is most likely a syntax error, please investigate the file itself and fix the error. Then restart nixos-manager. The error was: "
    .   fromEither

    <$> parseFile "packages.nix"

writePackages :: NixExpr -> IO ()
writePackages = writeExprFile "packages.nix"

readInstalledPackages :: IO (MaybeError [Text])
readInstalledPackages = ifSuccessIO parsePackages $ \expr ->
  case expr ^? packageLens of
    Just packages -> pure (Success (Text.drop 5 <$> cata evalSymbols packages))
    Nothing -> pure (Error "Couldn't find packages node in packages.nix file.")

packagePrefix :: Text
packagePrefix = "pkgs."

installPackage :: Text -> IO (MaybeError ())
installPackage p = ifSuccessIO parsePackages $ \expr -> do
  writePackages
    (expr & packageLens . _NixList' <>~ [Fix (NixSymbol (packagePrefix <> p))])
  pure (Success ())

uninstallPackage :: Text -> IO (MaybeError ())
uninstallPackage p = ifSuccessIO parsePackages $ \expr -> do
  writePackages
    (expr & packageLens . _NixList' %~ filter
      (hasn't (_NixSymbol' . only (packagePrefix <> p)))
    )
  pure (Success ())

readCache :: IO (MaybeError [NixPackage])
readCache = ifSuccessIO (nixSearch "") $ \cache ->
  ifSuccessIO readInstalledPackages $ \installedPackages ->
    pure
      $   Success
      $   (\ip -> ip & npInstalled .~ ((ip ^. npName) `elem` installedPackages))
      <$> cache

