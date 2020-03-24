{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module NixManager.Nix
  ( getExecutables
  , startProgram
  , installPackage
  , readCache
  , uninstallPackage
  )
where

import           Prelude                 hiding ( readFile )
import           Data.List                      ( intercalate
                                                , find
                                                , inits
                                                )
import           System.FilePath                ( (</>) )
import           System.Directory               ( listDirectory )
import           Control.Exception              ( catch
                                                , IOException
                                                )
import           Data.ByteString.Lazy           ( hGetContents )
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
                                                , stripPrefix
                                                )
import           Control.Lens                   ( (^.)
                                                , (.~)
                                                , (^?)
                                                , (^?!)
                                                , ix
                                                , Traversal'
                                                , view
                                                , hasn't
                                                , folded
                                                , only
                                                , (<>~)
                                                , (&)
                                                , to
                                                , (%~)
                                                )
import           Data.Text.Lens                 ( unpacked
                                                , packed
                                                )
import           Control.Monad                  ( void )
import           NixManager.NixExpr             ( NixExpr(NixSymbol)
                                                , _NixFunctionDecl
                                                , nfExpr
                                                , _NixSymbol
                                                , evalSymbols
                                                , _NixSet
                                                , parseNixFile
                                                , writeNixFile
                                                , _NixList
                                                )
import           NixManager.Util                ( MaybeError(Success, Error)
                                                , splitRepeat
                                                , addToError
                                                , fromEither
                                                , ifSuccessIO
                                                )
import           NixManager.NixServiceOption    ( )
import           NixManager.NixPackage          ( NixPackage
                                                , npPath
                                                , npName
                                                , npInstalled
                                                )
import           NixManager.PackageSearch       ( searchPackages )


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
  -- FIXME: error handling
  let realPath = pkg ^?! npPath . to (stripPrefix "nixpkgs.") . folded
  (_, Just hout, _, _) <- createProcess
    (proc "nix-build" ["-A", realPath ^. unpacked, "--no-out-link", "<nixpkgs>"]
      )
      { std_out = CreatePipe
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
  _NixFunctionDecl . nfExpr . _NixSet . ix "environment.systemPackages"

parsePackages :: IO (MaybeError NixExpr)
parsePackages =
  addToError
      "Error parsing the packages.nix file. This is most likely a syntax error, please investigate the file itself and fix the error. Then restart nixos-manager. The error was: "
    .   fromEither

    <$> parseNixFile "packages.nix"

writePackages :: NixExpr -> IO ()
writePackages = writeNixFile "packages.nix"

readInstalledPackages :: IO (MaybeError [Text])
readInstalledPackages = ifSuccessIO parsePackages $ \expr ->
  case expr ^? packageLens of
    Just packages -> pure (Success (Text.drop 5 <$> evalSymbols packages))
    Nothing -> pure (Error "Couldn't find packages node in packages.nix file.")

packagePrefix :: Text
packagePrefix = "pkgs."

installPackage :: Text -> IO (MaybeError ())
installPackage p = ifSuccessIO parsePackages $ \expr -> do
  writePackages
    (expr & packageLens . _NixList <>~ [NixSymbol (packagePrefix <> p)])
  pure (Success ())

uninstallPackage :: Text -> IO (MaybeError ())
uninstallPackage p = ifSuccessIO parsePackages $ \expr -> do
  writePackages
    (expr & packageLens . _NixList %~ filter
      (hasn't (_NixSymbol . only (packagePrefix <> p)))
    )
  pure (Success ())

readCache :: IO (MaybeError [NixPackage])
readCache = ifSuccessIO (searchPackages "") $ \cache ->
  ifSuccessIO readInstalledPackages $ \installedPackages ->
    pure
      $   Success
      $   (\ip -> ip & npInstalled .~ ((ip ^. npName) `elem` installedPackages))
      <$> cache

