{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
module NixManager.PackageSearch
  ( searchPackages
  , locatePackagesFileMaybeCreate
  , installPackage
  , readCache
  , startProgram
  , uninstallPackage
  , executablesFromStorePath
  , dryInstall
  )
where

import           Data.ByteString                ( ByteString )
import           System.Exit                    ( ExitCode
                                                  ( ExitSuccess
                                                  , ExitFailure
                                                  )
                                                )
import           NixManager.BashDsl             ( BashExpr(BashCommand)
                                                , BashArg(BashLiteralArg)
                                                , nixSearch
                                                )
import           NixManager.Constants           ( appName )
import           Data.Map.Strict                ( singleton )
import           Control.Monad                  ( void
                                                , unless
                                                )
import           System.Directory               ( listDirectory
                                                , getXdgDirectory
                                                , doesFileExist
                                                , XdgDirectory(XdgConfig)
                                                )
import           System.FilePath                ( (</>) )
import           Data.List                      ( intercalate
                                                , find
                                                , inits
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                , toLower
                                                , strip
                                                , stripPrefix
                                                )
import           NixManager.Util                ( MaybeError(Success, Error)
                                                , decodeUtf8
                                                , fromStrictBS
                                                , splitRepeat
                                                , addToError
                                                , ifSuccessIO
                                                , showText
                                                )
import qualified Data.Text                     as Text
import           Data.String                    ( IsString )
import           NixManager.NixExpr             ( NixExpr
                                                  ( NixSymbol
                                                  , NixSet
                                                  , NixList
                                                  , NixFunctionDecl
                                                  )
                                                , NixFunction(NixFunction)
                                                , _NixFunctionDecl
                                                , nfExpr
                                                , _NixSymbol
                                                , evalSymbols
                                                , _NixSet
                                                , parseNixFile
                                                , writeNixFile
                                                , _NixList
                                                )
import           Control.Exception              ( catch
                                                , IOException
                                                )
import           NixManager.NixPackage          ( NixPackage
                                                , npPath
                                                , npName
                                                , npInstalled
                                                , readPackages
                                                )
import           Control.Lens                   ( (^.)
                                                , (.~)
                                                , (^?)
                                                , (^?!)
                                                , ix
                                                , Traversal'
                                                , hasn't
                                                , folded
                                                , only
                                                , (<>~)
                                                , (&)
                                                , to
                                                , (%~)
                                                )
import           Data.Text.Lens                 ( unpacked )
import           NixManager.Process             ( runProcess
                                                , ProcessData
                                                , waitUntilFinished
                                                , poStdout
                                                , poStderr
                                                , poResult
                                                )
import           Data.Monoid                    ( First(getFirst) )

searchPackages :: Text -> IO (MaybeError [NixPackage])
searchPackages t = do
  pd <- runProcess Nothing (nixSearch t)
  po <- waitUntilFinished pd
  let
    processedResult = addToError
      "Error parsing output of \"nix search\" command. This could be due to changes in this command in a later version (and doesn't fix itself). Please open an issue in the nixos-manager repository. The error was: "
      (readPackages (po ^. poStdout . fromStrictBS))
  case po ^?! poResult . to getFirst . folded of
    ExitSuccess      -> pure processedResult
    ExitFailure 1    -> pure processedResult
    ExitFailure code -> pure
      (Error
        (  "Error executing \"nix search\" command (exit code "
        <> showText code
        <> "): standard error output: "
        <> (po ^. poStderr . decodeUtf8)
        )
      )


matchName :: String -> [FilePath] -> Maybe FilePath
matchName pkgName bins =
  let undashed :: [String]
      undashed = splitRepeat '-' pkgName
      parts :: [String]
      parts = intercalate "-" <$> reverse (inits undashed)
  in  find (`elem` bins) parts

dryInstall :: NixPackage -> IO ProcessData
dryInstall pkg =
  let realPath = pkg ^?! npPath . to (stripPrefix "nixpkgs.") . folded
  in  runProcess
        Nothing
        (BashCommand
          "nix-build"
          ["-A", BashLiteralArg realPath, "--no-out-link", "<nixpkgs>"]
        )

executablesFromStorePath
  :: NixPackage -> ByteString -> IO (FilePath, [FilePath])
executablesFromStorePath pkg stdout = do
  let packagePath = stdout ^. decodeUtf8 . to strip . unpacked
  let binPath     = packagePath </> "bin"
  bins <- listDirectory binPath `catch` \(_ :: IOException) -> pure []
  let normalizedName = pkg ^. npName . to toLower . unpacked
  case matchName normalizedName bins of
    Nothing      -> pure (binPath, bins)
    Just matched -> pure (binPath, [matched])

startProgram :: FilePath -> IO ()
startProgram fn = void (runProcess Nothing (BashCommand (pack fn) []))

packageLens :: Traversal' NixExpr NixExpr
packageLens =
  _NixFunctionDecl . nfExpr . _NixSet . ix "environment.systemPackages"

emptyPackagesFile :: NixExpr
emptyPackagesFile = NixFunctionDecl
  (NixFunction
    ["config", "pkgs", "..."]
    (NixSet (singleton "environment.systemPackages" (NixList mempty)))
  )

packagesFileName :: IsString s => s
packagesFileName = "packages.nix"

locatePackagesFile :: IO FilePath
locatePackagesFile = getXdgDirectory XdgConfig (appName </> packagesFileName)

locatePackagesFileMaybeCreate :: IO FilePath
locatePackagesFileMaybeCreate = do
  pkgsFile <- locatePackagesFile
  exists   <- doesFileExist pkgsFile
  unless exists (writePackages emptyPackagesFile)
  pure pkgsFile

parsePackages :: IO (MaybeError NixExpr)
parsePackages = do
  pkgsFile <- locatePackagesFile
  addToError
      ("Error parsing the "
      <> packagesFileName
      <> " file. This is most likely a syntax error, please investigate the file itself and fix the error. Then restart nixos-manager. The error was: "
      )
    <$> parseNixFile pkgsFile emptyPackagesFile

writePackages :: NixExpr -> IO ()
writePackages e = do
  pkgsFile <- locatePackagesFile
  writeNixFile pkgsFile e

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

