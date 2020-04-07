{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
module NixManager.NixPackages
  ( searchPackages
  , locateLocalPackagesFile
  , locateRootPackagesFile
  , locateLocalPackagesFileMaybeCreate
  , installPackage
  , readInstalledPackages
  , readPendingPackages
  , readPendingUninstallPackages
  , readPackageCache
  , startProgram
  , uninstallPackage
  , executablesFromStorePath
  , dryInstall
  )
where

import           NixManager.Constants           ( appName
                                                , rootManagerPath
                                                )
import           Data.ByteString                ( ByteString )
import           System.Exit                    ( ExitCode
                                                  ( ExitSuccess
                                                  , ExitFailure
                                                  )
                                                )
import           NixManager.Bash                ( Expr(Command)
                                                , Arg(LiteralArg)
                                                )
import NixManager.NixLocation(flattenedTail, flattened, NixLocation, locationFromText)
import           Data.Map.Strict                ( singleton )
import           Control.Monad                  ( void
                                                , unless
                                                )
import           System.Directory               ( listDirectory
                                                , getXdgDirectory
                                                , doesFileExist
                                                , XdgDirectory(XdgConfig)
                                                )
import           System.FilePath                ( (</>)
                                                , takeFileName
                                                )
import           Data.List                      ( intercalate
                                                , (\\)
                                                , find
                                                , inits
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                , toLower
                                                , strip
                                                , stripPrefix
                                                )
import           NixManager.Util                ( TextualError
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
import           NixManager.NixPackageStatus    ( NixPackageStatus
                                                  ( NixPackageNothing
                                                  , NixPackageInstalled
                                                  , NixPackagePendingInstall
                                                  , NixPackagePendingUninstall
                                                  )
                                                )
import           NixManager.NixPackage          ( NixPackage
                                                , npPath
                                                , npName
                                                , npStatus
                                                , readPackagesJson
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
                                                , runProcessToFinish
                                                , ProcessData
                                                , waitUntilFinished
                                                , poStdout
                                                , poStderr
                                                , poResult
                                                )
import           Data.Monoid                    ( First(getFirst) )

nixSearch :: Text -> Expr
nixSearch term = Command "nix" ["search", LiteralArg term, "--json"]

searchPackages :: Text -> IO (TextualError [NixPackage])
searchPackages t = do
  po <- runProcessToFinish Nothing (nixSearch t)
  let
    processedResult = addToError
      "Error parsing output of \"nix search\" command. This could be due to changes in this command in a later version (and doesn't fix itself). Please open an issue in the nixos-manager repository. The error was: "
      (readPackagesJson (po ^. poStdout . fromStrictBS))
  case po ^?! poResult . to getFirst . folded of
    ExitSuccess      -> pure processedResult
    ExitFailure 1    -> pure processedResult
    ExitFailure code -> pure
      (Left
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
  let realPath = pkg ^. npPath . flattenedTail
  in  runProcess
        Nothing
        (Command "nix-build"
                 ["-A", LiteralArg realPath, "--no-out-link", "<nixpkgs>"]
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
startProgram fn = void (runProcess Nothing (Command (pack fn) []))

packageLens :: Traversal' NixExpr NixExpr
packageLens =
  _NixFunctionDecl . nfExpr . _NixSet . ix "environment.systemPackages"

emptyPackagesFile :: NixExpr
emptyPackagesFile = NixFunctionDecl
  (NixFunction
    ["config", "nixpkgs", "..."]
    (NixSet (singleton "environment.systemPackages" (NixList mempty)))
  )

packagesFileName :: IsString s => s
packagesFileName = "packages.nix"

locateLocalPackagesFile :: IO FilePath
locateLocalPackagesFile =
  getXdgDirectory XdgConfig (appName </> packagesFileName)

locateRootPackagesFile :: IO FilePath
locateRootPackagesFile = do
  localFile <- locateLocalPackagesFile
  pure (rootManagerPath </> takeFileName localFile)

locateLocalPackagesFileMaybeCreate :: IO FilePath
locateLocalPackagesFileMaybeCreate = do
  pkgsFile <- locateLocalPackagesFile
  exists   <- doesFileExist pkgsFile
  unless exists (writeLocalPackages emptyPackagesFile)
  pure pkgsFile

parsePackagesExpr :: FilePath -> IO (TextualError NixExpr)
parsePackagesExpr fp =
  addToError
      ("Error parsing the "
      <> showText fp
      <> " file. This is most likely a syntax error, please investigate the file itself and fix the error. Then restart nixos-manager. The error was: "
      )
    <$> parseNixFile fp emptyPackagesFile

parsePackages :: FilePath -> IO (TextualError [NixLocation])
parsePackages fp = ifSuccessIO (parsePackagesExpr fp) $ \expr ->
  case expr ^? packageLens of
    Just packages -> pure (Right (locationFromText <$> evalSymbols packages))
    Nothing -> pure (Left "Couldn't find packages node in packages.nix file.")

parseLocalPackages :: IO (TextualError [NixLocation])
parseLocalPackages = locateLocalPackagesFile >>= parsePackages

parseLocalPackagesExpr :: IO (TextualError NixExpr)
parseLocalPackagesExpr = locateLocalPackagesFile >>= parsePackagesExpr

writeLocalPackages :: NixExpr -> IO ()
writeLocalPackages e = do
  pkgsFile <- locateLocalPackagesFile
  writeNixFile pkgsFile e

packagesOrEmpty :: IO FilePath -> IO (TextualError [NixLocation])
packagesOrEmpty fp' = do
  fp       <- fp'
  fpExists <- doesFileExist fp
  if fpExists then parsePackages fp else pure (Right [])

readInstalledPackages :: IO (TextualError [NixLocation])
readInstalledPackages = packagesOrEmpty locateRootPackagesFile

readPendingPackages :: IO (TextualError [NixLocation])
readPendingPackages =
  ifSuccessIO (packagesOrEmpty locateLocalPackagesFile) $ \local ->
    ifSuccessIO (packagesOrEmpty locateRootPackagesFile)
      $ \root -> pure (Right (local \\ root))

readPendingUninstallPackages :: IO (TextualError [NixLocation])
readPendingUninstallPackages =
  ifSuccessIO (packagesOrEmpty locateLocalPackagesFile) $ \local ->
    ifSuccessIO (packagesOrEmpty locateRootPackagesFile)
      $ \root -> pure (Right (root \\ local))

installPackage :: NixPackage -> IO (TextualError ())
installPackage p = ifSuccessIO parseLocalPackagesExpr $ \expr -> do
  writeLocalPackages
    (expr & packageLens . _NixList <>~ [NixSymbol (p ^. npPath . flattened)])
  pure (Right ())

uninstallPackage :: NixPackage -> IO (TextualError ())
uninstallPackage p = ifSuccessIO parseLocalPackagesExpr $ \expr -> do
  writeLocalPackages
    (expr & packageLens . _NixList %~ filter
      (hasn't (_NixSymbol . only (p ^. npPath . flattened)))
    )
  pure (Right ())

evaluateStatus
  :: (Eq a, Foldable t1, Foldable t2, Foldable t3)
  => a
  -> t3 a
  -> t2 a
  -> t1 a
  -> NixPackageStatus
evaluateStatus name installedPackages pendingPackages pendingUninstallPackages
  | name `elem` pendingUninstallPackages = NixPackagePendingUninstall
  | name `elem` pendingPackages          = NixPackagePendingInstall
  | name `elem` installedPackages        = NixPackageInstalled
  | otherwise                            = NixPackageNothing

readPackageCache :: IO (TextualError [NixPackage])
readPackageCache = ifSuccessIO (searchPackages "") $ \cache ->
  ifSuccessIO readInstalledPackages $ \installedPackages ->
    ifSuccessIO readPendingPackages $ \pendingPackages ->
      ifSuccessIO readPendingUninstallPackages $ \pendingUninstallPackages ->
        pure
          $   Right
          $   (\ip ->
                ip
                  &  npStatus
                  .~ evaluateStatus (ip ^. npPath)
                                    installedPackages
                                    pendingPackages
                                    pendingUninstallPackages
              )
          <$> cache

