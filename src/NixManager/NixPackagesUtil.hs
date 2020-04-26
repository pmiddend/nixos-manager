{-|
  Description: Functions to process (install/uninstall, ...) Nix packages
Functions to process (install/uninstall, ...) Nix packages
  -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
module NixManager.NixPackagesUtil
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

import           Data.Validation                ( Validation(Success, Failure) )
import           Data.Text                      ( strip
                                                , toLower
                                                , pack
                                                )
import           NixManager.Constants           ( appName
                                                , rootManagerPath
                                                )
import           Data.ByteString                ( ByteString )
import           NixManager.Bash                ( Expr(Command)
                                                , Arg(LiteralArg)
                                                )
import           NixManager.NixLocation         ( flattenedTail
                                                , flattened
                                                , NixLocation
                                                , locationFromText
                                                )
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
import           NixManager.Util                ( TextualError
                                                , decodeUtf8
                                                , splitRepeat
                                                , addToError
                                                , ifSuccessIO
                                                , showText
                                                )
import           Data.String                    ( IsString )
import           NixManager.NixExpr             ( NixExpr
                                                  ( NixSymbol
                                                  , NixSet
                                                  , NixList
                                                  , NixFunctionDecl
                                                  )
                                                , NixFunction(NixFunction)
                                                , evalSymbols
                                                , parseNixFile
                                                , writeNixFile
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
import           NixManager.NixPackage          ( NixPackage )
import           Control.Lens                   ( (^.)
                                                , (.~)
                                                , (^?)
                                                , ix
                                                , Traversal'
                                                , hasn't
                                                , only
                                                , (<>~)
                                                , (&)
                                                , to
                                                , (%~)
                                                )
import           Data.Text.Lens                 ( unpacked )
import           NixManager.Process             ( runProcess
                                                , ProcessData
                                                )
import           NixManager.NixPackageSearch    ( searchPackages )

-- | Given a package name and a list of binaries in that package, try to identify which binary is likely /the/ binary for the package.
matchName :: String -> [FilePath] -> Maybe FilePath
matchName pkgName bins =
  let undashed :: [String]
      undashed = splitRepeat '-' pkgName
      parts :: [String]
      parts = intercalate "-" <$> reverse (inits undashed)
  in  find (`elem` bins) parts

-- | Build the package (invoking @nix-build@). The returned stdout can then be inspected for the path.
dryInstall :: NixPackage -> IO ProcessData
dryInstall pkg =
  let realPath = pkg ^. #path . flattenedTail
  in  runProcess
        Nothing
        (Command "nix-build"
                 ["-A", LiteralArg realPath, "--no-out-link", "<nixpkgs>"]
        )

-- | Given a package and the output of @nix-build@, try to determine the binary path as well as the found binaries.
executablesFromStorePath
  :: NixPackage -> ByteString -> IO (FilePath, [FilePath])
executablesFromStorePath pkg stdout = do
  let packagePath = stdout ^. decodeUtf8 . to strip . unpacked
  let binPath     = packagePath </> "bin"
  bins <- listDirectory binPath `catch` \(_ :: IOException) -> pure []
  let normalizedName = pkg ^. #name . to toLower . unpacked
  case matchName normalizedName bins of
    Nothing      -> pure (binPath, bins)
    Just matched -> pure (binPath, [matched])

-- | Start a binary
startProgram :: FilePath -> IO ()
startProgram fn = void (runProcess Nothing (Command (pack fn) []))

-- | Lens to extract the list of packages inside a Nix expression
packageLens :: Traversal' NixExpr NixExpr
packageLens =
  #_NixFunctionDecl . #functionExpr . #_NixSet . ix "environment.systemPackages"

-- | The initial, empty packages file (containing, of course, no packages)
emptyPackagesFile :: NixExpr
emptyPackagesFile = NixFunctionDecl
  (NixFunction
    ["config", "nixpkgs", "..."]
    (NixSet (singleton "environment.systemPackages" (NixList mempty)))
  )

-- | File name for the packages Nix file
packagesFileName :: IsString s => s
packagesFileName = "packages.nix"

-- | Locate the /local/ packages file (the one for the user). Uses the XDG mechanism(s); returns the full path.
locateLocalPackagesFile :: IO FilePath
locateLocalPackagesFile =
  getXdgDirectory XdgConfig (appName </> packagesFileName)

-- | Locate the /root/ packages file; returns its full path.
locateRootPackagesFile :: IO FilePath
locateRootPackagesFile = do
  localFile <- locateLocalPackagesFile
  pure (rootManagerPath </> takeFileName localFile)

-- | Locate the /local/ packages file and possibly create an empty one (with a valid Nix expression though) if it doesn't exist.
locateLocalPackagesFileMaybeCreate :: IO FilePath
locateLocalPackagesFileMaybeCreate = do
  pkgsFile <- locateLocalPackagesFile
  exists   <- doesFileExist pkgsFile
  unless exists (writeLocalPackages emptyPackagesFile)
  pure pkgsFile

-- | Parse a Nix package file into a Nix expression, possibly returning an empty packages expression.
parsePackagesExpr :: FilePath -> IO (TextualError NixExpr)
parsePackagesExpr fp =
  addToError
      ("Error parsing the "
      <> showText fp
      <> " file. This is most likely a syntax error, please investigate the file itself and fix the error. Then restart nixos-manager. The error was: "
      )
    <$> parseNixFile fp emptyPackagesFile

-- | Parse a packages file into a list of package locations (the only real payload we're interested in).
parsePackages :: FilePath -> IO (TextualError [NixLocation])
parsePackages fp = ifSuccessIO (parsePackagesExpr fp) $ \expr ->
  case expr ^? packageLens of
    Just packages -> pure (Success (locationFromText <$> evalSymbols packages))
    Nothing ->
      pure (Failure "Couldn't find packages node in packages.nix file.")

-- | Parse the /local/ packages file, return the Nix expression. Possible creates the packages file.
parseLocalPackagesExpr :: IO (TextualError NixExpr)
parseLocalPackagesExpr = locateLocalPackagesFile >>= parsePackagesExpr

-- | Write a Nix packages expression into the corresponding /local/ file.
writeLocalPackages :: NixExpr -> IO ()
writeLocalPackages e = do
  pkgsFile <- locateLocalPackagesFile
  writeNixFile pkgsFile e

-- | Run an action returning a file path to a Nix file, parse that file and return the contained packages.
packagesOrEmpty :: IO FilePath -> IO (TextualError [NixLocation])
packagesOrEmpty fp' = do
  fp       <- fp'
  fpExists <- doesFileExist fp
  if fpExists then parsePackages fp else pure (Success [])

-- | Read the /root/ packages file and return the contained packages.
readInstalledPackages :: IO (TextualError [NixLocation])
readInstalledPackages = packagesOrEmpty locateRootPackagesFile

-- | Read the local and root packages file, compare them and return the packages that are pending installation.
readPendingPackages :: IO (TextualError [NixLocation])
readPendingPackages =
  ifSuccessIO (packagesOrEmpty locateLocalPackagesFile) $ \local ->
    ifSuccessIO (packagesOrEmpty locateRootPackagesFile)
      $ \root -> pure (Success (local \\ root))

-- | Read the local and root packages file, compare them and return the packages that are pending uninstallation.
readPendingUninstallPackages :: IO (TextualError [NixLocation])
readPendingUninstallPackages =
  ifSuccessIO (packagesOrEmpty locateLocalPackagesFile) $ \local ->
    ifSuccessIO (packagesOrEmpty locateRootPackagesFile)
      $ \root -> pure (Success (root \\ local))

-- | Mark a package for installation by writing it into the local packages file.
installPackage :: NixPackage -> IO (TextualError ())
installPackage p = ifSuccessIO parseLocalPackagesExpr $ \expr -> do
  writeLocalPackages
    (expr & packageLens . #_NixList <>~ [NixSymbol (p ^. #path . flattened)])
  pure (Success ())

-- | Mark a package for uninstallation by removing it from the local packages file.
uninstallPackage :: NixPackage -> IO (TextualError ())
uninstallPackage p = ifSuccessIO parseLocalPackagesExpr $ \expr -> do
  writeLocalPackages
    (expr & packageLens . #_NixList %~ filter
      (hasn't (#_NixSymbol . only (p ^. #path . flattened)))
    )
  pure (Success ())

-- | Evaluate a package's status given all the packages lists (pending, installed, ...)
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

-- | Read all packages including their correct status (installed, pending install, ...)
readPackageCache :: IO (TextualError [NixPackage])
readPackageCache = ifSuccessIO (searchPackages "") $ \cache ->
  ifSuccessIO readInstalledPackages $ \installedPackages ->
    ifSuccessIO readPendingPackages $ \pendingPackages ->
      ifSuccessIO readPendingUninstallPackages $ \pendingUninstallPackages ->
        pure
          $   Success
          $   (\ip ->
                ip
                  &  #status
                  .~ evaluateStatus (ip ^. #path)
                                    installedPackages
                                    pendingPackages
                                    pendingUninstallPackages
              )
          <$> cache

