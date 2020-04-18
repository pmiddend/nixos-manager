{-|
  Description: Functions to process (install/uninstall, ...) home-manager packages
Functions to process (install/uninstall, ...) home-manager packages
  -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
module NixManager.HMPackagesUtil
  ( readPackageCache
  , installPackage
  , uninstallPackage
  , packagesFileName
  , locatePendingPackagesFile
  , locatePendingPackagesFileMaybeCreate
  , locateInstalledPackagesFile
  )
where

import           Control.Monad                  ( void )
import           NixManager.NixPackageSearch    ( searchPackages )
import           NixManager.Constants           ( appName )
import           NixManager.NixLocation         ( flattened
                                                , NixLocation
                                                , locationFromText
                                                , replaceFirstComponent
                                                )
import           Data.Map.Strict                ( singleton )
import           System.Directory               ( getXdgDirectory
                                                , doesFileExist
                                                , XdgDirectory
                                                  ( XdgConfig
                                                  , XdgCache
                                                  )
                                                )
import           System.FilePath                ( (</>) )
import           Data.List                      ( (\\) )
import           NixManager.Util                ( TextualError
                                                , Endo
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
                                                , _NixFunctionDecl
                                                , nfExpr
                                                , _NixSymbol
                                                , evalSymbols
                                                , _NixSet
                                                , parseNixFile
                                                , writeNixFile
                                                , _NixList
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
                                                , npStatus
                                                )
import           Control.Lens                   ( (^.)
                                                , to
                                                , set
                                                , (^?)
                                                , ix
                                                , Traversal'
                                                , hasn't
                                                , only
                                                , (<>~)
                                                , (&)
                                                , (%~)
                                                )

-- | File name for the packages Nix file
packagesFileName :: IsString s => s
packagesFileName = "hm-extra-packages.nix"

-- | Locate the pending packages file. Uses the XDG mechanism(s); returns the full path.
locatePendingPackagesFile :: IO FilePath
locatePendingPackagesFile =
  getXdgDirectory XdgConfig ("nixpkgs" </> packagesFileName)

-- | Locate the pending packages file or create an empty one. Uses the XDG mechanism(s); returns the full path.
locatePendingPackagesFileMaybeCreate :: IO FilePath
locatePendingPackagesFileMaybeCreate = do
  void (modifyPendingPackagesExpr id)
  locatePendingPackagesFile

-- | Locate the /local/ packages file (the one for the user). Uses the XDG mechanism(s); returns the full path.
locateInstalledPackagesFile :: IO FilePath
locateInstalledPackagesFile =
  getXdgDirectory XdgCache (appName </> packagesFileName)

emptyPackagesFile :: NixExpr
emptyPackagesFile = NixFunctionDecl
  (NixFunction ["pkgs", "..."]
               (NixSet (singleton "home.packages" (NixList mempty)))
  )

-- | Parse a Nix package file into a Nix expression, possibly returning an empty packages expression.
parsePackagesExpr :: FilePath -> IO (TextualError NixExpr)
parsePackagesExpr fp =
  addToError
      ("Error parsing the "
      <> showText fp
      <> " file. This is most likely a syntax error, please investigate the file itself and fix the error. Then restart nixos-manager. The error was: "
      )
    <$> parseNixFile fp emptyPackagesFile

-- | Parse the /local/ packages file, return the Nix expression. Possible creates the packages file.
parsePendingPackagesExpr :: IO (TextualError NixExpr)
parsePendingPackagesExpr = locatePendingPackagesFile >>= parsePackagesExpr

-- | Write the pending package Nix expression
writePendingPackagesExpr :: NixExpr -> IO ()
writePendingPackagesExpr e = do
  pkgsFile <- locatePendingPackagesFile
  writeNixFile pkgsFile e

-- | Lens to extract the list of packages inside a Nix expression
packageLens :: Traversal' NixExpr NixExpr
packageLens = _NixFunctionDecl . nfExpr . _NixSet . ix "home.packages"

-- | Append a package into the packages list
appendPackage :: NixPackage -> Endo NixExpr
appendPackage p e =
  e
    &   packageLens
    .   _NixList
    <>~ [ NixSymbol
            (p ^. npPath . to (replaceFirstComponent "pkgs") . flattened)
        ]

-- | Remove a package from a package Nix expxression
removePackage :: NixPackage -> Endo NixExpr
removePackage p e = e & packageLens . _NixList %~ filter
  (hasn't
    ( _NixSymbol
    . only (p ^. npPath . to (replaceFirstComponent "pkgs") . flattened)
    )
  )

-- | Modify the pending package Nix expression, possibly creating it
modifyPendingPackagesExpr f = ifSuccessIO parsePendingPackagesExpr $ \expr ->
  do
    writePendingPackagesExpr (f expr)
    pure (Right ())

-- | Mark a package for installation by writing it into the local packages file.
installPackage :: NixPackage -> IO (TextualError ())
installPackage p = modifyPendingPackagesExpr (appendPackage p)

-- | Mark a package for uninstallation by removing it from the local packages file.
uninstallPackage :: NixPackage -> IO (TextualError ())
uninstallPackage p = modifyPendingPackagesExpr (removePackage p)

-- | Parse a packages file into a list of package locations (the only real payload we're interested in).
parsePackages :: FilePath -> IO (TextualError [NixLocation])
parsePackages fp = ifSuccessIO (parsePackagesExpr fp) $ \expr ->
  case expr ^? packageLens of
    Just packages -> pure (Right (locationFromText <$> evalSymbols packages))
    Nothing -> pure (Left "Couldn't find packages node in packages.nix file.")

-- | Run an action returning a file path to a Nix file, parse that file and return the contained packages.
packagesOrEmpty :: IO FilePath -> IO (TextualError [NixLocation])
packagesOrEmpty fp' = do
  fp       <- fp'
  fpExists <- doesFileExist fp
  if fpExists then parsePackages fp else pure (Right [])

-- | Read pending Nix package expression
readPendingPackages :: IO (TextualError [NixLocation])
readPendingPackages = packagesOrEmpty locatePendingPackagesFile

-- | Read installed Nix package expression
readInstalledPackages :: IO (TextualError [NixLocation])
readInstalledPackages = packagesOrEmpty locateInstalledPackagesFile

-- | Evaluate if a package is installed, marked for installation, marked for uninstallation or neither of those
evaluateStatus name installed added removed
  | name `elem` added     = NixPackagePendingInstall
  | name `elem` removed   = NixPackagePendingUninstall
  | name `elem` installed = NixPackageInstalled
  | otherwise             = NixPackageNothing

-- | Read all packages including their correct status (installed, pending install, ...)
readPackageCache :: IO (TextualError [NixPackage])
readPackageCache = ifSuccessIO (searchPackages "") $ \cache ->
  ifSuccessIO readInstalledPackages $ \installedPackages ->
    ifSuccessIO readPendingPackages $ \pendingPackages -> do
      let addedPackages   = pendingPackages \\ installedPackages
          removedPackages = installedPackages \\ pendingPackages
      pure
        $   Right
        $   (\ip -> set
              npStatus
              (evaluateStatus (ip ^. npPath . to (replaceFirstComponent "pkgs"))
                              installedPackages
                              addedPackages
                              removedPackages
              )
              ip
            )
        <$> cache


