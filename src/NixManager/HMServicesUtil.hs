{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
  Description: Provides functions and types regarding “services” in home-manager

Provides functions and types regarding “services” in home-manager
  -}
module NixManager.HMServicesUtil
  ( readPendingServicesFile
  , locateOptionsFile
  , writePendingServicesFile
  , locatePendingServicesFile
  , locateInstalledServicesFile
  , locatePendingServicesFileMaybeCreate
  )
where

import           Data.Text.Lens                 ( unpacked )
import           System.Environment             ( getEnv )
import           Control.Monad                  ( unless )
import           System.FilePath                ( (</>) )
import           NixManager.Constants           ( appName )
import           Data.String                    ( IsString )
import           System.Directory               ( getXdgDirectory
                                                , doesFileExist
                                                , XdgDirectory
                                                  ( XdgConfig
                                                  , XdgCache
                                                  )
                                                )
import           Prelude                 hiding ( readFile )
import           Control.Lens                   ( filteredBy
                                                , only
                                                , at
                                                , folded
                                                , (^?)
                                                )
import           NixManager.NixExpr             ( NixExpr
                                                  ( NixFunctionDecl
                                                  , NixSet
                                                  , NixNull
                                                  )
                                                , NixFunction(NixFunction)
                                                , parseNixFile
                                                , writeNixFile
                                                )
import           NixManager.Util                ( TextualError
                                                , addToError
                                                , toMaybe
                                                )

-- | File name for the services Nix file
servicesFileName :: IsString s => s
servicesFileName = "hm-extra-services.nix"

-- | Locate the services file containing the most recently (successfully) installed service configuration.
locateInstalledServicesFile :: IO FilePath
locateInstalledServicesFile =
  getXdgDirectory XdgCache (appName </> servicesFileName)

-- | Locate the currently pending services file
locatePendingServicesFile :: IO FilePath
locatePendingServicesFile =
  getXdgDirectory XdgConfig ("nixpkgs" </> servicesFileName)

-- | Locate the home-manager’s @options.json@ file
locateOptionsFile :: IO (Maybe FilePath)
locateOptionsFile = do
  home <- getEnv "HOME"
  pure
    (Just
      (   home
      </> ".nix-profile"
      </> "share"
      </> "doc"
      </> "home-manager"
      </> "options.json"
      )
    )

-- | Locate the currently pending services file, optionally creating it
locatePendingServicesFileMaybeCreate :: IO FilePath
locatePendingServicesFileMaybeCreate = do
  pkgsFile <- locatePendingServicesFile
  exists   <- doesFileExist pkgsFile
  unless exists (writePendingServicesFile emptyServiceFileContents)
  pure pkgsFile

-- | Parse the currently pending services file into a Nix expression, possibly returning an empty packages expression.
readPendingServicesFile :: IO (TextualError NixExpr)
readPendingServicesFile = do
  svcsFile <- locatePendingServicesFile
  addToError
      ("Error parsing the \""
      <> servicesFileName
      <> "\" file. This is most likely a syntax error, please investigate the file itself and fix the error. Then restart nixos-manager. The error was: "
      )
    <$> parseNixFile svcsFile emptyServiceFileContents

-- | Parse the most-recently installed services file into a Nix expression, possibly returning an empty packages expression.
readInstalledServiceFile :: IO (TextualError NixExpr)
readInstalledServiceFile = do
  svcsFile <- locateInstalledServicesFile
  addToError
      ("Error parsing the \""
      <> servicesFileName
      <> "\" file. This is most likely a syntax error, please investigate the file itself and fix the error. Then restart nixos-manager. The error was: "
      )
    <$> parseNixFile svcsFile emptyServiceFileContents

-- | The initial, empty services file (containing, of course, no services)
emptyServiceFileContents :: NixExpr
emptyServiceFileContents =
  NixFunctionDecl (NixFunction ["pkgs", "..."] (NixSet mempty))

-- | Write a Nix service expression into the corresponding /local/ file.
writePendingServicesFile :: NixExpr -> IO ()
writePendingServicesFile e = do
  svcsFile <- locatePendingServicesFile
  writeNixFile svcsFile e
