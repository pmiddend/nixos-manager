{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
  Description: Provides functions and types regarding “services” (so anything that’s not a package basically)
  -}
module NixManager.NixServicesUtil
  ( readLocalServiceFile
  , writeLocalServiceFile
  , locateLocalServicesFile
  , readHMServiceFile
  , locateRootServicesFile
  , readHMOptionsFile
  , locateHMOptionsFile
  , locateLocalServicesFileMaybeCreate
  , writeHMServiceFile
  )
where

import Data.Validation(Validation(Failure))
import           Data.Text.Lens                 ( unpacked )
import           System.Environment             ( getEnv )
import           Control.Monad                  ( unless )
import           System.FilePath                ( (</>)
                                                , takeFileName
                                                )
import           NixManager.Constants           ( appName
                                                , rootManagerPath
                                                )
import           Data.String                    ( IsString )
import           System.Directory               ( getXdgDirectory
                                                , doesFileExist
                                                , XdgDirectory(XdgConfig)
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
                                                , _NixList
                                                , _NixSet
                                                , _NixString
                                                , writeNixFile
                                                )
import           NixManager.Util                ( TextualError
                                                , addToError
                                                , toMaybe
                                                )

-- | File name for the services Nix file
servicesFileName :: IsString s => s
servicesFileName = "services.nix"

-- | File name for the services Nix file
hmServicesFileName :: IsString s => s
hmServicesFileName = "home.nix"

-- | Locate the /local/ services file (the one for the user). Uses the XDG mechanism(s); returns the fill path.
locateLocalServicesFile :: IO FilePath
locateLocalServicesFile =
  getXdgDirectory XdgConfig (appName </> servicesFileName)

-- | Locate the /local/ services file (the one for the user). Uses the XDG mechanism(s); returns the fill path.
locateHMServicesFile :: IO FilePath
locateHMServicesFile =
  getXdgDirectory XdgConfig (appName </> hmServicesFileName)

-- | Locate the /local/ services file (the one for the user). Uses the XDG mechanism(s); returns the fill path.
locateHMOptionsFile :: IO (Maybe FilePath)
locateHMOptionsFile = do
  home <- getEnv "HOME"
  let manifestPath = home </> ".nix-profile" </> "manifest.nix"
  exists <- doesFileExist manifestPath
  if not exists
    then pure Nothing
    else do
      manifestFileContents <- parseNixFile manifestPath NixNull
      let extractPath :: NixExpr -> Maybe FilePath
          extractPath x =
            x
              ^? _NixList
              .  folded
              .  _NixSet
              .  filteredBy
                   (at "name" . folded . _NixString . only "home-manager-path")
              .  at "out"
              .  folded
              .  _NixSet
              .  at "outPath"
              .  folded
              .  _NixString
              .  unpacked
          appendFileName :: FilePath -> FilePath
          appendFileName x =
            x </> "share" </> "doc" </> "home-manager" </> "options.json"
      pure (appendFileName <$> (toMaybe manifestFileContents >>= extractPath))

-- | Locate the /root/ services file; returns its full path.
locateRootServicesFile :: IO FilePath
locateRootServicesFile = do
  localFile <- locateLocalServicesFile
  pure (rootManagerPath </> takeFileName localFile)

-- | Locate the /local/ services file and possibly create an empty one (with a valid Nix expression though) if it doesn't exist.
locateLocalServicesFileMaybeCreate :: IO FilePath
locateLocalServicesFileMaybeCreate = do
  pkgsFile <- locateLocalServicesFile
  exists   <- doesFileExist pkgsFile
  unless exists (writeLocalServiceFile emptyServiceFileContents)
  pure pkgsFile

-- | Parse the home-manager services json file
readHMOptionsFile :: IO (TextualError NixExpr)
readHMOptionsFile = locateHMOptionsFile >>= \case
  Nothing -> pure
    (Failure
      "Couldn't find the options.json path in the manifest.nix file. Have you installed home-manager correctly?"
    )
  Just svcsFile ->
    addToError
        ("Error parsing the \""
        <> servicesFileName
        <> "\" file. This is most likely a syntax error, please investigate the file itself and fix the error. Then restart nixos-manager. The error was: "
        )
      <$> parseNixFile svcsFile emptyHMServiceFileContents

-- | Parse the local Nix services file into a Nix expression, possibly returning an empty packages expression.
readHMServiceFile :: IO (TextualError NixExpr)
readHMServiceFile = do
  svcsFile <- locateHMServicesFile
  addToError
      ("Error parsing the \""
      <> servicesFileName
      <> "\" file. This is most likely a syntax error, please investigate the file itself and fix the error. Then restart nixos-manager. The error was: "
      )
    <$> parseNixFile svcsFile emptyServiceFileContents

-- | Parse the local Nix services file into a Nix expression, possibly returning an empty packages expression.
readLocalServiceFile :: IO (TextualError NixExpr)
readLocalServiceFile = do
  svcsFile <- locateLocalServicesFile
  addToError
      ("Error parsing the \""
      <> servicesFileName
      <> "\" file. This is most likely a syntax error, please investigate the file itself and fix the error. Then restart nixos-manager. The error was: "
      )
    <$> parseNixFile svcsFile emptyServiceFileContents

-- | The initial, empty services file (containing, of course, no services)
emptyServiceFileContents :: NixExpr
emptyServiceFileContents =
  NixFunctionDecl (NixFunction ["config", "pkgs", "..."] (NixSet mempty))

-- | The initial, empty services file (containing, of course, no services)
emptyHMServiceFileContents :: NixExpr
emptyHMServiceFileContents =
  NixFunctionDecl (NixFunction ["pkgs", "..."] (NixSet mempty))

-- | Write a Nix service expression into the corresponding /local/ file.
writeLocalServiceFile :: NixExpr -> IO ()
writeLocalServiceFile e = do
  svcsFile <- locateLocalServicesFile
  writeNixFile svcsFile e

-- | Write Nix service file for home-manager
writeHMServiceFile :: NixExpr -> IO ()
writeHMServiceFile e = do
  svcsFile <- locateHMServicesFile
  writeNixFile svcsFile e
