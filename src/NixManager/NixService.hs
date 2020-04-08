{-|
  Description: Provides functions and types regarding “services” (so anything that’s not a package basically)
  -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module NixManager.NixService
  ( NixService
  , serviceLoc
  , readLocalServiceFile
  , writeLocalServiceFile
  , serviceOptions
  , locateLocalServicesFile
  , locateRootServicesFile
  , locateLocalServicesFileMaybeCreate
  , makeServices
  )
where

import           Data.Maybe                     ( mapMaybe )
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
import qualified Data.Set                      as Set
import           Prelude                 hiding ( readFile )
import           Data.Map.Strict                ( Map
                                                , insertWith
                                                , toList
                                                , elems
                                                )
import           Data.Text                      ( Text )
import           Control.Lens                   ( (^.)
                                                , makeLenses
                                                , view
                                                )
import           NixManager.NixExpr             ( NixExpr
                                                  ( NixFunctionDecl
                                                  , NixSet
                                                  )
                                                , NixFunction(NixFunction)
                                                , parseNixFile
                                                , writeNixFile
                                                )
import           NixManager.Util                ( Endo
                                                , TextualError
                                                , addToError
                                                )
import           NixManager.NixLocation         ( NixLocation
                                                , removeLastComponent
                                                , isPrefixOf
                                                )
import           NixManager.NixServiceOption    ( NixServiceOption
                                                , optionLoc
                                                )

-- | Represents a service with a location and some options.
data NixService = NixService {
    _serviceLoc :: NixLocation -- ^ Service location
  , _serviceOptions :: [NixServiceOption] -- ^ Service options
  } deriving(Show)

makeLenses ''NixService

-- canBeEnabled :: NixLocation -> Bool
-- canBeEnabled = (== "enable") . last

-- isService :: NixLocation -> Bool
-- isService = (== "services") . head

-- | Create a list of services from a map (such as the map contained in the @options.json@ file)
makeServices :: Map Text NixServiceOption -> [NixService]
makeServices options' =
  let
    options = elems options'
    servicePaths :: Set.Set NixLocation
    servicePaths =
      Set.fromList (removeLastComponent `mapMaybe` (view optionLoc <$> options))
    serviceForOption :: NixServiceOption -> Maybe NixLocation
    serviceForOption opt = case Set.lookupLT (opt ^. optionLoc) servicePaths of
      Nothing -> Nothing
      Just result ->
        if result `isPrefixOf` (opt ^. optionLoc) then Just result else Nothing
    transducer :: NixServiceOption -> Endo (Map NixLocation [NixServiceOption])
    transducer opt m = case serviceForOption opt of
      Nothing          -> m
      Just serviceLoc' -> insertWith (<>) serviceLoc' [opt] m
    serviceMap = foldr transducer mempty options
  in
    uncurry NixService <$> toList serviceMap

-- | File name for the services Nix file
servicesFileName :: IsString s => s
servicesFileName = "services.nix"

-- | Locate the /local/ services file (the one for the user). Uses the XDG mechanism(s); returns the fill path.
locateLocalServicesFile :: IO FilePath
locateLocalServicesFile =
  getXdgDirectory XdgConfig (appName </> servicesFileName)

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

-- | Write a Nix service expression into the corresponding /local/ file.
writeLocalServiceFile :: NixExpr -> IO ()
writeLocalServiceFile e = do
  svcsFile <- locateLocalServicesFile
  writeNixFile svcsFile e
