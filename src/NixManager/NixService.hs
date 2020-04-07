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
import           NixManager.NixLocation
                                                ( NixLocation
                                                , removeLastComponent
                                                , isPrefixOf
                                                )
import           NixManager.NixServiceOption    ( NixServiceOption
                                                , optionLoc
                                                )


data NixService = NixService {
    _serviceLoc :: NixLocation
  , _serviceOptions :: [NixServiceOption]
  } deriving(Show)

makeLenses ''NixService

-- canBeEnabled :: NixLocation -> Bool
-- canBeEnabled = (== "enable") . last

-- isService :: NixLocation -> Bool
-- isService = (== "services") . head

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
    transducer
      :: NixServiceOption
      -> Endo (Map NixLocation [NixServiceOption])
    transducer opt m = case serviceForOption opt of
      Nothing          -> m
      Just serviceLoc' -> insertWith (<>) serviceLoc' [opt] m
    serviceMap = foldr transducer mempty options
  in
    uncurry NixService <$> toList serviceMap

servicesFileName :: IsString s => s
servicesFileName = "services.nix"

locateLocalServicesFile :: IO FilePath
locateLocalServicesFile =
  getXdgDirectory XdgConfig (appName </> servicesFileName)

locateRootServicesFile :: IO FilePath
locateRootServicesFile = do
  localFile <- locateLocalServicesFile
  pure (rootManagerPath </> takeFileName localFile)

locateLocalServicesFileMaybeCreate :: IO FilePath
locateLocalServicesFileMaybeCreate = do
  pkgsFile <- locateLocalServicesFile
  exists   <- doesFileExist pkgsFile
  unless exists (writeLocalServiceFile emptyServiceFileContents)
  pure pkgsFile


readLocalServiceFile :: IO (TextualError NixExpr)
readLocalServiceFile = do
  svcsFile <- locateLocalServicesFile
  addToError
      ("Error parsing the \""
      <> servicesFileName
      <> "\" file. This is most likely a syntax error, please investigate the file itself and fix the error. Then restart nixos-manager. The error was: "
      )
    <$> parseNixFile svcsFile emptyServiceFileContents

emptyServiceFileContents :: NixExpr
emptyServiceFileContents =
  NixFunctionDecl (NixFunction ["config", "pkgs", "..."] (NixSet mempty))

writeLocalServiceFile :: NixExpr -> IO ()
writeLocalServiceFile e = do
  svcsFile <- locateLocalServicesFile
  writeNixFile svcsFile e
