{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module NixManager.NixService
  ( NixService
  , serviceLoc
  , readServices
  , writeServiceFile
  , serviceOptions
  , locateServicesFileMaybeCreate
  , makeServices
  )
where

import           Control.Monad                  ( unless )
import           System.FilePath                ( (</>) )
import           NixManager.Constants           ( appName )
import           Data.String                    ( IsString )
import           System.Directory               ( getXdgDirectory
                                                , doesFileExist
                                                , XdgDirectory(XdgConfig)
                                                )
import qualified Data.Set                      as Set
import           Prelude                 hiding ( readFile )
import           Data.List                      ( isPrefixOf )
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
                                                , MaybeError
                                                , addToError
                                                , predAnd
                                                )
import           NixManager.NixServiceOption    ( NixServiceOptionLocation
                                                , NixServiceOption
                                                , optionLoc
                                                )


data NixService = NixService {
    _serviceLoc :: NixServiceOptionLocation
  , _serviceOptions :: [NixServiceOption]
  } deriving(Show)

makeLenses ''NixService

canBeEnabled :: NixServiceOptionLocation -> Bool
canBeEnabled = (== "enable") . last

isService :: NixServiceOptionLocation -> Bool
isService = (== "services") . head

makeServices :: Map Text NixServiceOption -> [NixService]
makeServices options' =
  let
    options = elems options'
    servicePaths :: Set.Set NixServiceOptionLocation
    servicePaths = Set.fromList
      (init <$> filter (canBeEnabled `predAnd` isService)
                       (view optionLoc <$> options)
      )
    serviceForOption :: NixServiceOption -> Maybe NixServiceOptionLocation
    serviceForOption opt = case Set.lookupLT (opt ^. optionLoc) servicePaths of
      Nothing -> Nothing
      Just result ->
        if result `isPrefixOf` (opt ^. optionLoc) then Just result else Nothing
    transducer
      :: NixServiceOption
      -> Endo (Map NixServiceOptionLocation [NixServiceOption])
    transducer opt m = case serviceForOption opt of
      Nothing          -> m
      Just serviceLoc' -> insertWith (<>) serviceLoc' [opt] m
    serviceMap = foldr transducer mempty options
  in
    uncurry NixService <$> toList serviceMap

servicesFileName :: IsString s => s
servicesFileName = "services.nix"

locateServicesFile :: IO FilePath
locateServicesFile = getXdgDirectory XdgConfig (appName </> servicesFileName)

locateServicesFileMaybeCreate :: IO FilePath
locateServicesFileMaybeCreate = do
  pkgsFile <- locateServicesFile
  exists   <- doesFileExist pkgsFile
  unless exists (writeServiceFile emptyServiceFileContents)
  pure pkgsFile


readServices :: IO (MaybeError NixExpr)
readServices = do
  svcsFile <- locateServicesFile
  addToError
      ("Error parsing the \""
      <> servicesFileName
      <> "\" file. This is most likely a syntax error, please investigate the file itself and fix the error. Then restart nixos-manager. The error was: "
      )
    <$> parseNixFile svcsFile emptyServiceFileContents

emptyServiceFileContents :: NixExpr
emptyServiceFileContents =
  NixFunctionDecl (NixFunction ["config", "pkgs", "..."] (NixSet mempty))

writeServiceFile :: NixExpr -> IO ()
writeServiceFile e = do
  svcsFile <- locateServicesFile
  writeNixFile svcsFile e
