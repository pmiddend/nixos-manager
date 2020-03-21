{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module NixManager.NixService
  ( NixService
  , serviceLoc
  , readServices
  , writeServiceFile
  , serviceOptions
  , makeServices
  )
where

import           System.Directory               ( doesFileExist )
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
                                                , showText
                                                , MaybeError(Success)
                                                , addToError
                                                , predAnd
                                                , fromEither
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

locateServicesFile :: IO (Maybe FilePath)
locateServicesFile = do
  fileExists <- doesFileExist "services.nix" -- TODO: check $XDG_CONFIG_DIRS here
  if fileExists then pure (Just "services.nix") else pure Nothing


readServiceFile :: FilePath -> IO (MaybeError NixExpr)
readServiceFile file =
  addToError
      ("Error parsing the \""
      <> showText file
      <> "\" file. This is most likely a syntax error, please investigate the file itself and fix the error. Then restart nixos-manager. The error was: "
      )
    .   fromEither

    <$> parseNixFile "services.nix"

emptyServiceFileContents :: NixExpr
emptyServiceFileContents =
  NixFunctionDecl (NixFunction ["config", "pkgs", "..."] (NixSet mempty))

readServices :: IO (MaybeError NixExpr)
readServices = do
  servicesFile <- locateServicesFile
  case servicesFile of
    Just file -> readServiceFile file
    Nothing   -> pure (Success emptyServiceFileContents)

writeServiceFile :: NixExpr -> IO ()
writeServiceFile = writeNixFile "services.nix"
