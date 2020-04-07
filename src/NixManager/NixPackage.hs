{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.NixPackage
  ( NixPackage(..)
  , npName
  , npPath
  , npVersion
  , npDescription
  , npStatus
  , readPackagesJson
  )
where

import           NixManager.Util                ( TextualError
                                                , fromEither
                                                )
import           Data.ByteString.Lazy           ( ByteString )
import           Data.Map.Strict                ( Map
                                                , toList
                                                )
import           Data.Text                      ( Text )
import           Control.Lens                   ( makeLenses
                                                , (^.)
                                                )
import           Data.Aeson                     ( eitherDecode )
import           NixManager.NixPackageMeta      ( NixPackageMeta
                                                , npmName
                                                , npmVersion
                                                , npmDescription
                                                )
import           NixManager.NixPackageStatus    ( NixPackageStatus
                                                  ( NixPackageNothing
                                                  )
                                                )
import NixManager.NixLocation(NixLocation(NixLocation), locationFromText)

data NixPackage = NixPackage {
    _npName :: Text
  , _npPath :: NixLocation
  , _npVersion :: Text
  , _npDescription :: Text
  , _npStatus :: NixPackageStatus
  } deriving(Eq,Show)

makeLenses ''NixPackage

readPackagesJson :: ByteString -> TextualError [NixPackage]
readPackagesJson = (packagesFromMap <$>) . fromEither . eitherDecode

packagesFromMap :: Map Text NixPackageMeta -> [NixPackage]
packagesFromMap m =
  (\(path, meta) -> NixPackage (meta ^. npmName)
                               (locationFromText path)
                               (meta ^. npmVersion)
                               (meta ^. npmDescription)
                               NixPackageNothing
    )
    <$> toList m
