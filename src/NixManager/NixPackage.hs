{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.NixPackage
  ( NixPackage(..)
  , npName
  , npPath
  , npVersion
  , npDescription
  , npInstalled
  , readPackages
  )
where

import           NixManager.Util                ( MaybeError
                                                , fromEither
                                                )
import           Data.ByteString.Lazy           ( ByteString )
import           Data.Map.Strict                ( Map
                                                , toList
                                                )
import           Control.Monad                  ( mzero )
import           Data.Text                      ( Text )
import           Control.Lens                   ( makeLenses
                                                , (^.)
                                                )
import           Data.Aeson                     ( FromJSON
                                                , eitherDecode
                                                , Value(Object)
                                                , parseJSON
                                                , (.:)
                                                )

data NixPackageMeta = NixPackageMeta {
   _npmName :: Text
  , _npmVersion :: Text
  , _npmDescription :: Text
  } deriving(Eq,Show)

makeLenses ''NixPackageMeta

data NixPackage = NixPackage {
    _npName :: Text
  , _npPath :: Text
  , _npVersion :: Text
  , _npDescription :: Text
  , _npInstalled :: Bool
  } deriving(Eq,Show)

makeLenses ''NixPackage

instance FromJSON NixPackageMeta where
  parseJSON (Object v) =
    NixPackageMeta <$> v .: "pkgName" <*> v .: "version" <*> v .: "description"
  parseJSON _ = mzero

readPackages :: ByteString -> MaybeError [NixPackage]
readPackages = (packagesFromMap <$>) . fromEither . eitherDecode

packagesFromMap :: Map Text NixPackageMeta -> [NixPackage]
packagesFromMap m =
  (\(path, meta) -> NixPackage (meta ^. npmName)
                               path
                               (meta ^. npmVersion)
                               (meta ^. npmDescription)
                               False
    )
    <$> toList m
