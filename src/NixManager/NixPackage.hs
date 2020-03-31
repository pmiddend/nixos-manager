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

import           NixManager.Util                ( MaybeError
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

data NixPackage = NixPackage {
    _npName :: Text
  , _npPath :: Text
  , _npVersion :: Text
  , _npDescription :: Text
  , _npStatus :: NixPackageStatus
  } deriving(Eq,Show)

makeLenses ''NixPackage

readPackagesJson :: ByteString -> MaybeError [NixPackage]
readPackagesJson = (packagesFromMap <$>) . fromEither . eitherDecode

packagesFromMap :: Map Text NixPackageMeta -> [NixPackage]
packagesFromMap m =
  (\(path, meta) -> NixPackage (meta ^. npmName)
                               path
                               (meta ^. npmVersion)
                               (meta ^. npmDescription)
                               NixPackageNothing
    )
    <$> toList m
