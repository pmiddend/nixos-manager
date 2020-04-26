{-|
  Description: Types representing a Nix package (as read from @nix search@)
Types representing a Nix package (as read from @nix search@)
  -}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
module NixManager.NixPackage
  ( NixPackage(..)
  , readPackagesJson
  )
where

import           GHC.Generics                   ( Generic )
import           NixManager.Util                ( TextualError
                                                , fromStringEither
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
import           NixManager.NixPackageMeta      ( NixPackageMeta )
import           NixManager.NixPackageStatus    ( NixPackageStatus
                                                  ( NixPackageNothing
                                                  )
                                                )
import           NixManager.NixLocation         ( NixLocation
                                                , locationFromText
                                                )
import           Data.List                      ( sortOn )
import           Data.Generics.Labels           ( )

-- | Type representing a Nix package, along with information about its current status
data NixPackage = NixPackage {
    name :: Text -- ^ Name of the package (as per the JSON)
  , path :: NixLocation -- ^ Package path (as per the JSON)
  , version :: Text -- ^ Package version (as per the JSON)
  , description :: Text -- ^ Package description (as per the JSON)
  , status :: NixPackageStatus -- ^ Current status of the package (will be added after parsing the JSON)
  } deriving(Eq,Show, Generic)

-- | Read a package list from a 'ByteString'
readPackagesJson :: ByteString -> TextualError [NixPackage]
readPackagesJson =
  (sortOn name <$>) . (packagesFromMap <$>) . fromStringEither . eitherDecode

-- | Convert a map (like the one @nix search@ returns) into a package list
packagesFromMap :: Map Text NixPackageMeta -> [NixPackage]
packagesFromMap m =
  (\(path', meta) -> NixPackage (meta ^. #name)
                                (locationFromText path')
                                (meta ^. #version)
                                (meta ^. #description)
                                NixPackageNothing
    )
    <$> toList m
