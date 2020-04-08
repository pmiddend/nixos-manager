{-|
  Description: Types representing a Nix package (as read from @nix search@)
  -}
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
import           NixManager.NixLocation         ( NixLocation(NixLocation)
                                                , locationFromText
                                                )

-- | Type representing a Nix package, along with information about its current status
data NixPackage = NixPackage {
    _npName :: Text -- ^ Name of the package (as per the JSON)
  , _npPath :: NixLocation -- ^ Package path (as per the JSON)
  , _npVersion :: Text -- ^ Package version (as per the JSON)
  , _npDescription :: Text -- ^ Package description (as per the JSON)
  , _npStatus :: NixPackageStatus -- ^ Current status of the package (will be added after parsing the JSON)
  } deriving(Eq,Show)

makeLenses ''NixPackage

-- | Read a package list from a 'ByteString'
readPackagesJson :: ByteString -> TextualError [NixPackage]
readPackagesJson = (packagesFromMap <$>) . fromEither . eitherDecode

-- | Convert a map (like the one @nix search@ returns) into a package list
packagesFromMap :: Map Text NixPackageMeta -> [NixPackage]
packagesFromMap m =
  (\(path, meta) -> NixPackage (meta ^. npmName)
                               (locationFromText path)
                               (meta ^. npmVersion)
                               (meta ^. npmDescription)
                               NixPackageNothing
    )
    <$> toList m
