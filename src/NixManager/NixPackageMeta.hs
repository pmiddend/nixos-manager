{-|
  Description: Metadata for a Nix package. This is a companion module to "NixManager.NixPackage"
  -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module NixManager.NixPackageMeta
  ( NixPackageMeta(NixPackageMeta)
  , npmName
  , npmVersion
  , npmDescription
  )
where

import           Control.Monad                  ( mzero )
import           Control.Lens                   ( makeLenses )
import           Data.Text                      ( Text )
import           Data.Aeson                     ( FromJSON
                                                , Value(Object)
                                                , parseJSON
                                                , (.:)
                                                )

-- | Metadata for a Nix package. This is a companion module to "NixManager.NixPackage"
data NixPackageMeta = NixPackageMeta {
    _npmName :: Text -- ^ Package name
  , _npmVersion :: Text -- ^ Package version
  , _npmDescription :: Text -- ^ Package description
  } deriving(Eq,Show)

makeLenses ''NixPackageMeta

instance FromJSON NixPackageMeta where
  parseJSON (Object v) =
    NixPackageMeta <$> v .: "pkgName" <*> v .: "version" <*> v .: "description"
  parseJSON _ = mzero

