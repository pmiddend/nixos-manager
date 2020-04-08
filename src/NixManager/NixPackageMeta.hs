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

data NixPackageMeta = NixPackageMeta {
    _npmName :: Text
  , _npmVersion :: Text
  , _npmDescription :: Text
  } deriving(Eq,Show)

makeLenses ''NixPackageMeta

instance FromJSON NixPackageMeta where
  parseJSON (Object v) =
    NixPackageMeta <$> v .: "pkgName" <*> v .: "version" <*> v .: "description"
  parseJSON _ = mzero

