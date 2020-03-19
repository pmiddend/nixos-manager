{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.NixPackage
  ( NixPackage(..)
  , npName
  , npVersion
  , npDescription
  , npInstalled
  )
where

import           Control.Monad                  ( mzero )
import           Data.Text                      ( Text )
import           Control.Lens                   ( makeLenses )
import           Data.Aeson                     ( FromJSON
                                                , Value(Object)
                                                , parseJSON
                                                , (.:)
                                                )

data NixPackage = NixPackage {
    _npName :: Text
  , _npVersion :: Text
  , _npDescription :: Text
  , _npInstalled :: Bool
  } deriving(Eq,Show)

makeLenses ''NixPackage

instance FromJSON NixPackage where
  parseJSON (Object v) =
    NixPackage
      <$> v
      .:  "pkgName"
      <*> v
      .:  "version"
      <*> v
      .:  "description"
      <*> pure False
  parseJSON _ = mzero

