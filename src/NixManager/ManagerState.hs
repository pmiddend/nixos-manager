{-# LANGUAGE TemplateHaskell #-}
module NixManager.ManagerState where

import           NixManager.Nix                 ( NixPackage )
import           Data.Text                      ( Text )
import           Control.Lens                   ( makeLenses )

data ManagerState = ManagerState {
     _msPackageCache :: [NixPackage]
   , _msSearchString :: Text
   , _msPackageSearchResult :: [NixPackage]
   , _msSelectedPackage :: Maybe NixPackage
   , _msInstallingPackage :: Maybe NixPackage
   , _msLatestError :: Maybe Text
   } deriving(Eq,Show)

makeLenses ''ManagerState
