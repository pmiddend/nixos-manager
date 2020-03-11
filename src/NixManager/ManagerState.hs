{-# LANGUAGE TemplateHaskell #-}
module NixManager.ManagerState where

import           NixManager.Nix                 ( NixPackage )
import           Data.Text                      ( Text )
import           Control.Lens                   ( makeLenses )

data ManagerState = ManagerState {
     _msPackageCache :: [NixPackage]
   , _msSearchString :: Text
   } deriving(Eq,Show)

makeLenses ''ManagerState
