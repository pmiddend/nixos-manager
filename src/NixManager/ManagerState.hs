{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module NixManager.ManagerState where

import           NixManager.Nix                 ( NixPackage
                                                , npName
                                                , NixService
                                                )
import           NixManager.Message
import           Control.Lens                   ( makeLenses
                                                , folded
                                                , filtered
                                                , (^.)
                                                , (^..)
                                                , ix
                                                , (^?)
                                                , Getter
                                                , to
                                                )
import           Data.Text                      ( Text
                                                , toLower
                                                , isInfixOf
                                                )

data ManagerState = ManagerState {
     _msPackageCache :: [NixPackage]
   , _msSearchString :: Text
   , _msSelectedPackageIdx :: Maybe Int
   , _msInstallingPackage :: Maybe NixPackage
   , _msLatestMessage :: Maybe Message
   , _msServiceCache :: [NixService]
   } deriving(Eq,Show)

makeLenses ''ManagerState

packageMatches :: Text -> NixPackage -> Bool
packageMatches t p = toLower t `isInfixOf` (p ^. npName . to toLower)

msSearchResult :: Getter ManagerState [NixPackage]
msSearchResult = to
  (\s -> s ^.. msPackageCache . folded . filtered
    (packageMatches (s ^. msSearchString))
  )

msSelectedPackage :: Getter ManagerState (Maybe NixPackage)
msSelectedPackage = to f
 where
  f s = case s ^. msSelectedPackageIdx of
    Nothing     -> Nothing
    Just pkgIdx -> s ^? msSearchResult . ix pkgIdx
