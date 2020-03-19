{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module NixManager.ManagerState
  ( msSearchString
  , msSearchResult
  , msSelectedPackage
  , msLatestMessage
  , msServiceCache
  , msPackageCache
  , msSelectedServiceIdx
  , msServiceExpression
  , msInstallingPackage
  , msSelectedPackageIdx
  , ManagerState(..)
  )
where

import           NixManager.NixExpr             ( NixExpr )
import           NixManager.NixService          ( NixService )
import           NixManager.NixPackage          ( NixPackage
                                                , npName
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
   , _msSelectedServiceIdx :: Maybe Int
   , _msServiceExpression :: NixExpr
   } deriving(Show)

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
