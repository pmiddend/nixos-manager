{-# LANGUAGE TemplateHaskell #-}
module NixManager.Packages.State
  ( State
  , psSearchString
  , psSearchResult
  , psSelectedPackage
  , psLatestMessage
  , psInstallingPackage
  , psPackageCache
  , psSelectedIdx
  , initState
  , isProcessData
  , InstallingState(InstallingState)
  , isPackage
  , isCounter
  )
where

import           NixManager.Process             ( ProcessData )
import           NixManager.PackageSearch       ( readCache )
import           NixManager.Util                ( MaybeError(Success)
                                                , ifSuccessIO
                                                )
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

data InstallingState = InstallingState {
  _isPackage :: NixPackage
  , _isCounter :: Int
  , _isProcessData :: ProcessData
  }

makeLenses ''InstallingState

data State = State {
    _psPackageCache :: [NixPackage]
  , _psSearchString :: Text
  , _psSelectedIdx :: Maybe Int
  , _psInstallingPackage :: Maybe InstallingState
  , _psLatestMessage :: Maybe Message
  }

makeLenses ''State

packageMatches :: Text -> NixPackage -> Bool
packageMatches t p = toLower t `isInfixOf` (p ^. npName . to toLower)

psSearchResult :: Getter State [NixPackage]
psSearchResult = to
  (\s -> s ^.. psPackageCache . folded . filtered
    (packageMatches (s ^. psSearchString))
  )

psSelectedPackage :: Getter State (Maybe NixPackage)
psSelectedPackage = to f
 where
  f s = case s ^. psSelectedIdx of
    Nothing     -> Nothing
    Just pkgIdx -> s ^? psSearchResult . ix pkgIdx

initState :: IO (MaybeError State)
initState = ifSuccessIO readCache $ \cache -> pure $ Success $ State
  { _psPackageCache      = cache
  , _psSearchString      = mempty
  , _psSelectedIdx       = Nothing
  , _psInstallingPackage = Nothing
  , _psLatestMessage     = Nothing
  }
