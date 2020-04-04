{-# LANGUAGE TemplateHaskell #-}
module NixManager.Packages.State
  ( State
  , psSearchString
  , psSearchResult
  , psSelectedPackage
  , psLatestMessage
  , psInstallingPackage
  , psPackageCache
  , psCategory
  , psSelectedIdx
  , psCategoryIdx
  , initState
  , isProcessData
  , InstallingState(InstallingState)
  , isPackage
  , isCounter
  )
where

import           NixManager.Process             ( ProcessData )
import           NixManager.NixPackages         ( readPackageCache )
import           NixManager.Util                ( TextualError
                                                , ifSuccessIO
                                                )
import           NixManager.NixPackageStatus    ( NixPackageStatus
                                                  ( NixPackageInstalled
                                                  , NixPackagePendingInstall
                                                  , NixPackagePendingUninstall
                                                  )
                                                )
import           NixManager.NixPackage          ( NixPackage
                                                , npName
                                                , npStatus
                                                )
import           NixManager.Message
import           Control.Lens                   ( makeLenses
                                                , folded
                                                , filtered
                                                , Lens'
                                                , from
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
import           NixManager.Packages.PackageCategory
                                                ( PackageCategory
                                                  ( PackageCategoryAll
                                                  , PackageCategoryInstalled
                                                  , PackageCategoryPendingInstall
                                                  , PackageCategoryPendingUninstall
                                                  )
                                                , categoryIdx
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
  , _psCategoryIdx :: Int
  }

makeLenses ''State

psCategory :: Lens' State PackageCategory
psCategory = psCategoryIdx . from categoryIdx

packageMatches :: Text -> NixPackage -> Bool
packageMatches t p = toLower t `isInfixOf` (p ^. npName . to toLower)

packageMatchesCategory :: PackageCategory -> NixPackage -> Bool
packageMatchesCategory PackageCategoryAll _ = True
packageMatchesCategory PackageCategoryInstalled pkg =
  (pkg ^. npStatus) == NixPackageInstalled
packageMatchesCategory PackageCategoryPendingInstall pkg =
  (pkg ^. npStatus) == NixPackagePendingInstall
packageMatchesCategory PackageCategoryPendingUninstall pkg =
  (pkg ^. npStatus) == NixPackagePendingUninstall

psSearchResult :: Getter State [NixPackage]
psSearchResult = to
  (\s ->
    s
      ^.. psPackageCache
      .   folded
      .   filtered (packageMatches (s ^. psSearchString))
      .   filtered (packageMatchesCategory (s ^. psCategory))
  )

psSelectedPackage :: Getter State (Maybe NixPackage)
psSelectedPackage = to f
 where
  f s = case s ^. psSelectedIdx of
    Nothing     -> Nothing
    Just pkgIdx -> s ^? psSearchResult . ix pkgIdx

initState :: IO (TextualError State)
initState = ifSuccessIO readPackageCache $ \cache -> pure $ Right $ State
  { _psPackageCache      = cache
  , _psSearchString      = mempty
  , _psSelectedIdx       = Nothing
  , _psInstallingPackage = Nothing
  , _psLatestMessage     = Nothing
  , _psCategoryIdx       = 0
  }
