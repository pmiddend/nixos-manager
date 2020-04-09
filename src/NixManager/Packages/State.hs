{-|
  Description: Contains all the state for the Packages tab
  -}
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

-- | This is only used when the “Try install” operation is in progress and cumulates all the state pertaining to that
data InstallingState = InstallingState {
    _isPackage :: NixPackage -- ^ Which package is being try-installed
  , _isCounter :: Int  -- ^ This field is necessary to “pulse” the GTK progress bar while installing, see "NixManager.View.ProgressBar" for details
  , _isProcessData :: ProcessData -- ^ The process data
  }

makeLenses ''InstallingState

-- | Contains all the state for the packages tab
data State = State {
    _psPackageCache :: [NixPackage] -- ^ Cache of all available Nix packages
  , _psSearchString :: Text -- ^ Current search string
  , _psSelectedIdx :: Maybe Int -- ^ Currently selected index
  , _psInstallingPackage :: Maybe InstallingState -- ^ Only set if “Try install” is in progress
  , _psLatestMessage :: Maybe Message -- ^ The latest message to display, if any (“Install successful” and stuff)
  , _psCategoryIdx :: Int -- ^ Currently selected category
  }

makeLenses ''State

-- | Isomorphism between a category and its index in the list of all categories (needed for the combobox logic) 
psCategory :: Lens' State PackageCategory
psCategory = psCategoryIdx . from categoryIdx

-- | Whether a package matches the given search string
packageMatches :: Text -> NixPackage -> Bool
packageMatches t p = toLower t `isInfixOf` (p ^. npName . to toLower)

-- | Whether a package matches the given category
packageMatchesCategory :: PackageCategory -> NixPackage -> Bool
packageMatchesCategory PackageCategoryAll _ = True
packageMatchesCategory PackageCategoryInstalled pkg =
  (pkg ^. npStatus) == NixPackageInstalled
packageMatchesCategory PackageCategoryPendingInstall pkg =
  (pkg ^. npStatus) == NixPackagePendingInstall
packageMatchesCategory PackageCategoryPendingUninstall pkg =
  (pkg ^. npStatus) == NixPackagePendingUninstall

-- | Getter for the filtered search result
psSearchResult :: Getter State [NixPackage]
psSearchResult = to
  (\s ->
    s
      ^.. psPackageCache
      .   folded
      .   filtered (packageMatches (s ^. psSearchString))
      .   filtered (packageMatchesCategory (s ^. psCategory))
  )

-- | Getter for the selected package (if any)
psSelectedPackage :: Getter State (Maybe NixPackage)
psSelectedPackage = to f
 where
  f s = case s ^. psSelectedIdx of
    Nothing     -> Nothing
    Just pkgIdx -> s ^? psSearchResult . ix pkgIdx

-- | The initial Packages tab state (needs to read the package cache, hence the side-effect)
initState :: IO (TextualError State)
initState = ifSuccessIO readPackageCache $ \cache -> pure $ Right $ State
  { _psPackageCache      = cache
  , _psSearchString      = mempty
  , _psSelectedIdx       = Nothing
  , _psInstallingPackage = Nothing
  , _psLatestMessage     = Nothing
  , _psCategoryIdx       = 0
  }
