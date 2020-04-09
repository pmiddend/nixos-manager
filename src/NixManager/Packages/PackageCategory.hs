{-
  Description: Contains the enum for the package category combobox

Contains the enum for the package category combobox
  -}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.Packages.PackageCategory
  ( PackageCategory(..)
  , packageCategories
  , categoryToText
  , categoryIdx
  )
where

import           Data.List                      ( elemIndex )
import           Data.Maybe                     ( fromJust )
import           Data.Text                      ( Text )
import           Control.Lens                   ( Iso'
                                                , iso
                                                )

-- | All the choices for the category combobox
data PackageCategory = PackageCategoryAll -- ^ Display all packages
                     | PackageCategoryInstalled -- ^ Display only installed packages
                     | PackageCategoryPendingInstall -- ^ Display only pending installation packages
                     | PackageCategoryPendingUninstall -- ^ Display only pending uninstallation packages
                     deriving(Eq, Bounded, Enum)

-- | List of all package categories
packageCategories :: [PackageCategory]
packageCategories = [minBound .. maxBound]

-- | Prettyprint a category
categoryToText :: PackageCategory -> Text
categoryToText PackageCategoryAll              = "All"
categoryToText PackageCategoryInstalled        = "Installed"
categoryToText PackageCategoryPendingInstall   = "Marked for install"
categoryToText PackageCategoryPendingUninstall = "Marked for uninstall"

-- | Isomorphism between a category and its index in the list of all categories (needed for the combobox logic)
categoryIdx :: Iso' PackageCategory Int
categoryIdx =
  iso (fromJust . (`elemIndex` packageCategories)) (packageCategories !!)
