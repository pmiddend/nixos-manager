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

data PackageCategory = PackageCategoryAll
                     | PackageCategoryInstalled
                     | PackageCategoryPendingInstall
                     | PackageCategoryPendingUninstall
                     deriving(Eq, Bounded, Enum)

packageCategories :: [PackageCategory]
packageCategories = [minBound .. maxBound]

categoryToText :: PackageCategory -> Text
categoryToText PackageCategoryAll              = "All"
categoryToText PackageCategoryInstalled        = "Installed"
categoryToText PackageCategoryPendingInstall   = "Marked for install"
categoryToText PackageCategoryPendingUninstall = "Marked for uninstall"

categoryIdx :: Iso' PackageCategory Int
categoryIdx =
  iso (fromJust . (`elemIndex` packageCategories)) (packageCategories !!)
