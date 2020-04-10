{-|
  Description: Contains the service category comboxbox values

Contains the service category comboxbox values
  -}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.Services.ServiceCategory
  ( ServiceCategory(..)
  , categoryToText
  , categoryToNixPrefix
  , serviceCategories
  , serviceCategoryIdx
  )
where

import           Data.Maybe                     ( fromJust )
import           Data.Text                      ( Text )
import           Control.Lens                   ( Iso'
                                                , iso
                                                )
import           Data.List                      ( elemIndex )

-- | All values for the service category combobox
data ServiceCategory = ServiceCategoryServices
                     | ServiceCategoryHardware
                     | ServiceCategoryPrograms
                     | ServiceCategoryBoot
                     | ServiceCategoryNix
                     deriving(Enum, Bounded, Eq)

-- | Prettyprint the category
categoryToText :: ServiceCategory -> Text
categoryToText ServiceCategoryServices = "Services"
categoryToText ServiceCategoryPrograms = "Programs"
categoryToText ServiceCategoryHardware = "Hardware"
categoryToText ServiceCategoryBoot     = "Boot"
categoryToText ServiceCategoryNix      = "Nix"

-- | Conver a category to its option prefix
categoryToNixPrefix :: ServiceCategory -> Text
categoryToNixPrefix ServiceCategoryServices = "services"
categoryToNixPrefix ServiceCategoryPrograms = "programs"
categoryToNixPrefix ServiceCategoryHardware = "hardware"
categoryToNixPrefix ServiceCategoryBoot     = "boot"
categoryToNixPrefix ServiceCategoryNix      = "nix"

-- | List of all the service categories
serviceCategories :: [ServiceCategory]
serviceCategories = [minBound .. maxBound]

-- | Isomorphism between a category and its index in the list of all categories (needed for the combobox logic)
serviceCategoryIdx :: Iso' ServiceCategory Int
serviceCategoryIdx =
  iso (fromJust . (`elemIndex` serviceCategories)) (serviceCategories !!)
