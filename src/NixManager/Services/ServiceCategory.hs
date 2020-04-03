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

data ServiceCategory = ServiceCategoryServices
                     | ServiceCategoryHardware
                     | ServiceCategoryPrograms
                     | ServiceCategoryBoot
                     | ServiceCategoryNix
                     deriving(Enum, Bounded, Eq)

categoryToText :: ServiceCategory -> Text
categoryToText ServiceCategoryServices = "Services"
categoryToText ServiceCategoryPrograms = "Programs"
categoryToText ServiceCategoryHardware = "Hardware"
categoryToText ServiceCategoryBoot     = "Boot"
categoryToText ServiceCategoryNix      = "Nix"

categoryToNixPrefix :: ServiceCategory -> Text
categoryToNixPrefix ServiceCategoryServices = "services"
categoryToNixPrefix ServiceCategoryPrograms = "programs"
categoryToNixPrefix ServiceCategoryHardware = "hardware"
categoryToNixPrefix ServiceCategoryBoot     = "boot"
categoryToNixPrefix ServiceCategoryNix      = "nix"

serviceCategories :: [ServiceCategory]
serviceCategories = [minBound .. maxBound]

serviceCategoryIdx :: Iso' ServiceCategory Int
serviceCategoryIdx =
  iso (fromJust . (`elemIndex` serviceCategories)) (serviceCategories !!)
