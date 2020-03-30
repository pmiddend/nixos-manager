{-# LANGUAGE OverloadedStrings #-}
module NixManager.Services.ServiceCategory
  ( ServiceCategory(..)
  , categoryToText
  , categoryToNixPrefix
  , serviceCategories
  )
where

import           Data.Text                      ( Text )

data ServiceCategory = ServiceCategoryServices
                     | ServiceCategoryHardware
                     | ServiceCategoryPrograms
                     | ServiceCategoryBoot
                     | ServiceCategoryNix
                     deriving(Enum, Bounded)

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
