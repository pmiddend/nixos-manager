{-# LANGUAGE OverloadedStrings #-}
module NixManager.Admin.ValidRebuildModes
  ( validRebuildModesWithDescription
  , validRebuildModes
  , descriptionForValidRebuildMode
  , validRebuildModeIdx
  )
where

import Data.Maybe(fromJust)
import Control.Lens(Iso', iso)
import Data.List(elemIndex)
import           NixManager.NixRebuildMode      ( NixRebuildMode
                                                  ( NixRebuildSwitch
                                                  , NixRebuildBoot
                                                  , NixRebuildTest
                                                  , NixRebuildDryBuild
                                                  , NixRebuildDryActivate
                                                  )
                                                )
import           Data.Text                      ( Text )

descriptionForValidRebuildMode :: NixRebuildMode -> Maybe Text
descriptionForValidRebuildMode m = lookup m validRebuildModesWithDescription

validRebuildModesWithDescription :: [(NixRebuildMode, Text)]
validRebuildModesWithDescription =
  [ ( NixRebuildSwitch
    , "Build and activate the changes immediately. You can go back to previous configurations by rebooting and selecting an older generation."
    )
  -- , ( NixRebuildBoot
  --   , "Build the new configuration and make it the boot default, but do not activate it. That is, the system continues to run the previous configuration until the next reboot."
  --   )
  -- , ( NixRebuildTest
  --   , "Build and activate the new configuration, but do not add it to the GRUB boot menu. Thus, if you reboot the system (or if it crashes), you will automatically revert to the default configuration (i.e. the configuration resulting from the last rebuild)."
  --   )
  , ( NixRebuildDryBuild
    , "Show what store paths would be built or downloaded by any of the operations above, but otherwise do nothing."
    )
  , ( NixRebuildDryActivate
    , "Build the new configuration, but instead of activating it, show what changes would be performed by the activation. For instance, this command will print which systemd units would be restarted. The list of changes is not guaranteed to be complete."
    )
  ]

validRebuildModes :: [NixRebuildMode]
validRebuildModes = fst <$> validRebuildModesWithDescription

validRebuildModeIdx :: Iso' NixRebuildMode Int
validRebuildModeIdx = iso (fromJust . (`elemIndex` validRebuildModes)) (validRebuildModes !!)
