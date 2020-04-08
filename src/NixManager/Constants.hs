{-|
  Description: Various constants that are used throughout the application
  -}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.Constants
  ( appName
  , rootManagerPath
  , globalOptionsMagicString
  )
where

import           Data.Text                      ( Text )

-- | The application name, in case that needs to be displayed (it’s used when determining the application’s save paths)
appName :: String
appName = "nixos-manager"

-- | Where to put the configuration files for root. We could create a config file for this sooner or later.
rootManagerPath :: FilePath
rootManagerPath = "/etc/nixos/nixos-manager"

-- | String for those options below a service that don’t have a suffix (like @nix.path@)
globalOptionsMagicString :: Text
globalOptionsMagicString = "Global options"


