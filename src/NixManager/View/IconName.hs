{-|
  Description: An enum wrapping GTK’s icon name values

An enum wrapping GTK’s icon name values
  -}
module NixManager.View.IconName
  ( IconName(..)
  , nameToGtk
  )
where

import           Data.Text                      ( Text )
import           NixManager.Util                ( showText
                                                , kebapize
                                                )

-- | An enum wrapping GTK’s icon name values
data IconName = SystemRun
              | SystemSoftwareInstall
              | PreferencesOther
              | PackageXGeneric
              | EmblemImportant
              | EmblemDocuments
              | EmblemDownloads
              | EmblemDefault
              | DriveMultidisk
              | DialogError
              | DialogQuestion
              | DialogInformation
              | UserTrash
              | ApplicationsSystem
              | EditDelete
              | EditClear
              | ProcessStop
              | ViewRefresh
              | UserHome
              deriving(Eq, Show)

-- | Convert the enum to a GTK-compatible string
nameToGtk :: IconName -> Text
nameToGtk = kebapize mempty . showText
