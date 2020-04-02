module NixManager.View.IconName
  ( IconName(..)
  , nameToGtk
  )
where

import           Data.Text                      ( Text )
import           NixManager.Util                ( showText
                                                , kebapize
                                                )

data IconName = SystemRun
              | SystemSoftwareInstall
              | PreferencesOther
              | PackageXGeneric
              | EmblemImportant
              | DialogQuestion
              | DialogInformation
              | ApplicationsSystem
              | EditDelete
              | EditClear
              | ProcessStop
              | ViewRefresh
              deriving(Eq, Show)

nameToGtk :: IconName -> Text
nameToGtk = kebapize . showText
