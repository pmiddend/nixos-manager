{-# LANGUAGE TemplateHaskell #-}
module NixManager.ManagerEvent
  ( ManagerEvent(..)
  )
where

import           Data.Text                      ( Text )
import           Control.Lens                   ( makePrisms )
import           NixManager.Message             ( Message )
import           NixManager.NixPackage          ( NixPackage )
import           NixManager.NixExpr             ( NixExpr )

data ManagerEvent = ManagerEventClosed
      | ManagerEventSearchChanged Text
      | ManagerEventPackageSelected (Maybe Int)
      | ManagerEventServiceSelected (Maybe Int)
      | ManagerEventInstall
      | ManagerEventInstallCompleted [NixPackage]
      | ManagerEventUninstallCompleted [NixPackage]
      | ManagerEventUninstall
      | ManagerEventSettingChanged Text NixExpr
      | ManagerEventTryInstall
      | ManagerEventShowMessage Message
      | ManagerEventDiscard
      deriving(Show)

makePrisms ''ManagerEvent
