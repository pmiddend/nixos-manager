{-# LANGUAGE TemplateHaskell #-}
module NixManager.ManagerEvent
  ( ManagerEvent(..)
  )
where

import           Data.Text                      ( Text )
import           Control.Lens                   ( makePrisms )
import           NixManager.Message             ( Message )
import           NixManager.ServicesEvent       ( ServicesEvent )
import           NixManager.NixPackage          ( NixPackage )
import           NixManager.AdminEvent          ( AdminEvent )

data ManagerEvent = ManagerEventClosed
      | ManagerEventSearchChanged Text
      | ManagerEventPackageSelected (Maybe Int)
      | ManagerEventInstall
      | ManagerEventInstallCompleted [NixPackage]
      | ManagerEventUninstallCompleted [NixPackage]
      | ManagerEventUninstall
      | ManagerEventTryInstall
      | ManagerEventShowMessage Message
      | ManagerEventDiscard
      | ManagerEventAdmin AdminEvent
      | ManagerEventServices ServicesEvent

makePrisms ''ManagerEvent
