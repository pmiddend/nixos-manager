{-# LANGUAGE TemplateHaskell #-}
module NixManager.ManagerEvent where

import           Data.Text                      ( Text )
import           Control.Lens                   ( makePrisms )
import           NixManager.Message
import           NixManager.Nix                 ( NixPackage )

data ManagerEvent = ManagerEventClosed
      | ManagerEventSearchChanged Text
      | ManagerEventPackageSelected (Maybe Int)
      | ManagerEventServiceSelected (Maybe Int)
      | ManagerEventInstall
      | ManagerEventInstallCompleted [NixPackage]
      | ManagerEventUninstallCompleted [NixPackage]
      | ManagerEventUninstall
      | ManagerEventTryInstall
      | ManagerEventShowMessage Message
      | ManagerEventDiscard
      deriving(Eq,Show)

makePrisms ''ManagerEvent
