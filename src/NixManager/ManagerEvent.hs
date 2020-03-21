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
import           NixManager.Util                ( Endo )
import           NixManager.ServiceDownload     ( ServiceDownloadState )
import           NixManager.ServiceState        ( ServiceState )

data ManagerEvent = ManagerEventClosed
      | ManagerEventSearchChanged Text
      | ManagerEventPackageSelected (Maybe Int)
      | ManagerEventServiceSelected (Maybe Int)
      | ManagerEventInstall
      | ManagerEventServiceDownloadStart
      | ManagerEventServiceDownloadCheck ServiceDownloadState
      | ManagerEventServiceDownloadStarted ServiceDownloadState
      | ManagerEventServiceDownloadCancel
      | ManagerEventServiceStateResult ServiceState
      | ManagerEventServiceStateReload
      | ManagerEventInstallCompleted [NixPackage]
      | ManagerEventUninstallCompleted [NixPackage]
      | ManagerEventUninstall
      | ManagerEventSettingChanged (Endo NixExpr)
      | ManagerEventTryInstall
      | ManagerEventShowMessage Message
      | ManagerEventDiscard

makePrisms ''ManagerEvent
