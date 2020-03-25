module NixManager.ServicesEvent
  ( ServicesEvent(..)
  )
where

import           Data.Text                      ( Text )
import           Control.Lens                   ( makePrisms )
import           NixManager.Message             ( Message )
import           NixManager.NixPackage          ( NixPackage )
import           NixManager.NixExpr             ( NixExpr )
import           NixManager.Util                ( Endo )
import           NixManager.AdminEvent          ( AdminEvent )
import           NixManager.ServiceDownload     ( ServiceDownloadState )
import           NixManager.ServiceState        ( ServiceState )


data ServicesEvent = ServicesEventDownloadStart
                   | ServicesEventSelected (Maybe Int)
                   | ServicesEventDownloadCheck ServiceDownloadState
                   | ServicesEventDownloadStarted ServiceDownloadState
                   | ServicesEventDownloadCancel
                   | ServicesEventStateResult ServiceState
                   | ServicesEventStateReload
                   | ServicesEventSettingChanged (Endo NixExpr)

