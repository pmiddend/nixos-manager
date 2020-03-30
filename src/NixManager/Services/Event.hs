module NixManager.Services.Event
  ( Event(..)
  )
where

import           NixManager.NixExpr             ( NixExpr )
import           NixManager.Util                ( Endo )
import           NixManager.Services.Download   ( DownloadState )
import           NixManager.Services.State      ( State )
import           Data.Text                      ( Text )
import           NixManager.Services.ServiceCategory
                                                ( ServiceCategory )

data Event = EventDownloadStart
           | EventSelected (Maybe Int)
           | EventDownloadCheck DownloadState
           | EventDownloadStarted DownloadState
           | EventDownloadCancel
           | EventStateResult State
           | EventStateReload
           | EventSearchChanged Text
           | EventSettingChanged (Endo NixExpr)
           | EventCategoryChanged ServiceCategory

