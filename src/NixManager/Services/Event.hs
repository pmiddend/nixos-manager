{-|
  Description: Contains the event type for all events corresponding to the Services tab
Contains the event type for all events corresponding to the Services tab
  -}
module NixManager.Services.Event
  ( Event(..)
  )
where

import           NixManager.NixExpr             ( NixExpr )
import           NixManager.Util                ( Endo )
import           NixManager.Services.Download   ( DownloadState )
import           NixManager.Services.State      ( State )
import           Data.Text                      ( Text )

data Event = EventDownloadStart -- ^ Triggered when the user presses the “Start download” button. The next event will be the 'EventDownloadStarted' event.
           | EventSelected (Maybe Int) -- ^ Triggered when the current service selection changes
           | EventDownloadCheck DownloadState -- ^ Triggered regularly while the download is in progress to check if it’s finished and to “pulse” the progress bar
           | EventDownloadStarted DownloadState -- ^ Triggered just after the download has begun
           | EventDownloadCancel -- ^ Triggered when the user presses the Cancel button on a running download
           | EventStateResult State -- ^ Triggered when the download has finished and the results are in
           | EventStateReload -- ^ This is triggered externally whenever we need to reload the cache. For example, when the download has finished.
           | EventSearchChanged Text -- ^ Triggered when the search string changes
           | EventSettingChanged (Endo NixExpr) -- ^ Triggered whenever we change a service setting. Contains an endomorphism that changes the service Nix expression
           | EventCategoryIdxChanged Int -- ^ Triggered when the service category combobox changes

