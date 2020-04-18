{-|
  Description: Contains the event type for all events corresponding to the Services tab
Contains the event type for all events corresponding to the Services tab
  -}
module NixManager.Services.Event
  ( Event(..)
  )
where

import           NixManager.Services.Download   ( DownloadState )
import           NixManager.Services.State      ( State )
import           NixManager.View.ServiceEditView
                                                ( EditViewEvent )

data Event = EventDownloadStart -- ^ Triggered when the user presses the “Start download” button. The next event will be the 'EventDownloadStarted' event.
           | EventEditView EditViewEvent -- ^ Sub-event triggered by the "NixManager.Services.EditView"
           | EventDownloadCheck DownloadState -- ^ Triggered regularly while the download is in progress to check if it’s finished and to “pulse” the progress bar
           | EventDownloadStarted DownloadState -- ^ Triggered just after the download has begun
           | EventDownloadCancel -- ^ Triggered when the user presses the Cancel button on a running download
           | EventStateResult State -- ^ Triggered when the download has finished and the results are in
           | EventStateReload -- ^ This is triggered externally whenever we need to reload the cache. For example, when the download has finished.

