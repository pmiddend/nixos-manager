{-|
  Description: Contains the event type for all events corresponding to the home-manager Administration tab

Contains the event type for all events corresponding to the home-manager Administration tab
  -}
module NixManager.HMAdmin.Event
  ( Event(..)
  )
where

import           NixManager.ChangeType          ( ChangeType )
import           NixManager.Process             ( ProcessData
                                                , ProcessOutput
                                                )
import           NixManager.View.DetailsState   ( DetailsState )
import           System.Exit                    ( ExitCode )
import qualified NixManager.HMAdmin.GenerationsView
                                               as GenerationsView

data Event = EventRebuild -- ^ Triggered by the Rebuild button. Starts the home-manager rebuild process.
           | EventRebuildStarted ProcessData -- ^ Is emitted after the rebuild has started and initiates the “watchdog” event.
           | EventRebuildWatch ProcessOutput ProcessData -- ^ This event is emitted at regular intervals after the rebuild process is launched.
           | EventRebuildFinished ProcessOutput ExitCode -- ^ Emitted when the rebuild process finishes.
           | EventRebuildModeIdxChanged Int -- ^ Emitted when the rebuild mode changes
           | EventRebuildCancel -- ^ Emitted when the user clicks Cancel on a running rebuild
           | EventRebuildChangeDetails DetailsState -- ^ Emitted when the rebuild details are expanded/contracted
           | EventGarbageChangeDetails DetailsState -- ^ Emitted when the garbage details are expanded/contracted
           | EventGarbage -- ^ Triggered by the Collect Garbage button. Starts the garbage collection.
           | EventGarbageStarted ProcessData -- ^ Is emitted after the garbage collection has started and initiates the “watchdog” event.
           | EventGarbageWatch ProcessOutput ProcessData -- ^ This event is emitted at regular intervals after the garbage collect process is launched.
           | EventGarbageFinished ProcessOutput ExitCode -- ^ Emitted when the garbage collection process finishes.
           | EventGarbageCancel -- ^ Emitted when the user clicks Cancel on a running garbage collection
           | EventReload -- ^ Even that’s fired when we need to re-check if there are changes to be applied
           | EventReloadFinished ChangeType -- ^ Is fired when the results of aforementioned changes check are in
           | EventGenerations GenerationsView.Event -- ^ Wrapper event for the home-manager generations view “widget”
