{-|
  Description: Contains the event type for all events corresponding to the Administration tab

Contains the event type for all events corresponding to the Administration tab
  -}
module NixManager.Admin.Event
  ( Event(..)
  )
where

import           NixManager.Process             ( ProcessData
                                                , ProcessOutput
                                                )
import           NixManager.View.DetailsState   ( DetailsState )
import           System.Exit                    ( ExitCode )
import           NixManager.ChangeType          ( ChangeType )
import           NixManager.Password            ( Password )

data Event = EventRebuild -- ^ Triggered by the Rebuild button. Starts the password query.
           | EventRebuildWithPassword Password -- ^ Triggered by the “ask for password” process after it completes. It launches the actual rebuild process.
           | EventAskPassWatch (Password -> Event) ProcessOutput ProcessData -- ^ This event is emitted at regular intervals after the password query is launched. When the process finishes (successfully), there’s a CPS-style argument to see what to emit next.
           | EventRebuildStarted ProcessData Password -- ^ Is emitted after the rebuild has started and initiates the “watchdog” event above.
           | EventRebuildWatch Password ProcessOutput ProcessData -- ^ This event is emitted at regular intervals after the rebuild process is launched.
           | EventRebuildFinished ProcessOutput ExitCode -- ^ Emitted when the rebuild process finishes.
           | EventRebuildModeIdxChanged Int -- ^ Emitted when the rebuild mode changes
           | EventRebuildDoUpdateChanged Bool -- ^ Emitted when the “Update” checkbox changes
           | EventRebuildDoRollbackChanged Bool -- ^ Emitted when the “Rollback” checkbox changes
           | EventRebuildCancel -- ^ Emitted when the user clicks Cancel on a running rebuild
           | EventRebuildChangeDetails DetailsState -- ^ Emitted when the rebuild details are expanded/contracted
           | EventGarbageChangeDetails DetailsState -- ^ Emitted when the garbage details are expanded/contracted
           | EventGarbage -- ^ Triggered by the Collect Garbage button. Starts the password query.
           | EventGarbageWithPassword Password -- ^ Triggered by the “ask for password” process after it completes. It launches the actual garbage collection process.
           | EventGarbageStarted ProcessData Password -- ^ Is emitted after the garbage collection has started and initiates the “watchdog” event.
           | EventGarbageWatch ProcessOutput ProcessData -- ^ This event is emitted at regular intervals after the garbage collect process is launched.
           | EventGarbageFinished ProcessOutput ExitCode -- ^ Emitted when the garbage collection process finishes.
           | EventGarbageOlderGenerationsChanged Bool -- ^ Emitted when the “Older Generations” checkbox changes
           | EventGarbageCancel -- ^ Emitted when the user clicks Cancel on a running garbage collection
           | EventReload -- ^ Triggers a reload of the “Do we have changes” state
           | EventReloadFinished ChangeType -- ^ Emitted when we’ve determined the changes
