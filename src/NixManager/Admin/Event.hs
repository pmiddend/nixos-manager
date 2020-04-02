module NixManager.Admin.Event
  ( Event(..)
  )
where

import           NixManager.Process             ( ProcessData
                                                , ProcessOutput
                                                )
import           NixManager.Admin.DetailsState  ( DetailsState )
import           System.Exit                    ( ExitCode )
import           NixManager.Changes             ( ChangeType )
import           Data.Text                      ( Text )


data Event = EventRebuild
           | EventRebuildWithPassword Text
           | EventAskPassWatch (Text -> Event) ProcessOutput ProcessData
           | EventRebuildStarted ProcessData
           | EventRebuildWatch ProcessOutput ProcessData
           | EventRebuildFinished ProcessOutput ExitCode
           | EventRebuildModeChanged Text
           | EventRebuildDoUpdateChanged Bool
           | EventRebuildDoRollbackChanged Bool
           | EventRebuildCancel
           | EventRebuildChangeDetails DetailsState
           | EventGarbageChangeDetails DetailsState
           | EventGarbage
           | EventGarbageWithPassword Text
           | EventGarbageStarted ProcessData
           | EventGarbageWatch ProcessOutput ProcessData
           | EventGarbageFinished ProcessOutput ExitCode
           | EventGarbageOlderGenerationsChanged Bool
           | EventGarbageCancel
           | EventReload
           | EventReloadFinished ChangeType
