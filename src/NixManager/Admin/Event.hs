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
import           NixManager.Password            ( Password )

data Event = EventRebuild
           | EventRebuildWithPassword Password
           | EventAskPassWatch (Password -> Event) ProcessOutput ProcessData
           | EventRebuildStarted ProcessData Password
           | EventRebuildWatch Password ProcessOutput ProcessData
           | EventRebuildFinished ProcessOutput ExitCode
           | EventRebuildModeIdxChanged Int
           | EventRebuildDoUpdateChanged Bool
           | EventRebuildDoRollbackChanged Bool
           | EventRebuildCancel
           | EventRebuildChangeDetails DetailsState
           | EventGarbageChangeDetails DetailsState
           | EventGarbage
           | EventGarbageWithPassword Password
           | EventGarbageStarted ProcessData Password
           | EventGarbageWatch ProcessOutput ProcessData
           | EventGarbageFinished ProcessOutput ExitCode
           | EventGarbageOlderGenerationsChanged Bool
           | EventGarbageCancel
           | EventReload
           | EventReloadFinished ChangeType
