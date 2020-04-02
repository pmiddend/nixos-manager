module NixManager.Admin.Event
  ( Event(..)
  )
where

import           NixManager.Process             ( ProcessData
                                                , ProcessOutput
                                                )
import           NixManager.Admin.State         ( DetailsState )
import           System.Exit                    ( ExitCode )
import           NixManager.Changes             ( ChangeType )
import           Data.Text                      ( Text )


data Event = EventRebuild
           | EventAskPassWatch ProcessOutput ProcessData
           | EventRebuildStarted ProcessData
           | EventRebuildWatch ProcessOutput ProcessData
           | EventRebuildFinished ProcessOutput ExitCode
           | EventRebuildModeChanged Text
           | EventDoUpdateChanged Bool
           | EventDoRollbackChanged Bool
           | EventRebuildCancel
           | EventChangeDetails DetailsState
           | EventReload
           | EventReloadFinished ChangeType
