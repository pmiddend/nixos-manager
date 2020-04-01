module NixManager.Admin.Event
  ( Event(..)
  )
where

import           NixManager.Process             ( ProcessData
                                                , ProcessOutput
                                                )
import           Data.Text                      ( Text )
import           NixManager.Admin.State         ( DetailsState )
import           System.Exit                    ( ExitCode )
import           NixManager.Changes             ( ChangeType )

data Event = EventRebuild
           | EventAskPassWatch ProcessOutput ProcessData
           | EventRebuildStarted ProcessData
           | EventRebuildWatch ProcessOutput ProcessData
           | EventRebuildFinished ProcessOutput ExitCode
           | EventRebuildModeChanged Text
           | EventUpdateChanged Bool
           | EventRebuildCancel
           | EventChangeDetails DetailsState
           | EventReload
           | EventReloadFinished ChangeType
