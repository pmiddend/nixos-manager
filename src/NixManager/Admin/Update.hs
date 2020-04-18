{-|
  Description: Contains the update logic for the Administration tab
Contains the update logic for the Administration tab
  -}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module NixManager.Admin.Update
  ( updateEvent
  )
where

import           NixManager.PosixTools          ( kill )
import qualified Data.ByteString.Char8         as BS
import           NixManager.Password            ( Password
                                                  ( Password
                                                  , getPassword
                                                  )
                                                )
import           NixManager.NixGarbage          ( collectGarbage )
import           NixManager.ManagerEvent        ( adminEvent
                                                , ManagerEvent
                                                , pureTransition
                                                , packagesEvent
                                                )
import qualified NixManager.Packages.Event     as PackagesEvent
import           Data.Text.Encoding             ( decodeUtf8
                                                , encodeUtf8
                                                )
import           System.Exit                    ( ExitCode
                                                  ( ExitSuccess
                                                  , ExitFailure
                                                  )
                                                )
import           NixManager.Process             ( updateProcess
                                                , runProcessToFinish
                                                , poResult
                                                , poStdout
                                                , getProcessId
                                                )
import           NixManager.Admin.State         ( State
                                                , changes
                                                , rebuildData
                                                , garbageData
                                                , determineChanges
                                                )
import           NixManager.Admin.GarbageData   ( gdOlderGenerations
                                                , gdBuildState
                                                , gdProcessOutput
                                                , gdDetailsState
                                                )
import           Data.Monoid                    ( getFirst )
import           Control.Lens                   ( (^.)
                                                , from
                                                , traversed
                                                , (<>~)
                                                , (&)
                                                , (?~)
                                                , to
                                                , (.~)
                                                , (+~)
                                                )
import           NixManager.Admin.Event         ( Event
                                                  ( EventRebuild
                                                  , EventRebuildStarted
                                                  , EventReload
                                                  , EventGarbage
                                                  , EventGarbageCancel
                                                  , EventGarbageWatch
                                                  , EventGarbageFinished
                                                  , EventReloadFinished
                                                  , EventRebuildWatch
                                                  , EventRebuildWithPassword
                                                  , EventGarbageOlderGenerationsChanged
                                                  , EventRebuildCancel
                                                  , EventRebuildChangeDetails
                                                  , EventGarbageChangeDetails
                                                  , EventRebuildFinished
                                                  , EventRebuildDoUpdateChanged
                                                  , EventGarbageWithPassword
                                                  , EventGarbageStarted
                                                  , EventRebuildDoRollbackChanged
                                                  , EventRebuildModeIdxChanged
                                                  , EventAskPassWatch
                                                  )
                                                )
import           NixManager.ManagerState        ( ManagerState(..)
                                                , msAdminState
                                                )
import           NixManager.Util                ( threadDelayMillis )
import           NixManager.ChangeType          ( ChangeType(NoChanges) )
import           NixManager.AskPass             ( askPass
                                                , sudoExpr
                                                )
import           NixManager.NixRebuild          ( rebuild
                                                , rollbackRebuild
                                                )
import           GI.Gtk.Declarative.App.Simple  ( Transition(Transition) )
import           Prelude                 hiding ( length
                                                , putStrLn
                                                )
import           NixManager.NixRebuildUpdateMode
                                                ( NixRebuildUpdateMode
                                                  ( NixRebuildUpdateUpdate
                                                  , NixRebuildUpdateRollback
                                                  , NixRebuildUpdateNone
                                                  )
                                                )
import           NixManager.Admin.RebuildData   ( rdBuildState
                                                , rdProcessOutput
                                                , rdActiveRebuildModeIdx
                                                , rdDoUpdate
                                                , rdDoRollback
                                                , rdDetailsState
                                                )
import           NixManager.Admin.BuildState    ( bsProcessData
                                                , bsCounter
                                                , bsProcessData
                                                , bsPassword
                                                , BuildState(BuildState)
                                                )
import           NixManager.Admin.ValidRebuildModes
                                                ( validRebuildModeIdx )

-- | Given two mutually exclusive "do rollback" and "do update" booleans, calculate the update mode.
calculateRebuildUpdateMode :: Bool -> Bool -> NixRebuildUpdateMode
calculateRebuildUpdateMode _update@True _ = NixRebuildUpdateUpdate
calculateRebuildUpdateMode _ _rollback@True = NixRebuildUpdateRollback
calculateRebuildUpdateMode _ _ = NixRebuildUpdateNone

-- | Format a process exit code somewhat nicer than the default 'Show' instance
formatExitCode :: ExitCode -> BS.ByteString
formatExitCode (ExitFailure code) = "error code " <> BS.pack (show code)
formatExitCode ExitSuccess        = "success"

-- | Kill a running process using good olde @kill -9@. You might be wondering why this is done. The thing is, simply terminating the process (via "System.Process") didn’t really /do/ anything to the process. Weird as it seems. This one works — hopefully.
sudoKillProcess :: BuildState -> IO (Maybe ManagerEvent)
sudoKillProcess bs = do
  pid' <- getProcessId (bs ^. bsProcessData)
  case pid' of
    Nothing  -> pure Nothing
    Just pid -> do
      _po <- runProcessToFinish
        (Just (encodeUtf8 (getPassword (bs ^. bsPassword))))
        (sudoExpr (kill pid))
      pure Nothing

-- | For a broad description of what the events do, see 'Event'
updateEvent
  :: ManagerState -> State -> Event -> Transition ManagerState ManagerEvent
updateEvent ms _ EventRebuild = Transition
  ms
  (adminEvent . EventAskPassWatch EventRebuildWithPassword mempty <$> askPass)
updateEvent ms _ EventGarbage = Transition
  ms
  (adminEvent . EventAskPassWatch EventGarbageWithPassword mempty <$> askPass)
updateEvent ms _ EventRebuildCancel =
  Transition (ms & msAdminState . rebuildData . rdBuildState .~ Nothing) $ maybe
    (pure Nothing)
    sudoKillProcess
    (ms ^. msAdminState . rebuildData . rdBuildState)
updateEvent ms _ EventGarbageCancel =
  Transition (ms & msAdminState . garbageData . gdBuildState .~ Nothing) $ maybe
    (pure Nothing)
    sudoKillProcess
    (ms ^. msAdminState . garbageData . gdBuildState)
updateEvent ms _ (EventGarbageWithPassword password) = Transition ms $ do
  garbagePo <- collectGarbage
    (ms ^. msAdminState . garbageData . gdOlderGenerations)
    password
  pure (adminEvent (EventGarbageStarted garbagePo password))
updateEvent ms _ (EventRebuildWithPassword password) = Transition ms $ do
  rebuildPo <- rebuild
    (  ms
    ^. msAdminState
    .  rebuildData
    .  rdActiveRebuildModeIdx
    .  from validRebuildModeIdx
    )
    (calculateRebuildUpdateMode
      (ms ^. msAdminState . rebuildData . rdDoUpdate)
      (ms ^. msAdminState . rebuildData . rdDoRollback)
    )
    password
  pure (adminEvent (EventRebuildStarted rebuildPo password))
updateEvent ms _ (EventAskPassWatch andThen po pd) = Transition ms $ do
  -- See the readme about an explanation of why we do this “watch” event stuff
  newpo <- updateProcess pd
  let totalPo = po <> newpo
  case totalPo ^. poResult . to getFirst of
    Nothing -> do
      threadDelayMillis 500
      pure (adminEvent (EventAskPassWatch andThen totalPo pd))
    Just ExitSuccess ->
      pure
        (adminEvent (andThen (Password (totalPo ^. poStdout . to decodeUtf8))))
    Just (ExitFailure _) -> pure Nothing
updateEvent ms _ (EventRebuildStarted pd password) =
  Transition
      (  ms
      &  msAdminState
      .  rebuildData
      .  rdBuildState
      ?~ BuildState 0 pd password
      &  msAdminState
      .  rebuildData
      .  rdProcessOutput
      .~ mempty
      )
    $ pure (adminEvent (EventRebuildWatch password mempty pd))
updateEvent ms _ (EventGarbageStarted pd password) =
  Transition
      (  ms
      &  msAdminState
      .  garbageData
      .  gdBuildState
      ?~ BuildState 0 pd password
      &  msAdminState
      .  garbageData
      .  gdProcessOutput
      .~ mempty
      )
    $ pure (adminEvent (EventGarbageWatch mempty pd))
updateEvent ms _ (EventGarbageWatch priorOutput pd) =
  Transition
      (  ms
      &  msAdminState
      .  garbageData
      .  gdProcessOutput
      .~ priorOutput
      &  msAdminState
      .  garbageData
      .  gdBuildState
      .  traversed
      .  bsCounter
      +~ 1
      )
    $ do
        -- See the readme about an explanation of why we do this “watch” event stuff
        updates <- updateProcess pd
        let newOutput = priorOutput <> updates
        case updates ^. poResult . to getFirst of
          Nothing -> do
            threadDelayMillis 500
            pure (adminEvent (EventGarbageWatch newOutput pd))
          Just code -> pure (adminEvent (EventGarbageFinished newOutput code))
updateEvent ms _ (EventRebuildWatch password priorOutput pd) =
  Transition
      (  ms
      &  msAdminState
      .  rebuildData
      .  rdProcessOutput
      .~ priorOutput
      &  msAdminState
      .  rebuildData
      .  rdBuildState
      .  traversed
      .  bsCounter
      +~ 1
      )
    $ do
        -- See the readme about an explanation of why we do this “watch” event stuff
        updates <- updateProcess pd
        let newOutput = priorOutput <> updates
        case updates ^. poResult . to getFirst of
          Nothing -> do
            threadDelayMillis 500
            pure (adminEvent (EventRebuildWatch password newOutput pd))
          Just code@ExitSuccess ->
            pure (adminEvent (EventRebuildFinished newOutput code))
          Just code@(ExitFailure _) -> do
            rollbackRebuild password
            pure (adminEvent (EventRebuildFinished newOutput code))
updateEvent ms _ (EventGarbageFinished totalOutput exitCode) = pureTransition
  (  ms
  &  msAdminState
  .  garbageData
  .  gdBuildState
  .~ Nothing
  &  msAdminState
  .  garbageData
  .  gdProcessOutput
  .~ (   totalOutput
     &   poStdout
     <>~ ("\nFinished with " <> formatExitCode exitCode)
     )
  )
updateEvent ms _ (EventRebuildFinished totalOutput exitCode) =
  Transition
      (  ms
      &  msAdminState
      .  rebuildData
      .  rdBuildState
      .~ Nothing
      &  msAdminState
      .  rebuildData
      .  rdProcessOutput
      .~ (   totalOutput
         &   poStdout
         <>~ ("\nFinished with " <> formatExitCode exitCode)
         )
      &  msAdminState
      .  changes
      .~ NoChanges
      )
    $ pure (packagesEvent PackagesEvent.EventReload)
updateEvent ms _ (EventRebuildModeIdxChanged newIdx) =
  pureTransition
    $  ms
    &  msAdminState
    .  rebuildData
    .  rdActiveRebuildModeIdx
    .~ newIdx
updateEvent ms _ (EventRebuildChangeDetails newDetails) = pureTransition
  (ms & msAdminState . rebuildData . rdDetailsState .~ newDetails)
updateEvent ms _ (EventGarbageChangeDetails newDetails) = pureTransition
  (ms & msAdminState . garbageData . gdDetailsState .~ newDetails)
updateEvent ms _ EventReload =
  Transition ms $ adminEvent . EventReloadFinished <$> determineChanges
updateEvent ms _ (EventReloadFinished newChanges) =
  pureTransition (ms & msAdminState . changes .~ newChanges)
updateEvent ms _ (EventRebuildDoUpdateChanged newUpdate) = pureTransition
  (  ms
  &  msAdminState
  .  rebuildData
  .  rdDoUpdate
  .~ newUpdate
  &  msAdminState
  .  rebuildData
  .  rdDoRollback
  .~ False
  )
updateEvent ms _ (EventRebuildDoRollbackChanged newRollback) = pureTransition
  (  ms
  &  msAdminState
  .  rebuildData
  .  rdDoUpdate
  .~ False
  &  msAdminState
  .  rebuildData
  .  rdDoRollback
  .~ newRollback
  )
updateEvent ms _ (EventGarbageOlderGenerationsChanged newOldGen) =
  pureTransition
    (ms & msAdminState . garbageData . gdOlderGenerations .~ newOldGen)
