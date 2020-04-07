{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module NixManager.Admin.Update
  ( updateEvent
  )
where

import Data.Foldable(for_)
import           Control.Monad                  ( void )
import           NixManager.PosixTools          ( kill )
import qualified Data.ByteString.Char8         as BS
import           Debug.Trace                    ( traceShowId )
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
                                                , runProcess
                                                , runProcessToFinish
                                                , poResult
                                                , waitUntilFinished
                                                , poStdout
                                                , poStderr
                                                , getProcessId
                                                , terminate
                                                )
import           NixManager.Admin.State         ( State
                                                , asChanges
                                                , asRebuildData
                                                , asGarbageData
                                                )
import           NixManager.Admin.GarbageData   ( gdOlderGenerations
                                                , gdBuildState
                                                , gdProcessOutput
                                                , gdDetailsState
                                                )
import           Data.Monoid                    ( getFirst )
import           Control.Lens                   ( (^.)
                                                , from
                                                , folded
                                                , (^?)
                                                , traversed
                                                , (<>~)
                                                , (&)
                                                , (?~)
                                                , to
                                                , (.~)
                                                , (+~)
                                                , (^?!)
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
import           NixManager.Util                ( threadDelayMillis
                                                , showText
                                                )
import           NixManager.Changes             ( determineChanges
                                                , ChangeType(NoChanges)
                                                )
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
import           NixManager.NixRebuildMode      ( rebuildModeIdx )
import           NixManager.Admin.ValidRebuildModes
                                                ( validRebuildModeIdx )

calculateRebuildUpdateMode :: Bool -> Bool -> NixRebuildUpdateMode
calculateRebuildUpdateMode _update@True _ = NixRebuildUpdateUpdate
calculateRebuildUpdateMode _ _rollback@True = NixRebuildUpdateRollback
calculateRebuildUpdateMode _ _ = NixRebuildUpdateNone

formatExitCode :: ExitCode -> BS.ByteString
formatExitCode (ExitFailure code) = "error code " <> BS.pack (show code)
formatExitCode ExitSuccess        = "success"

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

updateEvent
  :: ManagerState -> State -> Event -> Transition ManagerState ManagerEvent
updateEvent ms _ EventRebuild = Transition
  ms
  (adminEvent . EventAskPassWatch EventRebuildWithPassword mempty <$> askPass)
updateEvent ms _ EventGarbage = Transition
  ms
  (adminEvent . EventAskPassWatch EventGarbageWithPassword mempty <$> askPass)
updateEvent ms _ EventRebuildCancel =
  Transition (ms & msAdminState . asRebuildData . rdBuildState .~ Nothing) $
    maybe (pure Nothing) sudoKillProcess (ms ^. msAdminState . asRebuildData . rdBuildState)
updateEvent ms _ EventGarbageCancel =
  Transition (ms & msAdminState . asGarbageData . gdBuildState .~ Nothing) $
    maybe (pure Nothing) sudoKillProcess (ms ^. msAdminState . asGarbageData . gdBuildState)
updateEvent ms _ (EventGarbageWithPassword password) = Transition ms $ do
  garbagePo <- collectGarbage
    (ms ^. msAdminState . asGarbageData . gdOlderGenerations)
    password
  pure (adminEvent (EventGarbageStarted garbagePo password))
updateEvent ms _ (EventRebuildWithPassword password) = Transition ms $ do
  rebuildPo <- rebuild
    (  ms
    ^. msAdminState
    .  asRebuildData
    .  rdActiveRebuildModeIdx
    .  from validRebuildModeIdx
    )
    (calculateRebuildUpdateMode
      (ms ^. msAdminState . asRebuildData . rdDoUpdate)
      (ms ^. msAdminState . asRebuildData . rdDoRollback)
    )
    password
  pure (adminEvent (EventRebuildStarted rebuildPo password))
updateEvent ms _ (EventAskPassWatch andThen po pd) = Transition ms $ do
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
      .  asRebuildData
      .  rdBuildState
      ?~ BuildState 0 pd password
      &  msAdminState
      .  asRebuildData
      .  rdProcessOutput
      .~ mempty
      )
    $ pure (adminEvent (EventRebuildWatch password mempty pd))
updateEvent ms _ (EventGarbageStarted pd password) =
  Transition
      (  ms
      &  msAdminState
      .  asGarbageData
      .  gdBuildState
      ?~ BuildState 0 pd password
      &  msAdminState
      .  asGarbageData
      .  gdProcessOutput
      .~ mempty
      )
    $ pure (adminEvent (EventGarbageWatch mempty pd))
updateEvent ms _ (EventGarbageWatch priorOutput pd) =
  Transition
      (  ms
      &  msAdminState
      .  asGarbageData
      .  gdProcessOutput
      .~ priorOutput
      &  msAdminState
      .  asGarbageData
      .  gdBuildState
      .  traversed
      .  bsCounter
      +~ 1
      )
    $ do
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
      .  asRebuildData
      .  rdProcessOutput
      .~ priorOutput
      &  msAdminState
      .  asRebuildData
      .  rdBuildState
      .  traversed
      .  bsCounter
      +~ 1
      )
    $ do
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
  .  asGarbageData
  .  gdBuildState
  .~ Nothing
  &  msAdminState
  .  asGarbageData
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
      .  asRebuildData
      .  rdBuildState
      .~ Nothing
      &  msAdminState
      .  asRebuildData
      .  rdProcessOutput
      .~ (   totalOutput
         &   poStdout
         <>~ ("\nFinished with " <> formatExitCode exitCode)
         )
      &  msAdminState
      .  asChanges
      .~ NoChanges
      )
    $ pure (packagesEvent PackagesEvent.EventReload)
updateEvent ms _ (EventRebuildModeIdxChanged newIdx) =
  pureTransition
    $  ms
    &  msAdminState
    .  asRebuildData
    .  rdActiveRebuildModeIdx
    .~ newIdx
updateEvent ms _ (EventRebuildChangeDetails newDetails) = pureTransition
  (ms & msAdminState . asRebuildData . rdDetailsState .~ newDetails)
updateEvent ms _ (EventGarbageChangeDetails newDetails) = pureTransition
  (ms & msAdminState . asGarbageData . gdDetailsState .~ newDetails)
updateEvent ms _ EventReload =
  Transition ms $ adminEvent . EventReloadFinished <$> determineChanges
updateEvent ms _ (EventReloadFinished newChanges) =
  pureTransition (ms & msAdminState . asChanges .~ newChanges)
updateEvent ms _ (EventRebuildDoUpdateChanged newUpdate) = pureTransition
  (  ms
  &  msAdminState
  .  asRebuildData
  .  rdDoUpdate
  .~ newUpdate
  &  msAdminState
  .  asRebuildData
  .  rdDoRollback
  .~ False
  )
updateEvent ms _ (EventRebuildDoRollbackChanged newRollback) = pureTransition
  (  ms
  &  msAdminState
  .  asRebuildData
  .  rdDoUpdate
  .~ False
  &  msAdminState
  .  asRebuildData
  .  rdDoRollback
  .~ newRollback
  )
updateEvent ms _ (EventGarbageOlderGenerationsChanged newOldGen) =
  pureTransition
    (ms & msAdminState . asGarbageData . gdOlderGenerations .~ newOldGen)
