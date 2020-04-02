{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module NixManager.Admin.Update
  ( updateEvent
  )
where

import           NixManager.NixGarbage          ( collectGarbage )
import           NixManager.ManagerEvent        ( adminEvent
                                                , ManagerEvent
                                                , pureTransition
                                                , packagesEvent
                                                )
import qualified NixManager.Packages.Event     as PackagesEvent
import           Data.Text.Encoding             ( decodeUtf8 )
import           System.Exit                    ( ExitCode
                                                  ( ExitSuccess
                                                  , ExitFailure
                                                  )
                                                )
import           NixManager.Process             ( updateProcess
                                                , poResult
                                                , poStdout
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
                                                , folded
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
                                                  , EventRebuildModeChanged
                                                  , EventAskPassWatch
                                                  )
                                                )
import           NixManager.ManagerState        ( ManagerState(..)
                                                , msAdminState
                                                )
import           NixManager.Util                ( threadDelayMillis )
import           NixManager.Changes             ( determineChanges
                                                , ChangeType(NoChanges)
                                                )
import           NixManager.NixRebuildMode      ( parseRebuildMode )
import           NixManager.AskPass             ( askPass )
import           NixManager.NixRebuild          ( rebuild )
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
                                                , rdActiveRebuildMode
                                                , rdDoUpdate
                                                , rdDoRollback
                                                , rdDetailsState
                                                )
import           NixManager.Admin.BuildState    ( bsProcessData
                                                , bsCounter
                                                , BuildState(BuildState)
                                                )

calculateRebuildUpdateMode :: Bool -> Bool -> NixRebuildUpdateMode
calculateRebuildUpdateMode _update@True _ = NixRebuildUpdateUpdate
calculateRebuildUpdateMode _ _rollback@True = NixRebuildUpdateRollback
calculateRebuildUpdateMode _ _ = NixRebuildUpdateNone

updateEvent
  :: ManagerState -> State -> Event -> Transition ManagerState ManagerEvent
updateEvent ms _ EventRebuild = Transition
  ms
  (adminEvent . EventAskPassWatch EventRebuildWithPassword mempty <$> askPass)
updateEvent ms _ EventGarbage = Transition
  ms
  (adminEvent . EventAskPassWatch EventGarbageWithPassword mempty <$> askPass)
updateEvent ms _ EventRebuildCancel =
  Transition (ms & msAdminState . asRebuildData . rdBuildState .~ Nothing) $ do
    terminate
      (   ms
      ^?! msAdminState
      .   asRebuildData
      .   rdBuildState
      .   folded
      .   bsProcessData
      )
    pure Nothing
updateEvent ms _ EventGarbageCancel =
  Transition (ms & msAdminState . asGarbageData . gdBuildState .~ Nothing) $ do
    terminate
      (   ms
      ^?! msAdminState
      .   asGarbageData
      .   gdBuildState
      .   folded
      .   bsProcessData
      )
    pure Nothing
updateEvent ms _ (EventGarbageWithPassword password) = Transition ms $ do
  garbagePo <- collectGarbage
    (ms ^. msAdminState . asGarbageData . gdOlderGenerations)
    password
  pure (adminEvent (EventGarbageStarted garbagePo))
updateEvent ms _ (EventRebuildWithPassword password) = Transition ms $ do
  rebuildPo <- rebuild
    (ms ^. msAdminState . asRebuildData . rdActiveRebuildMode)
    (calculateRebuildUpdateMode
      (ms ^. msAdminState . asRebuildData . rdDoUpdate)
      (ms ^. msAdminState . asRebuildData . rdDoRollback)
    )
    password
  pure (adminEvent (EventRebuildStarted rebuildPo))
updateEvent ms _ (EventAskPassWatch andThen po pd) = Transition ms $ do
  newpo <- updateProcess pd
  let totalPo = po <> newpo
  case totalPo ^. poResult . to getFirst of
    Nothing -> do
      threadDelayMillis 500
      pure (adminEvent (EventAskPassWatch andThen totalPo pd))
    Just ExitSuccess ->
      pure (adminEvent (andThen (totalPo ^. poStdout . to decodeUtf8)))
    Just (ExitFailure _) -> pure Nothing
updateEvent ms _ (EventRebuildStarted pd) =
  Transition
      (  ms
      &  msAdminState
      .  asRebuildData
      .  rdBuildState
      ?~ BuildState 0 pd
      &  msAdminState
      .  asRebuildData
      .  rdProcessOutput
      .~ mempty
      )
    $ pure (adminEvent (EventRebuildWatch mempty pd))
updateEvent ms _ (EventGarbageStarted pd) =
  Transition
      (  ms
      &  msAdminState
      .  asGarbageData
      .  gdBuildState
      ?~ BuildState 0 pd
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
updateEvent ms _ (EventRebuildWatch priorOutput pd) =
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
            pure (adminEvent (EventRebuildWatch newOutput pd))
          Just code -> pure (adminEvent (EventRebuildFinished newOutput code))
updateEvent ms _ (EventGarbageFinished totalOutput _exitCode) = pureTransition
  (  ms
  &  msAdminState
  .  asGarbageData
  .  gdBuildState
  .~ Nothing
  &  msAdminState
  .  asGarbageData
  .  gdProcessOutput
  .~ (totalOutput & poStdout <>~ "\nFinished!")
  )
updateEvent ms _ (EventRebuildFinished totalOutput _exitCode) =
  Transition
      (  ms
      &  msAdminState
      .  asRebuildData
      .  rdBuildState
      .~ Nothing
      &  msAdminState
      .  asRebuildData
      .  rdProcessOutput
      .~ (totalOutput & poStdout <>~ "\nFinished!")
      &  msAdminState
      .  asChanges
      .~ NoChanges
      )
    $ pure (packagesEvent PackagesEvent.EventReload)
updateEvent ms _ (EventRebuildModeChanged newType) =
  pureTransition $ case parseRebuildMode newType of
    Nothing -> ms
    Just newType' ->
      ms & msAdminState . asRebuildData . rdActiveRebuildMode .~ newType'
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
