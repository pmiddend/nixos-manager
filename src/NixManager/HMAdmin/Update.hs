{-|
  Description: Contains the update logic for the home-manager Administration tab
Contains the update logic for the home-manager Administration tab
  -}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module NixManager.HMAdmin.Update
  ( updateEvent
  )
where

import           Data.Foldable                  ( for_ )
import qualified Data.ByteString.Char8         as BS
import qualified NixManager.HMPackages.Event   as HMPackagesEvent
import           NixManager.ManagerEvent        ( hmAdminEvent
                                                , ManagerEvent
                                                  ( ManagerEventHMAdmin
                                                  )
                                                , liftUpdate
                                                , pureTransition
                                                , hmPackagesEvent
                                                )
import           System.Exit                    ( ExitCode
                                                  ( ExitSuccess
                                                  , ExitFailure
                                                  )
                                                )
import           NixManager.Process             ( updateProcess
                                                , terminate
                                                , poResult
                                                , poStdout
                                                )
import           NixManager.ChangeType          ( ChangeType(Changes, NoChanges)
                                                )
import qualified NixManager.HMAdmin.GenerationsView
                                               as GenerationsView
import           NixManager.HMAdmin.State       ( rebuildData
                                                , determineChanges
                                                , generationsState
                                                , garbageData
                                                , changes
                                                )
import           NixManager.HMAdmin.GarbageData ( gdBuildState
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
                                                , (^?)
                                                , folded
                                                , (.~)
                                                , (+~)
                                                )
import           NixManager.HMAdmin.Event       ( Event
                                                  ( EventRebuild
                                                  , EventRebuildStarted
                                                  , EventGarbage
                                                  , EventGarbageCancel
                                                  , EventGarbageWatch
                                                  , EventGenerations
                                                  , EventReload
                                                  , EventReloadFinished
                                                  , EventGarbageFinished
                                                  , EventRebuildWatch
                                                  , EventRebuildCancel
                                                  , EventRebuildChangeDetails
                                                  , EventGarbageChangeDetails
                                                  , EventRebuildFinished
                                                  , EventGarbageStarted
                                                  , EventRebuildModeIdxChanged
                                                  )
                                                )
import           NixManager.ManagerState        ( ManagerState(..)
                                                , msHMAdminState
                                                )
import           NixManager.Util                ( threadDelayMillis )
import           NixManager.HMRebuild           ( rebuild )
import           GI.Gtk.Declarative.App.Simple  ( Transition(Transition) )
import           Prelude                 hiding ( length
                                                , putStrLn
                                                )
import           NixManager.HMAdmin.RebuildData ( rdBuildState
                                                , rdProcessOutput
                                                , rdActiveRebuildModeIdx
                                                , rdDetailsState
                                                )
import           NixManager.HMAdmin.BuildState  ( bsProcessData
                                                , bsCounter
                                                , bsProcessData
                                                , BuildState(BuildState)
                                                )
import           NixManager.HMRebuildMode       ( rebuildModeIdx )
import           NixManager.HMGarbage           ( collectGarbage )

-- | Format a process exit code somewhat nicer than the default 'Show' instance
formatExitCode :: ExitCode -> BS.ByteString
formatExitCode (ExitFailure code) = "error code " <> BS.pack (show code)
formatExitCode ExitSuccess        = "success"

-- | For a broad description of what the events do, see 'Event'
updateEvent :: ManagerState -> Event -> Transition ManagerState ManagerEvent
updateEvent ms (EventGenerations e) = liftUpdate
  GenerationsView.updateEvent
  (msHMAdminState . generationsState)
  (ManagerEventHMAdmin . EventGenerations)
  ms
  e
updateEvent ms EventReload =
  Transition ms $ hmAdminEvent . EventReloadFinished <$> determineChanges
updateEvent ms (EventReloadFinished newChanges) = Transition
  (ms & msHMAdminState . changes .~ newChanges)
  (pure (hmAdminEvent (EventGenerations GenerationsView.EventReload)))
updateEvent ms EventRebuild = Transition
  ms
  do
    processData <- rebuild
      (  ms
      ^. msHMAdminState
      .  rebuildData
      .  rdActiveRebuildModeIdx
      .  from rebuildModeIdx
      )
    pure (hmAdminEvent (EventRebuildStarted processData))
updateEvent ms EventGarbage = Transition
  ms
  do
    processData <- collectGarbage
    pure (hmAdminEvent (EventGarbageStarted processData))
updateEvent ms EventRebuildCancel = Transition
  (ms & msHMAdminState . rebuildData . rdBuildState .~ Nothing)
  do
    for_
      (  ms
      ^? msHMAdminState
      .  rebuildData
      .  rdBuildState
      .  folded
      .  bsProcessData
      )
      terminate
    pure Nothing
updateEvent ms EventGarbageCancel = Transition
  (ms & msHMAdminState . garbageData . gdBuildState .~ Nothing)
  do
    for_
      (  ms
      ^? msHMAdminState
      .  garbageData
      .  gdBuildState
      .  folded
      .  bsProcessData
      )
      terminate
    pure Nothing
updateEvent ms (EventRebuildStarted pd) =
  Transition
      (  ms
      &  msHMAdminState
      .  rebuildData
      .  rdBuildState
      ?~ BuildState 0 pd
      &  msHMAdminState
      .  rebuildData
      .  rdProcessOutput
      .~ mempty
      )
    $ pure (hmAdminEvent (EventRebuildWatch mempty pd))
updateEvent ms (EventGarbageStarted pd) =
  Transition
      (  ms
      &  msHMAdminState
      .  garbageData
      .  gdBuildState
      ?~ BuildState 0 pd
      &  msHMAdminState
      .  garbageData
      .  gdProcessOutput
      .~ mempty
      )
    $ pure (hmAdminEvent (EventGarbageWatch mempty pd))
updateEvent ms (EventGarbageWatch priorOutput pd) =
  Transition
      (  ms
      &  msHMAdminState
      .  garbageData
      .  gdProcessOutput
      .~ priorOutput
      &  msHMAdminState
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
            pure (hmAdminEvent (EventGarbageWatch newOutput pd))
          Just code ->
            pure (hmAdminEvent (EventGarbageFinished newOutput code))
updateEvent ms (EventRebuildWatch priorOutput pd) =
  Transition
      (  ms
      &  msHMAdminState
      .  rebuildData
      .  rdProcessOutput
      .~ priorOutput
      &  msHMAdminState
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
            pure (hmAdminEvent (EventRebuildWatch newOutput pd))
          Just code ->
            pure (hmAdminEvent (EventRebuildFinished newOutput code))
updateEvent ms (EventGarbageFinished totalOutput exitCode) = pureTransition
  (  ms
  &  msHMAdminState
  .  garbageData
  .  gdBuildState
  .~ Nothing
  &  msHMAdminState
  .  garbageData
  .  gdProcessOutput
  .~ (   totalOutput
     &   poStdout
     <>~ ("\nFinished with " <> formatExitCode exitCode)
     )
  )
updateEvent ms (EventRebuildFinished totalOutput exitCode) =
  Transition
      (  ms
      &  msHMAdminState
      .  rebuildData
      .  rdBuildState
      .~ Nothing
      &  msHMAdminState
      .  changes
      .~ (if exitCode == ExitSuccess then NoChanges else Changes)
      &  msHMAdminState
      .  rebuildData
      .  rdProcessOutput
      .~ (   totalOutput
         &   poStdout
         <>~ ("\nFinished with " <> formatExitCode exitCode)
         )
      )
    $ pure (hmPackagesEvent HMPackagesEvent.EventReload)
updateEvent ms (EventRebuildModeIdxChanged newIdx) =
  pureTransition
    $  ms
    &  msHMAdminState
    .  rebuildData
    .  rdActiveRebuildModeIdx
    .~ newIdx
updateEvent ms (EventRebuildChangeDetails newDetails) = pureTransition
  (ms & msHMAdminState . rebuildData . rdDetailsState .~ newDetails)
updateEvent ms (EventGarbageChangeDetails newDetails) = pureTransition
  (ms & msHMAdminState . garbageData . gdDetailsState .~ newDetails)
