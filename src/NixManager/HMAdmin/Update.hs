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
import           NixManager.HMAdmin.GarbageData ( GarbageData )
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
import           NixManager.ManagerState        ( ManagerState(..) )
import           NixManager.Util                ( threadDelayMillis )
import           NixManager.HMRebuild           ( rebuild )
import           GI.Gtk.Declarative.App.Simple  ( Transition(Transition) )
import           Prelude                 hiding ( length
                                                , putStrLn
                                                )
import           NixManager.HMAdmin.BuildState  ( BuildState(BuildState) )
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
  (#hmAdminState . #generationsState)
  (ManagerEventHMAdmin . EventGenerations)
  ms
  e
updateEvent ms EventReload =
  Transition ms $ hmAdminEvent . EventReloadFinished <$> determineChanges
updateEvent ms (EventReloadFinished newChanges) = Transition
  (ms & #hmAdminState . #changes .~ newChanges)
  (pure (hmAdminEvent (EventGenerations GenerationsView.EventReload)))
updateEvent ms EventRebuild = Transition
  ms
  do
    processData <- rebuild
      (  ms
      ^. #hmAdminState
      .  #rebuildData
      .  #activeRebuildModeIdx
      .  from rebuildModeIdx
      )
    pure (hmAdminEvent (EventRebuildStarted processData))
updateEvent ms EventGarbage = Transition
  ms
  do
    processData <- collectGarbage
    pure (hmAdminEvent (EventGarbageStarted processData))
updateEvent ms EventRebuildCancel = Transition
  (ms & #hmAdminState . #rebuildData . #buildState .~ Nothing)
  do
    for_
      (ms ^? #hmAdminState . #rebuildData . #buildState . folded . #processData)
      terminate
    pure Nothing
updateEvent ms EventGarbageCancel = Transition
  (ms & #hmAdminState . #garbageData . #buildState .~ Nothing)
  do
    for_
      (ms ^? #hmAdminState . #garbageData . #buildState . folded . #processData)
      terminate
    pure Nothing
updateEvent ms (EventRebuildStarted pd) =
  Transition
      (  ms
      &  #hmAdminState
      .  #rebuildData
      .  #buildState
      ?~ BuildState 0 pd
      &  #hmAdminState
      .  #rebuildData
      .  #processOutput
      .~ mempty
      )
    $ pure (hmAdminEvent (EventRebuildWatch mempty pd))
updateEvent ms (EventGarbageStarted pd) =
  Transition
      (  ms
      &  #hmAdminState
      .  #garbageData
      .  #buildState
      ?~ BuildState 0 pd
      &  #hmAdminState
      .  #garbageData
      .  #processOutput
      .~ mempty
      )
    $ pure (hmAdminEvent (EventGarbageWatch mempty pd))
updateEvent ms (EventGarbageWatch priorOutput pd) =
  Transition
      (  ms
      &  #hmAdminState
      .  #garbageData
      .  #processOutput
      .~ priorOutput
      &  #hmAdminState
      .  #garbageData
      .  #buildState
      .  traversed
      .  #counter
      +~ 1
      )
    $ do
        -- See the readme about an explanation of why we do this “watch” event stuff
        updates <- updateProcess pd
        let newOutput = priorOutput <> updates
        case updates ^. #result . to getFirst of
          Nothing -> do
            threadDelayMillis 500
            pure (hmAdminEvent (EventGarbageWatch newOutput pd))
          Just code ->
            pure (hmAdminEvent (EventGarbageFinished newOutput code))
updateEvent ms (EventRebuildWatch priorOutput pd) =
  Transition
      (  ms
      &  #hmAdminState
      .  #rebuildData
      .  #processOutput
      .~ priorOutput
      &  #hmAdminState
      .  #rebuildData
      .  #buildState
      .  traversed
      .  #counter
      +~ 1
      )
    $ do
        -- See the readme about an explanation of why we do this “watch” event stuff
        updates <- updateProcess pd
        let newOutput = priorOutput <> updates
        case updates ^. #result . to getFirst of
          Nothing -> do
            threadDelayMillis 500
            pure (hmAdminEvent (EventRebuildWatch newOutput pd))
          Just code ->
            pure (hmAdminEvent (EventRebuildFinished newOutput code))
updateEvent ms (EventGarbageFinished totalOutput exitCode) = pureTransition
  (  ms
  &  #hmAdminState
  .  #garbageData
  .  #buildState
  .~ Nothing
  &  #hmAdminState
  .  #garbageData
  .  #processOutput
  .~ (   totalOutput
     &   #stdout
     <>~ ("\nFinished with " <> formatExitCode exitCode)
     )
  )
updateEvent ms (EventRebuildFinished totalOutput exitCode) =
  Transition
      (  ms
      &  #hmAdminState
      .  #rebuildData
      .  #buildState
      .~ Nothing
      &  #hmAdminState
      .  #changes
      .~ (if exitCode == ExitSuccess then NoChanges else Changes)
      &  #hmAdminState
      .  #rebuildData
      .  #processOutput
      .~ (   totalOutput
         &   #stdout
         <>~ ("\nFinished with " <> formatExitCode exitCode)
         )
      )
    $ pure (hmPackagesEvent HMPackagesEvent.EventReload)
updateEvent ms (EventRebuildModeIdxChanged newIdx) =
  pureTransition
    $  ms
    &  #hmAdminState
    .  #rebuildData
    .  #activeRebuildModeIdx
    .~ newIdx
updateEvent ms (EventRebuildChangeDetails newDetails) = pureTransition
  (ms & #hmAdminState . #rebuildData . #detailsState .~ newDetails)
updateEvent ms (EventGarbageChangeDetails newDetails) = pureTransition
  (ms & #hmAdminState . #garbageData . #detailsState .~ newDetails)
