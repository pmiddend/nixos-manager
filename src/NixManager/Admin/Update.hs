{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module NixManager.Admin.Update
  ( updateEvent
  )
where

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
                                                , asBuildState
                                                , asProcessOutput
                                                , asChanges
                                                , asActiveRebuildMode
                                                , absCounter
                                                , asDetailsState
                                                , absProcessData
                                                , BuildState(BuildState)
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
                                                  , EventReloadFinished
                                                  , EventRebuildWatch
                                                  , EventRebuildCancel
                                                  , EventChangeDetails
                                                  , EventRebuildFinished
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
import           NixManager.NixRebuild          ( askPass
                                                , rebuild
                                                )
import           GI.Gtk.Declarative.App.Simple  ( Transition(Transition) )
import           Prelude                 hiding ( length
                                                , putStrLn
                                                )


updateEvent
  :: ManagerState -> State -> Event -> Transition ManagerState ManagerEvent
updateEvent ms _ EventRebuild =
  Transition ms (adminEvent . EventAskPassWatch mempty <$> askPass)
updateEvent ms _ EventRebuildCancel =
  Transition (ms & msAdminState . asBuildState .~ Nothing) $ do
    terminate (ms ^?! msAdminState . asBuildState . folded . absProcessData)
    pure Nothing
updateEvent ms _ (EventAskPassWatch po pd) = Transition ms $ do
  newpo <- updateProcess pd
  let totalPo = po <> newpo
  case totalPo ^. poResult . to getFirst of
    Nothing -> do
      threadDelayMillis 500
      pure (adminEvent (EventAskPassWatch totalPo pd))
    Just ExitSuccess -> do
      rebuildPo <- rebuild (ms ^. msAdminState . asActiveRebuildMode)
                           (totalPo ^. poStdout . to decodeUtf8)
      pure (adminEvent (EventRebuildStarted rebuildPo))
    Just (ExitFailure _) -> pure Nothing
updateEvent ms _ (EventRebuildStarted pd) =
  Transition
      (  ms
      &  msAdminState
      .  asBuildState
      ?~ BuildState 0 pd
      &  msAdminState
      .  asProcessOutput
      .~ mempty
      )
    $ pure (adminEvent (EventRebuildWatch mempty pd))
updateEvent ms _ (EventRebuildWatch priorOutput pd) =
  Transition
      (  ms
      &  msAdminState
      .  asProcessOutput
      .~ priorOutput
      &  msAdminState
      .  asBuildState
      .  traversed
      .  absCounter
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
updateEvent ms _ (EventRebuildFinished totalOutput _exitCode) =
  Transition
      (  ms
      &  msAdminState
      .  asBuildState
      .~ Nothing
      &  msAdminState
      .  asProcessOutput
      .~ (totalOutput & poStdout <>~ "\nFinished!")
      &  msAdminState
      .  asChanges
      .~ NoChanges
      )
    $ pure (packagesEvent PackagesEvent.EventReload)
updateEvent ms _ (EventRebuildModeChanged newType) =
  pureTransition $ case parseRebuildMode newType of
    Nothing       -> ms
    Just newType' -> ms & msAdminState . asActiveRebuildMode .~ newType'
updateEvent ms _ (EventChangeDetails newDetails) =
  pureTransition (ms & msAdminState . asDetailsState .~ newDetails)
updateEvent ms _ EventReload =
  Transition ms $ adminEvent . EventReloadFinished <$> determineChanges
updateEvent ms _ (EventReloadFinished newChanges) =
  pureTransition (ms & msAdminState . asChanges .~ newChanges)
