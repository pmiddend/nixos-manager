{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module NixManager.Admin.Update
  ( updateEvent
  )
where

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
                                                , asActiveBuildType
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
                                                  , EventRebuildWatch
                                                  , EventRebuildCancel
                                                  , EventChangeDetails
                                                  , EventRebuildFinished
                                                  , EventBuildTypeChanged
                                                  , EventAskPassWatch
                                                  )
                                                )
import           NixManager.ManagerState        ( ManagerState(..)
                                                , msAdminState
                                                )
import           NixManager.Util                ( threadDelayMillis )
import           NixManager.ManagerEvent        ( ManagerEvent(..) )
import           NixManager.Rebuild             ( askPass
                                                , rebuild
                                                )
import           GI.Gtk.Declarative.App.Simple  ( Transition(Transition) )
import           Prelude                 hiding ( length
                                                , putStrLn
                                                )


adminEvent :: Event -> Maybe ManagerEvent
adminEvent = Just . ManagerEventAdmin

pureTransition :: ManagerState -> Transition ManagerState ManagerEvent
pureTransition x = Transition x (pure Nothing)

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
      rebuildPo <- rebuild (totalPo ^. poStdout . to decodeUtf8)
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
          Just _ -> pure (adminEvent (EventRebuildFinished newOutput))
updateEvent ms _ (EventRebuildFinished totalOutput) = pureTransition
  (  ms
  &  msAdminState
  .  asBuildState
  .~ Nothing
  &  msAdminState
  .  asProcessOutput
  .~ (totalOutput & poStdout <>~ "\n\nFinished!")
  )
updateEvent ms _ (EventBuildTypeChanged newType) =
  pureTransition (ms & msAdminState . asActiveBuildType .~ newType)
updateEvent ms _ (EventChangeDetails newDetails) =
  pureTransition (ms & msAdminState . asDetailsState .~ newDetails)

