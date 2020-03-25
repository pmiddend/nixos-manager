{-# LANGUAGE TemplateHaskell #-}
module NixManager.AdminEvent
  ( AdminEvent(..)
  )
where

import           Control.Lens                   ( makePrisms )
import           NixManager.Process             ( ProcessData
                                                , ProcessOutput
                                                )
import           Data.Text                      ( Text )

data AdminEvent = AdminEventRebuild
                | AdminEventAskPassWatch ProcessOutput ProcessData
                | AdminEventRebuildStarted ProcessData
                | AdminEventRebuildWatch ProcessOutput ProcessData
                | AdminEventRebuildFinished ProcessOutput
                | AdminEventBuildTypeChanged Text
                | AdminEventRebuildCancel

makePrisms ''AdminEvent
