{-# LANGUAGE TemplateHaskell #-}
module NixManager.Admin.Event
  ( Event(..)
  )
where

import           Control.Lens                   ( makePrisms )
import           NixManager.Process             ( ProcessData
                                                , ProcessOutput
                                                )
import           Data.Text                      ( Text )

data Event = EventRebuild
                | EventAskPassWatch ProcessOutput ProcessData
                | EventRebuildStarted ProcessData
                | EventRebuildWatch ProcessOutput ProcessData
                | EventRebuildFinished ProcessOutput
                | EventBuildTypeChanged Text
                | EventRebuildCancel

makePrisms ''Event
