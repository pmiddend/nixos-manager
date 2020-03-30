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
import           NixManager.Admin.State         ( DetailsState )

data Event = EventRebuild
                | EventAskPassWatch ProcessOutput ProcessData
                | EventRebuildStarted ProcessData
                | EventRebuildWatch ProcessOutput ProcessData
                | EventRebuildFinished ProcessOutput
                | EventBuildTypeChanged Text
                | EventRebuildCancel
                | EventChangeDetails DetailsState

makePrisms ''Event
