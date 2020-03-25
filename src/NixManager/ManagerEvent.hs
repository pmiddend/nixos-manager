{-# LANGUAGE TemplateHaskell #-}
module NixManager.ManagerEvent
  ( ManagerEvent(..)
  )
where

import           Control.Lens                   ( makePrisms )
import           NixManager.ServicesEvent       ( ServicesEvent )
import           NixManager.PackagesEvent       ( PackagesEvent )
import           NixManager.AdminEvent          ( AdminEvent )

data ManagerEvent = ManagerEventClosed
      | ManagerEventDiscard
      | ManagerEventAdmin AdminEvent
      | ManagerEventServices ServicesEvent
      | ManagerEventPackages PackagesEvent

makePrisms ''ManagerEvent
