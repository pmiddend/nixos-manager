{-# LANGUAGE TemplateHaskell #-}
module NixManager.ManagerEvent
  ( ManagerEvent(..)
  )
where

import           Control.Lens                   ( makePrisms )
import           NixManager.Services.Event     as Services
import qualified NixManager.Packages.Event     as Packages
import qualified NixManager.Admin.Event        as Admin

data ManagerEvent = ManagerEventClosed
      | ManagerEventDiscard
      | ManagerEventAdmin Admin.Event
      | ManagerEventServices Services.Event
      | ManagerEventPackages Packages.Event

makePrisms ''ManagerEvent
