{-# LANGUAGE TemplateHaskell #-}
module NixManager.ManagerEvent
  ( ManagerEvent(..)
  , adminEvent
  , servicesEvent
  , packagesEvent
  , pureTransition
  )
where

import           Control.Lens                   ( makePrisms )
import           NixManager.Services.Event     as Services
import qualified NixManager.Packages.Event     as Packages
import qualified NixManager.Admin.Event        as Admin
import           GI.Gtk.Declarative.App.Simple  ( Transition(Transition) )

data ManagerEvent = ManagerEventClosed
                  | ManagerEventDiscard
                  | ManagerEventAdmin Admin.Event
                  | ManagerEventServices Services.Event
                  | ManagerEventPackages Packages.Event

makePrisms ''ManagerEvent

adminEvent :: Admin.Event -> Maybe ManagerEvent
adminEvent = Just . ManagerEventAdmin

servicesEvent :: Services.Event -> Maybe ManagerEvent
servicesEvent = Just . ManagerEventServices

packagesEvent :: Packages.Event -> Maybe ManagerEvent
packagesEvent = Just . ManagerEventPackages

pureTransition :: state -> Transition state ManagerEvent
pureTransition x = Transition x (pure Nothing)
