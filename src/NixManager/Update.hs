{-|
  Description: Trampoline module for all the update functions (for the separate tabs)
  -}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
module NixManager.Update
  ( update
  )
where

import qualified NixManager.Admin.Update       as AdminUpdate
import qualified NixManager.Services.Update    as ServicesUpdate
import qualified NixManager.Packages.Update    as PackagesUpdate
import           Control.Lens                   ( (^.) )
import           NixManager.ManagerState        ( ManagerState(..)
                                                , msAdminState
                                                )
import           NixManager.ManagerEvent        ( ManagerEvent(..), pureTransition )
import           GI.Gtk.Declarative.App.Simple  ( Transition(Transition, Exit) )
import           Prelude                 hiding ( length
                                                , putStrLn
                                                )


-- | Process an event, change the state, and potentially emit an event and some side-effects
update :: ManagerState -> ManagerEvent -> Transition ManagerState ManagerEvent
update s (ManagerEventAdmin ae) =
  AdminUpdate.updateEvent s (s ^. msAdminState) ae
update s (ManagerEventServices se) = ServicesUpdate.updateEvent s se
update s (ManagerEventPackages se) = PackagesUpdate.updateEvent s se
update _ ManagerEventClosed        = Exit
update s ManagerEventDiscard       = pureTransition s

