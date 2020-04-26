{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-|
  Description: Trampoline module for all the update functions (for the separate tabs)
  -}
module NixManager.Update
  ( update
  )
where

import qualified NixManager.Admin.Update       as AdminUpdate
import qualified NixManager.Services.Update    as ServicesUpdate
import qualified NixManager.Packages.Update    as PackagesUpdate
import qualified NixManager.HMPackages.Update  as HMPackagesUpdate
import qualified NixManager.HMServices.Update  as HMServicesUpdate
import qualified NixManager.HMAdmin.Update     as HMAdminUpdate
import           Control.Lens                   ( (^.) )
import           NixManager.ManagerState        ( ManagerState(..) )
import           NixManager.ManagerEvent        ( ManagerEvent(..)
                                                , pureTransition
                                                )
import           GI.Gtk.Declarative.App.Simple  ( Transition(Exit) )
import           Prelude                 hiding ( length
                                                , putStrLn
                                                )


-- | Process an event, change the state, and potentially emit an event and some side-effects
update :: ManagerState -> ManagerEvent -> Transition ManagerState ManagerEvent
update s (ManagerEventAdmin ae) =
  AdminUpdate.updateEvent s (s ^. #adminState) ae
update s (ManagerEventServices   se) = ServicesUpdate.updateEvent s se
update s (ManagerEventHMServices se) = HMServicesUpdate.updateEvent s se
update s (ManagerEventPackages   se) = PackagesUpdate.updateEvent s se
update s (ManagerEventHMPackages se) = HMPackagesUpdate.updateEvent s se
update s (ManagerEventHMAdmin    se) = HMAdminUpdate.updateEvent s se
update _ ManagerEventClosed          = Exit
update s ManagerEventDiscard         = pureTransition s

