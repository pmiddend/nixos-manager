{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
module NixManager.UpdateHandler
  ( update'
  )
where

import qualified NixManager.Admin.Update       as AdminUpdate
import qualified NixManager.Services.Update    as ServicesUpdate
import qualified NixManager.Packages.Update    as PackagesUpdate
import           Control.Lens                   ( (^.) )
import           NixManager.ManagerState        ( ManagerState(..)
                                                , msAdminState
                                                )
import           NixManager.ManagerEvent        ( ManagerEvent(..) )
import           GI.Gtk.Declarative.App.Simple  ( Transition(Transition, Exit) )
import           Prelude                 hiding ( length
                                                , putStrLn
                                                )


pureTransition :: ManagerState -> Transition ManagerState ManagerEvent
pureTransition x = Transition x (pure Nothing)

update' :: ManagerState -> ManagerEvent -> Transition ManagerState ManagerEvent
update' s (ManagerEventAdmin ae) =
  AdminUpdate.updateEvent s (s ^. msAdminState) ae
update' s (ManagerEventServices se) = ServicesUpdate.updateEvent s se
update' s (ManagerEventPackages se) = PackagesUpdate.updateEvent s se
update' _ ManagerEventClosed        = Exit
update' s ManagerEventDiscard       = pureTransition s

