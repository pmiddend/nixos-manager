{-|
  Description: The "root" manager state, containing the substates for the tabs (notebook pages). To be used with gi-gtk-declarative-app-simple model.
  -}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module NixManager.ManagerState
  ( msAdminState
  , msServiceState
  , msPackagesState
  , ManagerState(..)
  )
where

import           Control.Lens                   ( makeLenses )
import qualified NixManager.Services.State     as Services
import qualified NixManager.Admin.State        as Admin
import qualified NixManager.Packages.State     as Packages

-- | The root manager state
data ManagerState = ManagerState {
     _msPackagesState :: Packages.State
   , _msServiceState :: Services.State
   , _msAdminState :: Admin.State
   }

makeLenses ''ManagerState

