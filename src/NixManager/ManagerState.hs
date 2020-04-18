{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-|
  Description: The "root" manager state, containing the substates for the tabs (notebook pages). To be used with gi-gtk-declarative-app-simple model.

The "root" manager state, containing the substates for the tabs (notebook pages). To be used with gi-gtk-declarative-app-simple model.
  -}
module NixManager.ManagerState
  ( msAdminState
  , msServiceState
  , msPackagesState
  , msHMServiceState
  , msHMAdminState
  , msHMPackagesState
  , ManagerState(..)
  )
where

import           Control.Lens                   ( makeLenses )
import qualified NixManager.Services.State     as Services
import qualified NixManager.Admin.State        as Admin
import qualified NixManager.Packages.State     as Packages
import qualified NixManager.HMServices.State   as HMServices
import qualified NixManager.HMAdmin.State      as HMAdmin
import qualified NixManager.HMPackages.State   as HMPackages

-- | The root manager state
data ManagerState = ManagerState {
     _msPackagesState :: Packages.State -- ^ State for the packages tab
   , _msServiceState :: Services.State -- ^ State for the services tab
   , _msAdminState :: Admin.State -- ^ State for the administration tab
   , _msHMServiceState :: HMServices.State -- ^ State for the home-manager services tab
   , _msHMAdminState :: HMAdmin.State -- ^ State for the home-manager administration tab
   , _msHMPackagesState :: HMPackages.State -- ^ State for the home-manager packages tab
   }

makeLenses ''ManagerState

