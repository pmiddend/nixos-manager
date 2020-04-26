{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-|
  Description: The "root" manager state, containing the substates for the tabs (notebook pages). To be used with gi-gtk-declarative-app-simple model.

The "root" manager state, containing the substates for the tabs (notebook pages). To be used with gi-gtk-declarative-app-simple model.
  -}
module NixManager.ManagerState
  ( ManagerState(..)
  )
where

import qualified NixManager.Services.State     as Services
import qualified NixManager.Admin.State        as Admin
import qualified NixManager.Packages.State     as Packages
import qualified NixManager.HMServices.State   as HMServices
import qualified NixManager.HMAdmin.State      as HMAdmin
import qualified NixManager.HMPackages.State   as HMPackages
import           GHC.Generics                   ( Generic )

-- | The root manager state
data ManagerState = ManagerState {
     packagesState :: Packages.State -- ^ State for the packages tab
   , serviceState :: Services.State -- ^ State for the services tab
   , adminState :: Admin.State -- ^ State for the administration tab
   , hmServiceState :: HMServices.State -- ^ State for the home-manager services tab
   , hmAdminState :: HMAdmin.State -- ^ State for the home-manager administration tab
   , hmPackagesState :: HMPackages.State -- ^ State for the home-manager packages tab
   } deriving(Generic)

