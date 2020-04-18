{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-|
  Description: The "root" event type, to be used with the gi-gtk-declarative-app-simple model. The different tabs (notebook pages) use their own events, which are also manager events.
  #-}
module NixManager.ManagerEvent
  ( ManagerEvent(..)
  , adminEvent
  , servicesEvent
  , packagesEvent
  , hmServicesEvent
  , hmPackagesEvent
  , hmAdminEvent
  , pureTransition
  , liftUpdate
  )
where

import           Control.Lens                   ( makePrisms
                                                , (^.)
                                                , Lens'
                                                , set
                                                )
import           NixManager.Services.Event     as Services
import qualified NixManager.Packages.Event     as Packages
import qualified NixManager.Admin.Event        as Admin
import qualified NixManager.HMAdmin.Event      as HMAdmin
import qualified NixManager.HMPackages.Event   as HMPackages
import qualified NixManager.HMServices.Event   as HMServices
import           GI.Gtk.Declarative.App.Simple  ( Transition(Transition, Exit) )

-- | The root event type
data ManagerEvent = ManagerEventClosed -- ^ Used only for closing the application
                  | ManagerEventDiscard -- ^ Used in situations where we /must/ return an event but don't want to actually do something in response (to other peoeple: can this be removed somehow?)
                  | ManagerEventAdmin Admin.Event -- ^ Specific event for the "Admin" tab (the first one)
                  | ManagerEventServices Services.Event -- ^ Specific event for the "Services" tab (the third one)
                  | ManagerEventPackages Packages.Event -- ^ Specific event for the "Packages" tab (the second one) 
                  | ManagerEventHMServices HMServices.Event -- ^ Specific event for the "Home Manager Services" tab 
                  | ManagerEventHMAdmin HMAdmin.Event -- ^ Specific event for the "Home Manager Services" tab 
                  | ManagerEventHMPackages HMPackages.Event -- ^ Specific event for the "Home Manager Packages" tab 

makePrisms ''ManagerEvent

-- | Shortcut to construct an 'NixManager.Admin.Event' (in a 'Just', simply because of the way 'Transition' is defined)
adminEvent :: Admin.Event -> Maybe ManagerEvent
adminEvent = Just . ManagerEventAdmin

-- | Shortcut to construct an 'NixManager.HMAdmin.Event' (in a 'Just', simply because of the way 'Transition' is defined)
hmAdminEvent :: HMAdmin.Event -> Maybe ManagerEvent
hmAdminEvent = Just . ManagerEventHMAdmin

-- | Shortcut to construct an 'NixManager.HMPackages.Event' (in a 'Just', simply because of the way 'Transition' is defined)
hmPackagesEvent :: HMPackages.Event -> Maybe ManagerEvent
hmPackagesEvent = Just . ManagerEventHMPackages

-- | Shortcut to construct an 'NixManager.Services.Event' (in a 'Just', simply because of the way 'Transition' is defined)
servicesEvent :: Services.Event -> Maybe ManagerEvent
servicesEvent = Just . ManagerEventServices

-- | Shortcut to construct an 'NixManager.Packages.Event' (in a 'Just', simply because of the way 'Transition' is defined)
packagesEvent :: Packages.Event -> Maybe ManagerEvent
packagesEvent = Just . ManagerEventPackages

-- | Shortcut to construct an 'NixManager.HMServices.Event' (in a 'Just', simply because of the way 'Transition' is defined)
hmServicesEvent :: HMServices.Event -> Maybe ManagerEvent
hmServicesEvent = Just . ManagerEventHMServices

-- | A special transition that doesn't have side-effects or emit an event. 
pureTransition :: state -> Transition state ManagerEvent
pureTransition x = Transition x (pure Nothing)

-- | Given an update function for something “deep” in a state, construct and update function more “shallow” in the state
liftUpdate
  :: (innerState -> innerEvent -> Transition innerState innerEvent)
  -> Lens' outerState innerState
  -> (innerEvent -> outerEvent)
  -> outerState
  -> innerEvent
  -> Transition outerState outerEvent
liftUpdate makeTransition getInnerState embedOuterEvent outerState innerEvent =
  case makeTransition (outerState ^. getInnerState) innerEvent of
    Exit -> Exit
    Transition newInnerState eventHandler -> Transition
      (set getInnerState newInnerState outerState)
      ((embedOuterEvent <$>) <$> eventHandler)
