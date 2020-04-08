{-|
  Description: The "root" event type, to be used with the gi-gtk-declarative-app-simple model. The different tabs (notebook pages) use their own events, which are also manager events.
  #-}
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

-- | The root event type
data ManagerEvent = ManagerEventClosed -- ^ Used only for closing the application
                  | ManagerEventDiscard -- ^ Used in situations where we /must/ return an event but don't want to actually do something in response (to other peoeple: can this be removed somehow?)
                  | ManagerEventAdmin Admin.Event -- ^ Specific event for the "Admin" tab (the first one)
                  | ManagerEventServices Services.Event -- ^ Specific event for the "Services" tab (the third one)
                  | ManagerEventPackages Packages.Event -- ^ Specific event for the "Packages" tab (the second one) 

makePrisms ''ManagerEvent

-- | Shortcut to construct an 'NixManager.Admin.Event' (in a 'Just', simply because of the way 'Transition' is defined)
adminEvent :: Admin.Event -> Maybe ManagerEvent
adminEvent = Just . ManagerEventAdmin

-- | Shortcut to construct an 'NixManager.Services.Event' (in a 'Just', simply because of the way 'Transition' is defined)
servicesEvent :: Services.Event -> Maybe ManagerEvent
servicesEvent = Just . ManagerEventServices

-- | Shortcut to construct an 'NixManager.Packages.Event' (in a 'Just', simply because of the way 'Transition' is defined)
packagesEvent :: Packages.Event -> Maybe ManagerEvent
packagesEvent = Just . ManagerEventPackages

-- | A special transition that doesn't have side-effects or emit an event. 
pureTransition :: state -> Transition state ManagerEvent
pureTransition x = Transition x (pure Nothing)
