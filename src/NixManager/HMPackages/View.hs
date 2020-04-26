{-|
  Description: Contains the actual GUI (widgets) for the Packages tab
Contains the actual GUI (widgets) for the Packages tab
  -}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
module NixManager.HMPackages.View
  ( packagesBox
  )
where

import qualified NixManager.View.PackageEditView
                                               as PackageEditView
import           NixManager.HMPackages.Event    ( Event(EventPackageEditView) )
import           Control.Lens                   ( (^.) )
import           NixManager.ManagerState        ( ManagerState )
import           NixManager.ManagerEvent        ( ManagerEvent
                                                  ( ManagerEventHMPackages
                                                  )
                                                )
import           GI.Gtk.Declarative             ( Widget )

-- | The package list
packagesBox :: ManagerState -> Widget ManagerEvent
packagesBox s =
  ManagerEventHMPackages . EventPackageEditView <$> PackageEditView.packagesBox
    (s ^. #hmPackagesState)
