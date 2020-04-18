{-|
  Description: Contains the actual GUI (widgets) for the Packages tab
Contains the actual GUI (widgets) for the Packages tab
  -}
{-# LANGUAGE FlexibleContexts #-}
module NixManager.HMPackages.View
  ( packagesBox
  )
where

import qualified NixManager.View.PackageEditView
                                               as PackageEditView
import           NixManager.HMPackages.Event    ( Event(EventPackageEditView) )
import           Control.Lens                   ( (^.) )
import           NixManager.ManagerState        ( msHMPackagesState )
import           NixManager.ManagerEvent        ( ManagerEvent
                                                  ( ManagerEventHMPackages
                                                  )
                                                )

-- | The package list
packagesBox s =
  ManagerEventHMPackages . EventPackageEditView <$> PackageEditView.packagesBox
    (s ^. msHMPackagesState)
