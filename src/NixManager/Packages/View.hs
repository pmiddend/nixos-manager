{-|
  Description: Contains the actual GUI (widgets) for the Packages tab
Contains the actual GUI (widgets) for the Packages tab
  -}
{-# LANGUAGE FlexibleContexts #-}
module NixManager.Packages.View
  ( packagesBox
  )
where

import qualified NixManager.View.PackageEditView
                                               as PackageEditView
import           NixManager.Packages.Event      ( Event(EventPackageEditView) )
import           Control.Lens                   ( (^.) )
import           NixManager.ManagerState        ( msPackagesState )
import           NixManager.ManagerEvent        ( ManagerEvent
                                                  ( ManagerEventPackages
                                                  )
                                                )

-- | The package list
packagesBox s =
  ManagerEventPackages . EventPackageEditView <$> PackageEditView.packagesBox
    (s ^. msPackagesState)
