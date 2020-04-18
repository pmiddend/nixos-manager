{-|
  Description: Contains all the state for the Packages tab
  -}
module NixManager.Packages.State
  ( State
  , initState
  , emptyState
  )
where

import           NixManager.NixPackagesUtil     ( readPackageCache )
import           NixManager.Util                ( TextualError
                                                , ifSuccessIO
                                                )
import qualified NixManager.View.PackageEditView
                                               as PackageEditView

type State = PackageEditView.State

-- | The initial Packages tab state (needs to read the package cache, hence the side-effect)
initState :: IO (TextualError State)
initState =
  ifSuccessIO readPackageCache (pure . Right . PackageEditView.initState)

-- | An empty package state (we need this so we can "no-init" the NixOS package view if HM is enabled)
emptyState :: State
emptyState = PackageEditView.emptyState
