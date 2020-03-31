module NixManager.Changes
  ( determineChanges
  , ChangeType(..)
  )
where

import           System.Directory               ( doesFileExist )
import           NixManager.Util                ( filesEqual )
import           NixManager.NixPackages         ( locateLocalPackagesFile
                                                , locateRootPackagesFile
                                                )
import           NixManager.NixService          ( locateLocalServicesFile
                                                , locateRootServicesFile
                                                )


data ChangeType = NoChanges
                | Changes
                deriving (Eq, Bounded, Enum)

determineFilesEqual :: IO FilePath -> IO FilePath -> IO Bool
determineFilesEqual fp' rootFp' = do
  fp          <- fp'
  rootFp      <- rootFp'
  localExists <- doesFileExist fp
  if localExists
    then do
      rootExists <- doesFileExist rootFp
      if rootExists then filesEqual fp rootFp else pure False
    else pure True

determineChanges :: IO ChangeType
determineChanges = do
  packagesEqual <- determineFilesEqual locateLocalPackagesFile
                                       locateRootPackagesFile
  servicesEqual <- determineFilesEqual locateLocalServicesFile
                                       locateRootServicesFile
  pure (if packagesEqual && servicesEqual then NoChanges else Changes)
