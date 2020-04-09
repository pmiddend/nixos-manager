{-|
  Description: Determine if the user made any changes which will have to be applied.
 -}
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


-- | Avoid boolean blindness by using this enum instead.
data ChangeType = NoChanges -- ^ No changes to apply
                | Changes   -- ^ There are changes to apply
                deriving (Eq, Bounded, Enum)

-- | Determine if two files, a local one and a root one, are equal, treating “local file doesn’t” exist as “we have no changes to apply regarding that file”.
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

-- | Determine if there are changes that have to be applied.
determineChanges :: IO ChangeType
determineChanges = do
  packagesEqual <- determineFilesEqual locateLocalPackagesFile
                                       locateRootPackagesFile
  servicesEqual <- determineFilesEqual locateLocalServicesFile
                                       locateRootServicesFile
  pure (if packagesEqual && servicesEqual then NoChanges else Changes)
