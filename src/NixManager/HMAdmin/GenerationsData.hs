{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-|
  Description: State data for the home-manager generations view in case the generations command succeeded
State data for the home-manager generations view in case the generations command succeeded
  -}
module NixManager.HMAdmin.GenerationsData
  ( GenerationsData(GenerationsData)
  , selectedGeneration
  )
where

import           NixManager.HMGenerations       ( GenerationLine )
import           NixManager.Message             ( Message )
import           Control.Lens                   ( Traversal'
                                                , traversed
                                                )
import           NixManager.Util                ( indirectIndexTraversal )
import           GHC.Generics                   ( Generic )

-- | State data for the home-manager generations view in case the generations command succeeded
data GenerationsData = GenerationsData {
    selectedGenerationIdx :: Maybe Int -- ^ Currently selected generation, if any
  , message :: Maybe Message -- ^ A message to display (for example, about a success while switching generations)
  , generations :: [GenerationLine] -- ^ The actual generations
  } deriving(Generic)

-- | Traversal over the currently selected generation(s)
selectedGeneration :: Traversal' GenerationsData GenerationLine
selectedGeneration =
  indirectIndexTraversal #selectedGenerationIdx (#generations . traversed)
