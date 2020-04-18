{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
  Description: State data for the home-manager generations view in case the generations command succeeded
State data for the home-manager generations view in case the generations command succeeded
  -}
module NixManager.HMAdmin.GenerationsData
  ( GenerationsData(GenerationsData)
  , gdGenerations
  , gdMessage
  , gdSelectedGenerationIdx
  , gdSelectedGeneration
  )
where

import           NixManager.HMGenerations       ( GenerationLine )
import           NixManager.Message             ( Message )
import           Control.Lens                   ( makeLenses
                                                , Traversal'
                                                , traversed
                                                )
import           NixManager.Util                ( indirectIndexTraversal )

-- | State data for the home-manager generations view in case the generations command succeeded
data GenerationsData = GenerationsData {
    _gdSelectedGenerationIdx :: Maybe Int -- ^ Currently selected generation, if any
  , _gdMessage :: Maybe Message -- ^ A message to display (for example, about a success while switching generations)
  , _gdGenerations :: [GenerationLine] -- ^ The actual generations
  }

makeLenses ''GenerationsData

-- | Traversal over the currently selected generation(s)
gdSelectedGeneration :: Traversal' GenerationsData GenerationLine
gdSelectedGeneration =
  indirectIndexTraversal gdSelectedGenerationIdx (gdGenerations . traversed)
