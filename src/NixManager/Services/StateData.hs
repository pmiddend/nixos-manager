{-# LANGUAGE TemplateHaskell #-}
module NixManager.Services.StateData
  ( StateData(StateData)
  , sdCache
  , sdSelectedIdx
  , sdExpression
  , sdSearchString
  , sdCategoryIdx
  )
where

import           NixManager.NixService          ( NixService )
import           NixManager.NixExpr             ( NixExpr )
import           Control.Lens                   ( makeLenses )
import           Data.Text                      ( Text )

data StateData = StateData {
    _sdCache :: [NixService]
  , _sdSelectedIdx :: Maybe Int
  , _sdExpression :: NixExpr
  , _sdSearchString :: Text
  , _sdCategoryIdx :: Int
  }

makeLenses ''StateData
