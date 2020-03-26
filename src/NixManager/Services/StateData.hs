{-# LANGUAGE TemplateHaskell #-}
module NixManager.Services.StateData
  ( StateData(StateData)
  , sdCache
  , sdSelectedIdx
  , sdExpression
  )
where

import           NixManager.NixService          ( NixService )
import           NixManager.NixExpr             ( NixExpr )
import           Control.Lens                   ( makeLenses )

data StateData = StateData {
    _sdCache :: [NixService]
  , _sdSelectedIdx :: Maybe Int
  , _sdExpression :: NixExpr
  }

makeLenses ''StateData
