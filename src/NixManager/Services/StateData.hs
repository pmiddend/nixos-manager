{-|
  Description: Contains the service tab data, assuming we have successfully read the options JSON File

Contains the service tab data, assuming we have successfully read the options JSON File
  -}
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

-- | Contains the service tab data, assuming we have successfully read the options JSON File
data StateData = StateData {
    _sdCache :: [NixService] -- ^ The list of all services
  , _sdSelectedIdx :: Maybe Int -- ^ The currently selected service in the list
  , _sdExpression :: NixExpr -- ^ The current service expression
  , _sdSearchString :: Text -- ^ The current service search string
  , _sdCategoryIdx :: Int -- ^ The currently selected service category, see "NixManager.Services.ServiceCategory"
  }

makeLenses ''StateData
