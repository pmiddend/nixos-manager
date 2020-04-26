{-|
  Description: Contains the service tab data, assuming we have successfully read the options JSON File

Contains the service tab data, assuming we have successfully read the options JSON File
  -}
{-# LANGUAGE DeriveGeneric #-}
module NixManager.Services.StateData
  ( StateData(StateData)
  )
where

import           NixManager.NixService          ( NixService )
import           NixManager.NixExpr             ( NixExpr )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

-- | Contains the service tab data, assuming we have successfully read the options JSON File
data StateData = StateData {
    cache :: [NixService] -- ^ The list of all services
  , selectedIdx :: Maybe Int -- ^ The currently selected service in the list
  , expression :: NixExpr -- ^ The current service expression
  , searchString :: Text -- ^ The current service search string
  , categoryIdx :: Int -- ^ The currently selected service category, see "NixManager.Services.ServiceCategory"
  } deriving(Generic)
