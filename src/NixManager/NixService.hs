{-|
  Description: Contains the "NixService" type (representing a service plus its options), and companion functions
Contains the "NixService" type (representing a service plus its options), and companion functions
  -}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
module NixManager.NixService
  ( NixService(NixService)
  , serviceLoc
  , serviceOptions
  , makeServices
  )
where

import           NixManager.NixLocation         ( NixLocation
                                                , removeLastComponent
                                                , isPrefixOf
                                                )
import           NixManager.NixServiceOption    ( NixServiceOption )
import           Data.Text                      ( Text )
import           Data.Map.Strict                ( Map
                                                , elems
                                                , insertWith
                                                , toList
                                                )
import qualified Data.Set                      as Set
import           NixManager.Util                ( Endo )
import           Control.Lens                   ( (^.)
                                                , view
                                                )
import           Data.Maybe                     ( mapMaybe )
import           GHC.Generics                   ( Generic )
import           Data.Generics.Labels           ( )

-- | Represents a service with a location and some options.
data NixService = NixService {
    serviceLoc :: NixLocation -- ^ Service location
  , serviceOptions :: [NixServiceOption] -- ^ Service options
  } deriving(Show, Generic)

-- | Create a list of services from a map (such as the map contained in the @options.json@ file)
makeServices :: Map Text NixServiceOption -> [NixService]
makeServices options' =
  let
    options = elems options'
    servicePaths :: Set.Set NixLocation
    servicePaths = Set.fromList
      (removeLastComponent `mapMaybe` (view #optionLoc <$> options))
    serviceForOption :: NixServiceOption -> Maybe NixLocation
    serviceForOption opt =
      case Set.lookupLT (opt ^. #optionLoc) servicePaths of
        Nothing     -> Nothing
        Just result -> if result `isPrefixOf` (opt ^. #optionLoc)
          then Just result
          else Nothing
    transducer :: NixServiceOption -> Endo (Map NixLocation [NixServiceOption])
    transducer opt m = case serviceForOption opt of
      Nothing          -> m
      Just serviceLoc' -> insertWith (<>) serviceLoc' [opt] m
    serviceMap = foldr transducer mempty options
  in
    uncurry NixService <$> toList serviceMap

