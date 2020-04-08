{-|
  Description: Defines 'NixLocation', a type representing a dot-separated “location” inside nixpkgs.
 -}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.NixLocation
  ( NixLocation(NixLocation)
  , flattenLocation
  , flattenedTail
  , removeLastComponent
  , firstComponent
  , isSingleton
  , isPrefixOf
  , locationDropComponents
  , locationComponents
  , locationFromText
  , flattened
  )
where

import           Debug.Trace                    ( traceShowId )
import qualified Data.List.NonEmpty            as NE
import           Data.Text                      ( Text
                                                , intercalate
                                                , splitOn
                                                )
import           Data.Foldable                  ( toList )
import           NixManager.Util                ( Endo )
import           Control.Lens                   ( to
                                                , Getter
                                                )

-- | A dot-separated, non-empty “location” inside nixpkgs.
newtype NixLocation = NixLocation {
    getLocation :: NE.NonEmpty Text
  }
  deriving(Eq, Show, Ord)

-- | Create a location from a text, splitting on the dot character. Unsafe.
locationFromText :: Text -> NixLocation
locationFromText = NixLocation . NE.fromList . splitOn "."

-- | Lift an endomorphism of nonempties to locations
liftLocation :: Endo (NE.NonEmpty Text) -> Endo NixLocation
liftLocation f (NixLocation xs) = NixLocation (f xs)

-- | Drop some components from a location. Unsafe.
locationDropComponents :: Int -> Endo NixLocation
locationDropComponents i = liftLocation (NE.fromList . NE.drop i)

-- | Number of components in a location
locationComponents :: NixLocation -> Int
locationComponents = NE.length . getLocation

-- | Flatten a location, adding dots inbetween
flattenLocation :: NixLocation -> Text
flattenLocation = intercalate "." . toList . getLocation

-- | Flatten a location, adding dots inbetween (@Getter@ version)
flattened :: Getter NixLocation Text
flattened = to flattenLocation

-- | Does the location only have one component?
isSingleton :: NixLocation -> Bool
isSingleton = (== 1) . locationComponents

-- | Flatten the location’s tail (all but the first component)
flattenTail :: NixLocation -> Text
flattenTail (NixLocation xs) = intercalate "." (NE.tail xs)

-- | Flatten the location’s tail (all but the first component; @Getter@ version)
flattenedTail :: Getter NixLocation Text
flattenedTail = to flattenTail

-- | Remove the location’s last component, possibly returning @Nothing@ on a singleton
removeLastComponent :: NixLocation -> Maybe NixLocation
removeLastComponent (NixLocation option) = case NE.init option of
  [] -> Nothing
  xs -> Just (NixLocation (NE.fromList xs))

-- | Determine if one location is the prefix of another (same as for lists)
isPrefixOf :: NixLocation -> NixLocation -> Bool
isPrefixOf (NixLocation prefix) (NixLocation tester) =
  toList prefix `NE.isPrefixOf` tester

-- | Extract the first component
firstComponent :: NixLocation -> Text
firstComponent (NixLocation (x NE.:| _)) = x
