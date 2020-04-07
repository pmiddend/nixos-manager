{-# LANGUAGE OverloadedStrings #-}
module NixManager.NixLocation
  ( NixLocation(NixLocation)
  , flattenLocation
  , locationFromList
  , flattenedTail
  , removeLastComponent
  , firstComponent
  , needsMarkup
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
import Control.Lens(to, Getter)

newtype NixLocation = NixLocation {
    getLocation :: NE.NonEmpty Text
  }
  deriving(Eq, Show, Ord)

locationFromText :: Text -> NixLocation
locationFromText = NixLocation . NE.fromList . splitOn "."

liftLocation :: Endo (NE.NonEmpty Text) -> Endo NixLocation
liftLocation f (NixLocation xs) = NixLocation (f xs)

locationDropComponents :: Int -> Endo NixLocation
locationDropComponents i = liftLocation (NE.fromList . NE.drop i)

locationComponents :: NixLocation -> Int
locationComponents = NE.length . getLocation

locationFromList :: [Text] -> NixLocation
locationFromList = NixLocation . NE.fromList

flattenLocation :: NixLocation -> Text
flattenLocation = intercalate "." . toList . getLocation

flattened :: Getter NixLocation Text
flattened = to flattenLocation

needsMarkup :: NixLocation -> Bool
needsMarkup (NixLocation (_ NE.:| [])) = traceShowId True
needsMarkup _            = False

globalOptionsMagicString :: Text
globalOptionsMagicString = "Global options"

flattenTail :: NixLocation -> Text
flattenTail (NixLocation (_ NE.:| [])) = globalOptionsMagicString
flattenTail (NixLocation xs)           = intercalate "." (NE.tail xs)

flattenedTail :: Getter NixLocation Text
flattenedTail = to flattenTail

removeLastComponent
  :: NixLocation -> Maybe NixLocation
removeLastComponent (NixLocation option) = case NE.init option of
  [] -> Nothing
  xs -> Just (NixLocation (NE.fromList xs))

isPrefixOf :: NixLocation -> NixLocation -> Bool
isPrefixOf (NixLocation prefix) (NixLocation tester) = toList prefix `NE.isPrefixOf` tester

firstComponent :: NixLocation -> Text
firstComponent (NixLocation (x NE.:| _)) = x
