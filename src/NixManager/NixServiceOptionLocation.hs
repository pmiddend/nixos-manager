{-# LANGUAGE OverloadedStrings #-}
module NixManager.NixServiceOptionLocation
  ( NixServiceOptionLocation
  , flattenOptionLocation
  , optionLocationFromList
  , flattenedTail
  , removeLastComponent
  , needsMarkup
  , isPrefixOf
  , locationDropComponents
  , locationComponents
  )
where

import           Debug.Trace                    ( traceShowId )
import qualified Data.List.NonEmpty            as NE
import           Data.Text                      ( Text
                                                , intercalate
                                                )
import           Data.Foldable                  ( toList )
import           NixManager.Util                ( Endo )

type NixServiceOptionLocation = NE.NonEmpty Text

locationDropComponents :: Int -> Endo NixServiceOptionLocation
locationDropComponents i xs = NE.fromList (NE.drop i xs)

locationComponents :: NixServiceOptionLocation -> Int
locationComponents = NE.length

optionLocationFromList :: [Text] -> NixServiceOptionLocation
optionLocationFromList = NE.fromList

flattenOptionLocation :: NixServiceOptionLocation -> Text
flattenOptionLocation = intercalate "." . toList

needsMarkup :: NixServiceOptionLocation -> Bool
needsMarkup (_ NE.:| []) = traceShowId True
needsMarkup _            = False

globalOptionsMagicString :: Text
globalOptionsMagicString = "Global options"

flattenedTail :: NixServiceOptionLocation -> Text
flattenedTail (_ NE.:| []) = globalOptionsMagicString
flattenedTail xs           = intercalate "." (NE.tail xs)

removeLastComponent
  :: NixServiceOptionLocation -> Maybe NixServiceOptionLocation
removeLastComponent option = case NE.init option of
  [] -> Nothing
  xs -> Just (NE.fromList xs)

isPrefixOf :: NixServiceOptionLocation -> NixServiceOptionLocation -> Bool
isPrefixOf prefix tester = toList prefix `NE.isPrefixOf` tester
