{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module NixManager.NixServiceOption
  ( NixServiceOption
  , optionDescription
  , optionLoc
  , optionType
  , optionValue
  , readOptionsFile
  , locateOptionsFile
  , NixServiceOptionLocation
  , desiredOptionsFileLocation
  )
where

import           System.Directory               ( doesFileExist )
import           Control.Monad                  ( mzero )
import           Prelude                 hiding ( readFile )
import           Data.Map.Strict                ( Map )
import           NixManager.Util                ( MaybeError
                                                , toEither
                                                , addToError
                                                , fromEither
                                                )
import           Data.ByteString.Lazy           ( ByteString
                                                , readFile
                                                )
import           Data.Text                      ( Text )
import           NixManager.NixServiceOptionType
                                                ( NixServiceOptionType
                                                , parseNixServiceOptionType
                                                )
import           NixManager.NixExpr             ( NixExpr )
import           Control.Lens                   ( makeLenses )
import           Data.Aeson                     ( FromJSON
                                                , parseJSON
                                                , Value(Object)
                                                , (.:)
                                                , eitherDecode
                                                )

type NixServiceOptionLocation = [Text]

data NixServiceOption = NixServiceOption {
   _optionDescription :: Text
  , _optionLoc :: NixServiceOptionLocation
  , _optionType :: Either Text NixServiceOptionType
  , _optionValue :: Maybe NixExpr
  } deriving(Show)

makeLenses ''NixServiceOption

instance FromJSON NixServiceOption where
  parseJSON (Object v) = do
    objectType <- v .: "type"
    let realOptionType = toEither (parseNixServiceOptionType objectType)
    description <- v .: "description"
    loc         <- v .: "loc"
    -- pure $ NixServiceOption (convertJson objectType <$> defaultValue)
    pure $ NixServiceOption description loc realOptionType Nothing
  parseJSON _ = mzero


decodeOptions :: ByteString -> MaybeError (Map Text NixServiceOption)
decodeOptions =
  ( addToError "Couldn't read the options JSON file. The error was: "
    . fromEither
    )
    . eitherDecode

desiredOptionsFileLocation :: IO FilePath
desiredOptionsFileLocation = pure "options.json"

locateOptionsFile :: IO (Maybe FilePath)
locateOptionsFile = do
  defExists <- doesFileExist "options.json"
  if defExists then pure (Just "options.json") else pure Nothing

readOptionsFile :: FilePath -> IO (MaybeError (Map Text NixServiceOption))
readOptionsFile fp = decodeOptions <$> readFile fp

