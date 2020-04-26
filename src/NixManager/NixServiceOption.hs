{-|
  Description: Provides the type for a service option, as read from the @options.json@ file as well as functions to read and write it.
Provides the type for a service option, as read from the @options.json@ file as well as functions to read and write it.
  -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module NixManager.NixServiceOption
  ( NixServiceOption
  , optionDescription
  , optionLoc
  , optionType
  , optionValue
  , readOptionsFile
  , locateOptionsFile
  , desiredOptionsFileLocation
  )
where

import           System.FilePath                ( (</>) )
import           NixManager.Constants           ( appName )
import           Data.String                    ( IsString )
import           System.Directory               ( getXdgDirectory
                                                , XdgDirectory(XdgCache)
                                                , doesFileExist
                                                )
import           Control.Monad                  ( mzero )
import           Prelude                 hiding ( readFile )
import           Data.Map.Strict                ( Map )
import           NixManager.Util                ( TextualError
                                                , addToError
                                                , fromStringEither
                                                )
import           Data.ByteString.Lazy           ( ByteString
                                                , readFile
                                                )
import           Data.Text                      ( Text )
import           NixManager.NixLocation         ( NixLocation(NixLocation) )
import           NixManager.NixServiceOptionType
                                                ( NixServiceOptionType
                                                , parseNixServiceOptionType
                                                )
import           NixManager.NixExpr             ( NixExpr )
import           Data.Aeson                     ( FromJSON
                                                , parseJSON
                                                , Value(Object)
                                                , (.:)
                                                , eitherDecode
                                                )
import           GHC.Generics                   ( Generic )

-- | Service option, as read from the @options.json@ file
data NixServiceOption = NixServiceOption {
    optionDescription :: Text -- ^ The option description
  , optionLoc :: NixLocation -- ^ The option location
  , optionType :: TextualError NixServiceOptionType -- ^ The type, possibly parsed
  , optionValue :: Maybe NixExpr -- ^ The option value, if present
  } deriving(Show, Generic)

instance FromJSON NixServiceOption where
  parseJSON (Object v) = do
    objectType <- v .: "type"
    let realOptionType = parseNixServiceOptionType objectType
    description <- v .: "description"
    loc         <- v .: "loc"
    -- pure $ NixServiceOption (convertJson objectType <$> defaultValue)
    pure $ NixServiceOption description (NixLocation loc) realOptionType Nothing
  parseJSON _ = mzero

-- | Decode a bytestring into a map of options
decodeOptions :: ByteString -> TextualError (Map Text NixServiceOption)
decodeOptions =
  ( addToError "Couldn't read the options JSON file. The error was: "
    . fromStringEither
    )
    . eitherDecode

-- | The options JSON file name
optionsFileName :: IsString s => s
optionsFileName = "options.json"

-- | Where to put the @options.json@ after downloading it
desiredOptionsFileLocation :: IO FilePath
desiredOptionsFileLocation =
  getXdgDirectory XdgCache (appName </> optionsFileName)

-- | Locate the @options.json@ file
locateOptionsFile :: IO (Maybe FilePath)
locateOptionsFile = do
  optionsPath <- getXdgDirectory XdgCache (appName </> optionsFileName)
  defExists   <- doesFileExist optionsPath
  if defExists then pure (Just optionsPath) else pure Nothing

-- | Read the options file into a map
readOptionsFile :: FilePath -> IO (TextualError (Map Text NixServiceOption))
readOptionsFile fp = decodeOptions <$> readFile fp

