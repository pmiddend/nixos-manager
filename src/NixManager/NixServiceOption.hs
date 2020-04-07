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
                                                , fromEither
                                                )
import           Data.ByteString.Lazy           ( ByteString
                                                , readFile
                                                )
import           Data.Text                      ( Text )
import           NixManager.NixLocation
                                                ( NixLocation(NixLocation) )
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

data NixServiceOption = NixServiceOption {
   _optionDescription :: Text
  , _optionLoc :: NixLocation
  , _optionType :: Either Text NixServiceOptionType
  , _optionValue :: Maybe NixExpr
  } deriving(Show)

makeLenses ''NixServiceOption

instance FromJSON NixServiceOption where
  parseJSON (Object v) = do
    objectType <- v .: "type"
    let realOptionType = parseNixServiceOptionType objectType
    description <- v .: "description"
    loc         <- v .: "loc"
    -- pure $ NixServiceOption (convertJson objectType <$> defaultValue)
    pure $ NixServiceOption description (NixLocation loc) realOptionType Nothing
  parseJSON _ = mzero


decodeOptions :: ByteString -> TextualError (Map Text NixServiceOption)
decodeOptions =
  ( addToError "Couldn't read the options JSON file. The error was: "
    . fromEither
    )
    . eitherDecode

optionsFileName :: IsString s => s
optionsFileName = "options.json"

desiredOptionsFileLocation :: IO FilePath
desiredOptionsFileLocation =
  getXdgDirectory XdgCache (appName </> optionsFileName)

locateOptionsFile :: IO (Maybe FilePath)
locateOptionsFile = do
  optionsPath <- getXdgDirectory XdgCache (appName </> optionsFileName)
  defExists   <- doesFileExist optionsPath
  if defExists then pure (Just optionsPath) else pure Nothing

readOptionsFile :: FilePath -> IO (TextualError (Map Text NixServiceOption))
readOptionsFile fp = decodeOptions <$> readFile fp

