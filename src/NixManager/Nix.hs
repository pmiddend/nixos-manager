{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module NixManager.Nix where

import           Data.Map.Strict                ( Map
                                                , elems
                                                )
import           Data.ByteString.Lazy           ( ByteString
                                                , hGetContents
                                                )
import           System.Process                 ( createProcess
                                                , proc
                                                , std_out
                                                , StdStream(CreatePipe)
                                                )
import           Data.Text                      ( Text )
import           Data.Aeson                     ( FromJSON
                                                , Value(Object)
                                                , parseJSON
                                                , (.:)
                                                , eitherDecode
                                                )
import           Control.Lens                   ( (^.)
                                                , makeLenses
                                                )
import           Data.Text.Lens                 ( unpacked )
import           Control.Monad                  ( mzero )


data NixPackage = NixPackage {
    _npName :: Text
  , _npVersion :: Text
  , _npDescription :: Text
  } deriving(Eq,Show)

makeLenses ''NixPackage

instance FromJSON NixPackage where
  parseJSON (Object v) =
    NixPackage <$> v .: "pkgName" <*> v .: "version" <*> v .: "description"
  parseJSON _ = mzero

decodeNixSearchResult :: ByteString -> Either String (Map Text NixPackage)
decodeNixSearchResult = eitherDecode

nixSearch :: Text -> IO (Either String [NixPackage])
nixSearch t = do
  (_, Just hout, _, _) <- createProcess
    (proc "nix" ["search", t ^. unpacked, "--json"]) { std_out = CreatePipe }
  out <- hGetContents hout
  pure (elems <$> decodeNixSearchResult out)

nixSearchUnsafe :: Text -> IO [NixPackage]
nixSearchUnsafe t = do
  result <- nixSearch t
  case result of
    Left  e -> error e
    Right v -> pure v

