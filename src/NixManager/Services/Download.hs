{-|
  Description: Contains all functions relating to the service JSON download
Contains all functions relating to the service JSON download
  -}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.Services.Download
  ( result
  , start
  , cancel
  , DownloadState
  , DownloadResult
  )
where

import           Network.HTTP.Client            ( HttpException )
import           Prelude                 hiding ( writeFile )
import           System.FilePath                ( dropFileName )
import           System.Directory               ( createDirectoryIfMissing )
import           System.Process                 ( readProcess )
import           NixManager.NixServiceOption    ( desiredOptionsFileLocation )
import           Control.Exception              ( try
                                                , Exception
                                                , SomeException
                                                )
import           Network.Wreq                   ( get
                                                , Response
                                                , responseStatus
                                                , responseBody
                                                , statusCode
                                                )
import           Control.Concurrent.MVar        ( MVar
                                                , newEmptyMVar
                                                , putMVar
                                                , tryTakeMVar
                                                )
import           Control.Concurrent             ( forkIO
                                                , ThreadId
                                                , killThread
                                                )
import           NixManager.Util                ( TextualError
                                                , showText
                                                )
import           Control.Lens                   ( makeLenses
                                                , view
                                                , (^.)
                                                )
import           Data.ByteString.Lazy           ( ByteString
                                                , writeFile
                                                )
import           Codec.Compression.Brotli       ( decompress )

-- | When the download finishes, this type contains either an error or the filepath to the downloaded file
type DownloadResult = TextualError FilePath

-- | We regularly check for the current state of the download. Locking is done with this 'MVar'
type DownloadVar = MVar DownloadResult

-- | The current state of the download
data DownloadState = DownloadState {
    _sdsVar :: DownloadVar -- ^ The mutex to check
  , _sdsThreadId :: ThreadId -- ^ The thread we started the download in
  }

makeLenses ''DownloadState

-- | Try to decompress the received data (it’s Brotli compressed nowadays)
tryDecompress
  :: Response ByteString -> IO (Either SomeException (Response ByteString))
tryDecompress bs = try (pure (decompress <$> bs))

-- | Try to download and decompress the options file
tryDownload :: IO (Either SomeException (Response ByteString))
tryDownload = do
  errorOrResponse <- try
    (get "https://channels.nixos.org/nixos-19.09/options.json.br")
  case errorOrResponse of
    -- FIXME: This is pretty ugly.
    Left  e -> pure (Left e)
    Right v -> tryDecompress v

-- | Start the download, return its state
start :: IO DownloadState
start = do
  resultVar      <- newEmptyMVar
  resultThreadId <- forkIO $ do
    optLoc <- desiredOptionsFileLocation
    createDirectoryIfMissing True (dropFileName optLoc)
    readProcess "nix-build"
      [ "--out-link", optLoc, "-E"
      , "with import <nixpkgs> {}; let eval = import (pkgs.path + \"/nixos/lib/eval-config.nix\") { modules = []; }; opts = (nixosOptionsDoc { options = eval.options; }).optionsJSON; in runCommandLocal \"options.json\" { opts = opts; } '' cp \"$opts/share/doc/nixos/options.json\" $out ''" ] ""
    putMVar resultVar (Right optLoc)
  pure (DownloadState resultVar resultThreadId)

-- | Cancel a started download
cancel :: DownloadState -> IO ()
cancel = killThread . view sdsThreadId

-- | Return the result of the download, maybe
result :: DownloadState -> IO (Maybe DownloadResult)
result = tryTakeMVar . view sdsVar
