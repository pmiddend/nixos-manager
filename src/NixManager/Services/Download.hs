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
import           NixManager.NixServiceOption    ( desiredOptionsFileLocation )
import           Control.Exception              ( try )
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

type DownloadResult = TextualError FilePath

type DownloadVar = MVar DownloadResult

data DownloadState = DownloadState {
  _sdsVar :: DownloadVar
  , _sdsThreadId :: ThreadId
  }

makeLenses ''DownloadState

tryDownload :: IO (Either HttpException (Response ByteString))
tryDownload = try (get "https://nixos.org/nixos/options.json")

start :: IO DownloadState
start = do
  resultVar      <- newEmptyMVar
  resultThreadId <- forkIO $ do
    r' <- tryDownload
    case r' of
      Left ex -> putMVar resultVar (Left ("I/O error: " <> showText ex))
      Right response ->
        let sc = response ^. responseStatus . statusCode
        in  if sc == 200
              then do
                optLoc <- desiredOptionsFileLocation
                createDirectoryIfMissing True (dropFileName optLoc)
                writeFile optLoc (response ^. responseBody)
                putMVar resultVar (Right optLoc)
              else putMVar resultVar
                           (Left ("HTTP error, status code: " <> showText sc))
  pure (DownloadState resultVar resultThreadId)

cancel :: DownloadState -> IO ()
cancel = killThread . view sdsThreadId

result :: DownloadState -> IO (Maybe DownloadResult)
result = tryTakeMVar . view sdsVar
