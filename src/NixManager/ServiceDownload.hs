{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.ServiceDownload
  ( result
  , start
  , cancel
  , ServiceDownloadState
  , ServiceDownloadResult
  )
where

import           Network.HTTP.Client            ( HttpException )
import           Prelude                 hiding ( writeFile )
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
import           NixManager.Util                ( MaybeError(Success, Error)
                                                , showText
                                                )
import           Control.Lens                   ( makeLenses
                                                , view
                                                , (^.)
                                                )
import           Data.ByteString.Lazy           ( ByteString
                                                , writeFile
                                                )

type ServiceDownloadResult = MaybeError FilePath

type ServiceDownloadVar = MVar ServiceDownloadResult

data ServiceDownloadState = ServiceDownloadState {
  _sdsVar :: ServiceDownloadVar
  , _sdsThreadId :: ThreadId
  }

makeLenses ''ServiceDownloadState

tryDownload :: IO (Either HttpException (Response ByteString))
tryDownload = try (get "https://nixos.org/nixos/options.json")

start :: IO ServiceDownloadState
start = do
  resultVar      <- newEmptyMVar
  resultThreadId <- forkIO $ do
    r' <- tryDownload
    case r' of
      Left ex -> putMVar resultVar (Error ("I/O error: " <> showText ex))
      Right response ->
        let sc = response ^. responseStatus . statusCode
        in  if sc == 200
              then do
                optLoc <- desiredOptionsFileLocation
                writeFile optLoc (response ^. responseBody)
                putMVar resultVar (Success optLoc)
              else putMVar
                resultVar
                (Error ("HTTP error, status code: " <> showText sc))
  pure (ServiceDownloadState resultVar resultThreadId)

cancel :: ServiceDownloadState -> IO ()
cancel = killThread . view sdsThreadId

result :: ServiceDownloadState -> IO (Maybe ServiceDownloadResult)
result = tryTakeMVar . view sdsVar
