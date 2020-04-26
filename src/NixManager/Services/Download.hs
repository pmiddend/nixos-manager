{-|
  Description: Contains all functions relating to the service JSON download
Contains all functions relating to the service JSON download
  -}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
module NixManager.Services.Download
  ( result
  , start
  , cancel
  , DownloadState
  , DownloadResult
  )
where

import           GHC.Generics                   ( Generic )
import           Data.Validation                ( Validation(Success, Failure) )
import           System.Exit                    ( ExitCode
                                                  ( ExitSuccess
                                                  , ExitFailure
                                                  )
                                                )
import           Prelude                 hiding ( writeFile )
import           System.FilePath                ( dropFileName )
import           System.Directory               ( createDirectoryIfMissing )
import           NixManager.Bash                ( Expr(Command)
                                                , Arg(RawArg)
                                                )
import           NixManager.Process             ( runProcessToFinish
                                                , noStdin
                                                )
import           NixManager.NixServiceOption    ( desiredOptionsFileLocation )
import           Control.Exception              ( try
                                                , SomeException
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
                                                , decodeUtf8
                                                , showText
                                                )
import           Control.Lens                   ( view
                                                , to
                                                , (^.)
                                                , (^?!)
                                                )
import           Data.ByteString.Lazy           ( ByteString
                                                , writeFile
                                                )
import           Data.Text                      ( pack )
import           Data.Monoid                    ( getFirst )

-- | When the download finishes, this type contains either an error or the filepath to the downloaded file
type DownloadResult = TextualError FilePath

-- | We regularly check for the current state of the download. Locking is done with this 'MVar'
type DownloadVar = MVar DownloadResult

-- | The current state of the download
data DownloadState = DownloadState {
    var :: DownloadVar -- ^ The mutex to check
  , threadId :: ThreadId -- ^ The thread we started the download in
  } deriving(Generic)

-- | Start the download, return its state
start :: IO DownloadState
start = do
  resultVar      <- newEmptyMVar
  resultThreadId <- forkIO $ do
    optLoc <- desiredOptionsFileLocation
    createDirectoryIfMissing True (dropFileName optLoc)
    po <- runProcessToFinish noStdin $ Command
      "nix-build"
      (RawArg
      <$> [ "--out-link"
          , pack optLoc
          , "-E"
          , "with import <nixpkgs> {}; let eval = import (pkgs.path + \"/nixos/lib/eval-config.nix\") { modules = []; }; opts = (nixosOptionsDoc { options = eval.options; }).optionsJSON; in runCommandLocal \"options.json\" { opts = opts; } '' cp \"$opts/share/doc/nixos/options.json\" $out ''"
          ]
      )
    putMVar resultVar $ case po ^?! #result . to getFirst of
      Just ExitSuccess        -> Success optLoc
      Just (ExitFailure code) -> Failure
        (  "Building the options file failed with error code "
        <> showText code
        <> ", standard error was:\n\n"
        <> (po ^. #stderr . decodeUtf8)
        )
  pure (DownloadState resultVar resultThreadId)

-- | Cancel a started download
cancel :: DownloadState -> IO ()
cancel = killThread . view #threadId

-- | Return the result of the download, maybe
result :: DownloadState -> IO (Maybe DownloadResult)
result = tryTakeMVar . view #var
