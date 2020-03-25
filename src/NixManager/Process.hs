{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
module NixManager.Process
  ( ProcessOutput
  , ProcessData
  , runProcess
  , updateProcess
  , noStdin
  , poResult
  , poStderr
  , poStdout
  )
where

import           Data.Foldable                  ( for_ )
import           Data.Monoid                    ( First(First) )
import           Data.ByteString                ( ByteString
                                                , hGetNonBlocking
                                                , hPutStr
                                                )
import           Control.Lens                   ( makeLenses
                                                , (^.)
                                                )
import           System.Process                 ( ProcessHandle
                                                , getProcessExitCode
                                                , createProcess
                                                , CreateProcess(..)
                                                , CmdSpec(ShellCommand)
                                                , StdStream(CreatePipe)
                                                )
import           System.IO                      ( Handle )
import           System.Exit                    ( ExitCode )
import           NixManager.BashDsl             ( BashExpr
                                                , evalBashExpr
                                                )
import           Data.Text                      ( unpack )

data ProcessData = ProcessData {
   _pdStdoutHandle :: Handle
  , _pdStderrHandle :: Handle
  , _pdProcessHandle :: ProcessHandle
  }

makeLenses ''ProcessData

data ProcessOutput = ProcessOutput {
  _poStdout :: ByteString
  , _poStderr :: ByteString
  , _poResult :: First ExitCode
  }

instance Semigroup ProcessOutput where
  (ProcessOutput a b c) <> (ProcessOutput a' b' c') =
    ProcessOutput (a <> a') (b <> b') (c <> c')

instance Monoid ProcessOutput where
  mempty = ProcessOutput mempty mempty mempty

makeLenses ''ProcessOutput

exprToCmdSpec :: BashExpr -> CmdSpec
exprToCmdSpec x = ShellCommand (unpack (evalBashExpr x))

noStdin :: Maybe ByteString
noStdin = Nothing

runProcess :: Maybe ByteString -> BashExpr -> IO ProcessData
runProcess stdinString command = do
  (Just hin, Just hout, Just herr, ph) <- createProcess $ CreateProcess
    { cmdspec            = exprToCmdSpec command
    , cwd                = Nothing
    , env                = Nothing
    , std_in             = CreatePipe
    , std_out            = CreatePipe
    , std_err            = CreatePipe
    , close_fds          = False
    , create_group       = False
    , delegate_ctlc      = False
    , detach_console     = False
    , create_new_console = False
    , new_session        = False
    , child_group        = Nothing
    , child_user         = Nothing
    , use_process_jobs   = False
    }
  for_ stdinString (hPutStr hin)
  pure (ProcessData hout herr ph)

updateProcess :: ProcessData -> IO ProcessOutput
updateProcess pd = do
  newStdout   <- hGetNonBlocking (pd ^. pdStdoutHandle) 1024
  newStderr   <- hGetNonBlocking (pd ^. pdStderrHandle) 1024
  newExitCode <- getProcessExitCode (pd ^. pdProcessHandle)
  pure (ProcessOutput newStdout newStderr (First newExitCode))
