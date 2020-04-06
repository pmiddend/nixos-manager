{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
module NixManager.Process
  ( ProcessOutput
  , ProcessData
  , runProcess
  , terminate
  , waitUntilFinished
  , getProcessId
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
                                                , hGetContents
                                                )
import           Control.Lens                   ( makeLenses
                                                , view
                                                , (^.)
                                                )
import           System.Process                 ( ProcessHandle
                                                , getProcessExitCode
                                                , createProcess
                                                , Pid
                                                , getPid
                                                , CreateProcess(..)
                                                , CmdSpec(ShellCommand)
                                                , StdStream(CreatePipe)
                                                , terminateProcess
                                                , interruptProcessGroupOf
                                                , waitForProcess
                                                )
import           System.IO                      ( Handle )
import           System.Exit                    ( ExitCode )
import           NixManager.Bash             ( Expr
                                                , evalExpr
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

terminate :: ProcessData -> IO ()
terminate = terminateProcess . view pdProcessHandle

exprToCmdSpec :: Expr -> CmdSpec
exprToCmdSpec x = ShellCommand (unpack (evalExpr x))

noStdin :: Maybe ByteString
noStdin = Nothing

getProcessId :: ProcessData -> IO (Maybe Pid)
getProcessId = getPid . view pdProcessHandle

runProcess :: Maybe ByteString -> Expr -> IO ProcessData
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

waitUntilFinished :: ProcessData -> IO ProcessOutput
waitUntilFinished pd = do
  stdout   <- hGetContents (pd ^. pdStdoutHandle)
  stderr   <- hGetContents (pd ^. pdStderrHandle)
  exitCode <- waitForProcess (pd ^. pdProcessHandle)
  pure (ProcessOutput stdout stderr (First (Just exitCode)))

updateProcess :: ProcessData -> IO ProcessOutput
updateProcess pd = do
  newStdout   <- hGetNonBlocking (pd ^. pdStdoutHandle) 1024
  newStderr   <- hGetNonBlocking (pd ^. pdStderrHandle) 1024
  newExitCode <- getProcessExitCode (pd ^. pdProcessHandle)
  pure (ProcessOutput newStdout newStderr (First newExitCode))
