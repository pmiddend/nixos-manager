{-|
  Description: Functions and structures relating to the @nixos-rebuild@ command
Functions and structures relating to the @nixos-rebuild@ command
  -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
module NixManager.NixRebuild
  ( rebuild
  , rootManagerPath
  , rollbackRebuild
  , NixRebuildUpdateMode(..)
  )
where

import           Control.Monad                  ( void )
import           NixManager.Password            ( Password
                                                , getPassword
                                                )
import           NixManager.AskPass             ( sudoExpr )
import           NixManager.Constants           ( rootManagerPath )
import           Data.Text.Encoding             ( encodeUtf8 )
import           NixManager.PosixTools          ( mkdir
                                                , cp
                                                , mv
                                                )
import           NixManager.Bash                ( Expr(Command, Subshell)
                                                , Arg(LiteralArg)
                                                , (||.)
                                                , (&&.)
                                                , (>>.)
                                                , devNullify
                                                )
import           Prelude                 hiding ( readFile )
import           NixManager.Util                ( mwhen )
import           NixManager.NixPackagesUtil     ( locateLocalPackagesFileMaybeCreate
                                                , locateRootPackagesFile
                                                )
import           NixManager.NixServicesUtil     ( locateLocalServicesFileMaybeCreate
                                                , locateRootServicesFile
                                                )
import           NixManager.Process             ( runProcess
                                                , ProcessData
                                                , waitUntilFinished
                                                )
import           System.FilePath                ( (-<.>) )
import           NixManager.NixRebuildMode      ( NixRebuildMode
                                                , rebuildModeToText
                                                , isDry
                                                )
import           NixManager.NixRebuildUpdateMode
                                                ( NixRebuildUpdateMode
                                                  ( NixRebuildUpdateUpdate
                                                  , NixRebuildUpdateRollback
                                                  )
                                                )



-- | Bash expression for @nixos-rebuild@ (see the "NixManager.Bash" module)
nixosRebuildExpr :: NixRebuildMode -> NixRebuildUpdateMode -> Expr
-- Turn this on for debugging purposes
-- nixosRebuildExpr _mode _updateMode = Command "sleep" ["3s"]
nixosRebuildExpr mode updateMode = Command
  "nixos-rebuild"
  (  [LiteralArg (rebuildModeToText mode)]
  <> mwhen (updateMode == NixRebuildUpdateUpdate)   ["--upgrade"]
  <> mwhen (updateMode == NixRebuildUpdateRollback) ["--rollback"]
  )

-- | Copy @<file>.<ext>@ to @<file>.old@
copyToOld :: FilePath -> Expr
copyToOld fn = cp fn (fn -<.> "old")

-- | Move @<file>.<old>@ to @<file>@
moveFromOld :: FilePath -> Expr
moveFromOld fn = mv (fn -<.> "old") fn

-- | Expression to rollback a rebuild (by moving the nixos-manager files, not via @nixos-rebuild --rollback@, mind you)
rollbackExpr :: IO Expr
rollbackExpr = do
  rootPackagesFile <- locateRootPackagesFile
  rootServicesFile <- locateRootServicesFile
  pure (moveFromOld rootServicesFile >>. moveFromOld rootPackagesFile)

-- | Expression to call @nixos-rebuild@, after coping the local files to the root location, and possibly rolling that back.
installExpr :: NixRebuildMode -> NixRebuildUpdateMode -> IO Expr
installExpr rebuildMode updateMode = do
  localPackagesFile <- locateLocalPackagesFileMaybeCreate
  rootPackagesFile  <- locateRootPackagesFile
  localServicesFile <- locateLocalServicesFileMaybeCreate
  rootServicesFile  <- locateRootServicesFile
  rollback          <- rollbackExpr
  let copyOldFiles =
        devNullify (copyToOld rootServicesFile >>. copyToOld rootPackagesFile)
      copyToRoot =
        cp localPackagesFile rootPackagesFile
          &&. cp localServicesFile rootServicesFile
      finalOperator = if isDry rebuildMode then (>>.) else (||.)
  pure
    $ ((mkdir True [rootManagerPath] &&. copyOldFiles) >>. copyToRoot)
    &&. nixosRebuildExpr rebuildMode updateMode
    `finalOperator` Subshell (devNullify rollback)

-- | Rollback a rebuild
rollbackRebuild :: Password -> IO ()
rollbackRebuild password = do
  rollback <- rollbackExpr
  result   <- runProcess (Just (encodeUtf8 (getPassword password)))
                         (sudoExpr rollback)
  void (waitUntilFinished result)

-- | Call @nixos-rebuild@, after asking for a password
rebuild :: NixRebuildMode -> NixRebuildUpdateMode -> Password -> IO ProcessData
rebuild rebuildMode updateMode password =
  installExpr rebuildMode updateMode
    >>= runProcess (Just (encodeUtf8 (getPassword password)))
    .   sudoExpr

