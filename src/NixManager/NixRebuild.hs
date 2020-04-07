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
import           NixManager.Util                ( showText
                                                , mwhen
                                                )
import           NixManager.PosixTools          ( mkdir
                                                , cp
                                                , mv
                                                )
import           NixManager.Bash                ( Expr
                                                  ( And
                                                  , Command
                                                  , Or
                                                  , Subshell
                                                  , Then
                                                  )
                                                , Arg(LiteralArg)
                                                , devNullify
                                                )
import           Prelude                 hiding ( readFile )
import           NixManager.NixPackages         ( locateLocalPackagesFileMaybeCreate
                                                , locateRootPackagesFile
                                                )
import           NixManager.NixService          ( locateLocalServicesFileMaybeCreate
                                                , locateRootServicesFile
                                                )
import           NixManager.Process             ( runProcess
                                                , ProcessData
                                                , waitUntilFinished
                                                )
import           System.FilePath                ( (-<.>) )
import           NixManager.NixRebuildMode      ( NixRebuildMode
                                                , isDry
                                                , rebuildModeToText
                                                )
import           NixManager.NixRebuildUpdateMode
                                                ( NixRebuildUpdateMode
                                                  ( NixRebuildUpdateUpdate
                                                  , NixRebuildUpdateRollback
                                                  )
                                                )


nixosRebuild :: NixRebuildMode -> NixRebuildUpdateMode -> Expr
nixosRebuild _mode _updateMode = Command "sleep" ["3s"]
-- nixosRebuild :: NixRebuildMode -> NixRebuildUpdateMode -> Expr
-- nixosRebuild mode updateMode = Command
--   "nixos-rebuild"
--   (  [LiteralArg (rebuildModeToText mode)]
--   <> mwhen (updateMode == NixRebuildUpdateUpdate)   ["--upgrade"]
--   <> mwhen (updateMode == NixRebuildUpdateRollback) ["--rollback"]
--   )

copyToOld :: FilePath -> Expr
copyToOld fn = cp fn (fn -<.> "old")

moveFromOld :: FilePath -> Expr
moveFromOld fn = mv (fn -<.> "old") fn

rollbackExpr :: IO Expr
rollbackExpr = do
  rootPackagesFile <- locateRootPackagesFile
  rootServicesFile <- locateRootServicesFile
  pure (moveFromOld rootServicesFile `Then` moveFromOld rootPackagesFile)

installExpr :: NixRebuildMode -> NixRebuildUpdateMode -> IO Expr
installExpr rebuildMode updateMode = do
  localPackagesFile <- locateLocalPackagesFileMaybeCreate
  rootPackagesFile  <- locateRootPackagesFile
  localServicesFile <- locateLocalServicesFileMaybeCreate
  rootServicesFile  <- locateRootServicesFile
  rollback          <- rollbackExpr
  let copyOldFiles = devNullify
        (copyToOld rootServicesFile `Then` copyToOld rootPackagesFile)
      copyToRoot =
        cp localPackagesFile rootPackagesFile
          `And` cp localServicesFile rootServicesFile
      finalOperator = if isDry rebuildMode then Then else Or
  pure
    $ ((mkdir True [rootManagerPath] `And` copyOldFiles) `Then` copyToRoot)
    `And` nixosRebuild rebuildMode updateMode
    `finalOperator` Subshell (devNullify rollback)

rollbackRebuild :: Password -> IO ()
rollbackRebuild password = do
  rollback <- rollbackExpr
  result   <- runProcess (Just (encodeUtf8 (getPassword password)))
                         (sudoExpr rollback)
  void (waitUntilFinished result)

rebuild :: NixRebuildMode -> NixRebuildUpdateMode -> Password -> IO ProcessData
rebuild rebuildMode updateMode password =
  installExpr rebuildMode updateMode
    >>= runProcess (Just (encodeUtf8 (getPassword password)))
    .   sudoExpr

