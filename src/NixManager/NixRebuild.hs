{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
module NixManager.NixRebuild
  ( askPass
  , rebuild
  , rootManagerPath
  , NixRebuildUpdateMode(..)
  )
where

import           NixManager.Constants           ( rootManagerPath )
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( encodeUtf8 )
import           NixManager.Util                ( showText
                                                , mwhen
                                                )
import           NixManager.BashDsl             ( mkdir
                                                , Expr
                                                  ( And
                                                  , Command
                                                  , Or
                                                  , Subshell
                                                  )
                                                , Arg(LiteralArg)
                                                , evalExpr
                                                , cp
                                                , mv
                                                )
import           Prelude                 hiding ( readFile )
import           NixManager.NixPackages         ( locateLocalPackagesFileMaybeCreate
                                                , locateRootPackagesFile
                                                )
import           NixManager.NixService          ( locateLocalServicesFileMaybeCreate
                                                , locateRootServicesFile
                                                )
import           NixManager.Process             ( runProcess
                                                , noStdin
                                                , ProcessData
                                                )
import           System.FilePath                ( (-<.>) )
import           NixManager.NixRebuildMode      ( NixRebuildMode )
import           NixManager.NixRebuildUpdateMode
                                                ( NixRebuildUpdateMode
                                                  ( NixRebuildUpdateUpdate
                                                  , NixRebuildUpdateRollback
                                                  )
                                                )


nixosRebuild :: NixRebuildMode -> NixRebuildUpdateMode -> Expr
nixosRebuild mode updateMode = Command
  "nixos-rebuild"
  (  [LiteralArg (showText mode)]
  <> mwhen (updateMode == NixRebuildUpdateUpdate)   ["--upgrade"]
  <> mwhen (updateMode == NixRebuildUpdateRollback) ["--rollback"]
  )

installExpr :: NixRebuildMode -> NixRebuildUpdateMode -> IO Expr
installExpr rebuildMode updateMode = do
  localPackagesFile <- locateLocalPackagesFileMaybeCreate
  rootPackagesFile  <- locateRootPackagesFile
  localServicesFile <- locateLocalServicesFileMaybeCreate
  rootServicesFile  <- locateRootServicesFile
  let copyToOld fn = cp fn (fn -<.> "old")
      moveFromOld fn = mv (fn -<.> "old") fn
  pure
    $     mkdir True [rootManagerPath]
    `And` copyToOld rootServicesFile
    `And` copyToOld rootPackagesFile
    `And` cp localPackagesFile rootPackagesFile
    `And` cp localServicesFile rootServicesFile
    `And` nixosRebuild rebuildMode updateMode
    `Or`  Subshell
            (moveFromOld rootServicesFile `And` moveFromOld localServicesFile)

sudoExpr :: Expr -> Expr
sudoExpr e = Command
  "sudo"
  ["-H", "-S", "-u", "root", "--", "sh", "-c", LiteralArg (evalExpr e)]

askPassExpr :: Text -> Expr
askPassExpr description =
  Command "gksudo" ["--description", LiteralArg description, "--print-pass"]

askPass :: IO ProcessData
askPass = runProcess noStdin (askPassExpr "NixOS system rebuild")

rebuild :: NixRebuildMode -> NixRebuildUpdateMode -> Text -> IO ProcessData
rebuild rebuildMode updateMode password =
  installExpr rebuildMode updateMode
    >>= runProcess (Just (encodeUtf8 password))
    .   sudoExpr

