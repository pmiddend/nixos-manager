{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
module NixManager.NixRebuild
  ( askPass
  , rebuild
  , rootManagerPath
  )
where

import           NixManager.Constants           ( rootManagerPath )
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( encodeUtf8 )
import           NixManager.Util                ( showText )
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

nixosRebuild :: NixRebuildMode -> Expr
nixosRebuild mode = Command "nixos-rebuild" [LiteralArg (showText mode)]

installExpr :: NixRebuildMode -> IO Expr
installExpr rebuildMode = do
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
    `And` nixosRebuild rebuildMode
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

rebuild :: NixRebuildMode -> Text -> IO ProcessData
rebuild rebuildMode password =
  installExpr rebuildMode >>= runProcess (Just (encodeUtf8 password)) . sudoExpr

