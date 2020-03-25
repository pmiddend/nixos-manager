{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
module NixManager.Rebuild
  ( askPass
  , rebuild
  )
where

import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( encodeUtf8 )
import           NixManager.BashDsl             ( mkdir
                                                , BashExpr(BashAnd, BashCommand)
                                                , BashArg
                                                  ( BashRawArg
                                                  , BashLiteralArg
                                                  )
                                                , evalBashExpr
                                                , cp
                                                , nixosRebuild
                                                , RebuildMode(RebuildBuild)
                                                )
import           Prelude                 hiding ( readFile )
import           NixManager.PackageSearch       ( locatePackagesFileMaybeCreate
                                                )
import           NixManager.NixService          ( locateServicesFileMaybeCreate
                                                )
import           NixManager.Process             ( runProcess
                                                , noStdin
                                                , ProcessData
                                                )

rootManagerPath :: FilePath
rootManagerPath = "/etc/nixos/nixos-manager"

installExpr :: IO BashExpr
installExpr = do
  packagesFile <- locatePackagesFileMaybeCreate
  servicesFile <- locateServicesFileMaybeCreate
  pure
    $         mkdir True [rootManagerPath]
    `BashAnd` cp packagesFile rootManagerPath
    `BashAnd` cp servicesFile rootManagerPath
    `BashAnd` nixosRebuild RebuildBuild

sudoExpr :: BashExpr -> BashExpr
sudoExpr e = BashCommand
  "sudo"
  ["-H", "-S", "-u", "root", "--", "sh", "-c", BashLiteralArg (evalBashExpr e)]

askPassExpr :: Text -> BashExpr
askPassExpr description = BashCommand
  "gksudo"
  ["--description", BashLiteralArg description, "--print-pass"]

askPass :: IO ProcessData
askPass = runProcess noStdin (askPassExpr "NixOS system rebuild")

rebuild :: Text -> IO ProcessData
rebuild password =
  installExpr >>= runProcess (Just (encodeUtf8 password)) . sudoExpr
