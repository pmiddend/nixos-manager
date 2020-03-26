{-# LANGUAGE OverloadedStrings #-}
module NixManager.BashDsl
  ( BashExpr(..)
  , BashArg(..)
  , evalBashExpr
  , mkdir
  , cp
  , nixSearch
  , nixosRebuild
  , RebuildMode(..)
  )
where

import           Data.List.NonEmpty             ( NonEmpty )
import           Data.Foldable                  ( toList )
import           NixManager.Util                ( mwhen
                                                , showText
                                                )
import           Data.Text                      ( Text
                                                , unwords
                                                , replace
                                                , isInfixOf
                                                , pack
                                                )
import           Data.String                    ( IsString
                                                , fromString
                                                )
import           Prelude                 hiding ( unwords
                                                , elem
                                                )


data BashArg = BashLiteralArg Text
             | BashRawArg Text

instance IsString BashArg where
  fromString = BashLiteralArg . pack

data BashExpr = BashCommand Text [BashArg]
              | BashAnd BashExpr BashExpr

surround :: Text -> Text -> Text
surround c e = c <> e <> c

escape :: Text -> Text
escape = replace "\"" "\\\""

bashSpecial :: Text -> Bool
bashSpecial t = " " `isInfixOf` t || "<" `isInfixOf` t || ">" `isInfixOf` t

maybeSurround :: BashArg -> Text
maybeSurround (BashRawArg t) = t
maybeSurround (BashLiteralArg t) | bashSpecial t = surround "\"" (escape t)
                                 | otherwise     = escape t

evalBashExpr :: BashExpr -> Text
evalBashExpr (BashAnd l r) = evalBashExpr l <> " && " <> evalBashExpr r
evalBashExpr (BashCommand c args) =
  c <> " " <> unwords (maybeSurround <$> args)

mkdir :: Bool -> NonEmpty FilePath -> BashExpr
mkdir recursive paths = BashCommand
  "mkdir"
  (mwhen recursive ["-p"] <> toList (BashLiteralArg . pack <$> paths))

cp :: FilePath -> FilePath -> BashExpr
cp from to = BashCommand "cp" (BashLiteralArg <$> [pack from, pack to])

nixSearch :: Text -> BashExpr
nixSearch term = BashCommand "nix" ["search", BashLiteralArg term, "--json"]

data RebuildMode = RebuildSwitch
 | RebuildBoot
 | RebuildTest
 | RebuildBuild
 | RebuildDryBuild
 | RebuildDryActivate
 | RebuildEdit
 | RebuildBuildVm
 | RebuildBuildVmWithBootloader

instance Show RebuildMode where
  show RebuildSwitch                = "switch"
  show RebuildBoot                  = "boot"
  show RebuildTest                  = "test"
  show RebuildBuild                 = "build"
  show RebuildDryBuild              = "dry-build"
  show RebuildDryActivate           = "dry-activate"
  show RebuildEdit                  = "edit"
  show RebuildBuildVm               = "build-vm"
  show RebuildBuildVmWithBootloader = "build-vm-with-booloader"

nixosRebuild :: RebuildMode -> BashExpr
nixosRebuild mode =
  BashCommand "nixos-rebuild" [BashLiteralArg (showText mode)]
