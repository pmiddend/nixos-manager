{-# LANGUAGE OverloadedStrings #-}
module NixManager.PosixTools(mkdir, cp, mv, kill) where

import System.Process(Pid)
import           Data.Foldable                  ( toList )
import           NixManager.Util                ( mwhen, showText )
import NixManager.Bash(Expr(Command), Arg(LiteralArg, RawArg))
import           Data.Text                      ( Text
                                                , unwords
                                                , replace
                                                , isInfixOf
                                                , pack
                                                )
import Data.List.NonEmpty(NonEmpty)

mkdir :: Bool -> NonEmpty FilePath -> Expr
mkdir recursive paths = Command
  "mkdir"
  (mwhen recursive ["-p"] <> toList (LiteralArg . pack <$> paths))

cp :: FilePath -> FilePath -> Expr
cp from to = Command "cp" (LiteralArg <$> [pack from, pack to])

mv :: FilePath -> FilePath -> Expr
mv from to = Command "mv" (LiteralArg <$> [pack from, pack to])

kill :: Pid -> Expr
kill pid = Command "kill" ["-9", RawArg (showText pid)]
