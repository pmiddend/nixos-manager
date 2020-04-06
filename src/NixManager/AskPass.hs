{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
module NixManager.AskPass
  ( sudoExpr
  , askPassExpr
  , askPass
  )
where

import           Data.Text                      ( Text )
import           NixManager.Bash                ( Expr(Command)
                                                , Arg(LiteralArg)
                                                , evalExpr
                                                )
import           Prelude                 hiding ( readFile )
import           NixManager.Process             ( runProcess
                                                , noStdin
                                                , ProcessData
                                                )


sudoExpr :: Expr -> Expr
sudoExpr e = Command
  "sudo"
  ["-H", "-S", "-u", "root", "--", "sh", "-c", LiteralArg (evalExpr e)]

askPassExpr :: Text -> Expr
askPassExpr description =
  Command "gksudo" ["--description", LiteralArg description, "--print-pass"]

askPass :: IO ProcessData
askPass = runProcess noStdin (askPassExpr "NixOS Manager")



