{-# LANGUAGE OverloadedStrings #-}
module NixManager.BashDsl
  ( Expr(..)
  , Arg(..)
  , evalExpr
  , mkdir
  , cp
  , mv
  , appendArgs
  , devNullify
  , nixSearch
  )
where

import           Data.List.NonEmpty             ( NonEmpty )
import           Data.Foldable                  ( toList )
import           NixManager.Util                ( mwhen )
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


data Arg = LiteralArg Text
             | RawArg Text

instance IsString Arg where
  fromString = LiteralArg . pack

data Expr = Command Text [Arg]
          | And Expr Expr
          | Or Expr Expr
          | Then Expr Expr
          | Subshell Expr

surround :: Text -> Text -> Text
surround c e = c <> e <> c

escape :: Text -> Text
escape = replace "\"" "\\\""

specialChar :: Text -> Bool
specialChar t = " " `isInfixOf` t || "<" `isInfixOf` t || ">" `isInfixOf` t

maybeSurround :: Arg -> Text
maybeSurround (RawArg t) = t
maybeSurround (LiteralArg t) | specialChar t = surround "\"" (escape t)
                             | otherwise     = escape t

evalExpr :: Expr -> Text
evalExpr (And     l r     ) = evalExpr l <> " && " <> evalExpr r
evalExpr (Then    l r     ) = evalExpr l <> " ; " <> evalExpr r
evalExpr (Or      l r     ) = evalExpr l <> " || " <> evalExpr r
evalExpr (Command c args  ) = c <> " " <> unwords (maybeSurround <$> args)
evalExpr (Subshell subExpr) = "(" <> evalExpr subExpr <> ")"

mkdir :: Bool -> NonEmpty FilePath -> Expr
mkdir recursive paths = Command
  "mkdir"
  (mwhen recursive ["-p"] <> toList (LiteralArg . pack <$> paths))

cp :: FilePath -> FilePath -> Expr
cp from to = Command "cp" (LiteralArg <$> [pack from, pack to])

mv :: FilePath -> FilePath -> Expr
mv from to = Command "mv" (LiteralArg <$> [pack from, pack to])

nixSearch :: Text -> Expr
nixSearch term = Command "nix" ["search", LiteralArg term, "--json"]

appendArgs :: [Arg] -> Expr -> Expr
appendArgs newArgs (Command t args) = Command t (args <> newArgs)
appendArgs newArgs (And l r) = And (appendArgs newArgs l) (appendArgs newArgs r)
appendArgs newArgs (Or l r) = Or (appendArgs newArgs l) (appendArgs newArgs r)
appendArgs newArgs (Then l r) = Then (appendArgs newArgs l) (appendArgs newArgs r)
appendArgs newArgs (Subshell e) = Subshell (appendArgs newArgs e)

devNullify :: Expr -> Expr
devNullify = appendArgs [RawArg ">/dev/null", RawArg "2>&1"]
