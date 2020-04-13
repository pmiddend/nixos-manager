{-|
  Description: A little DSL to construct valid Bash expressions that can then be passed to "NixManager.Process"
  -}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.Bash
  ( Expr(..)
  , Arg(..)
  , evalExpr
  , appendArgs
  , devNullify
  , (&&.)
  , (||.)
  , (>>.)
  )
where

import           NixManager.Util                ( surround )
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


-- | The type of bash argument
data Arg = LiteralArg Text -- ^ A literal argument that will be surrounded by quotes and/or escaped if it contains special characters
         | RawArg Text     -- ^ A "raw" argument that is not subject to escaping/quoting.

instance IsString Arg where
  fromString = LiteralArg . pack

-- | A very small subset of a bash grammar.
data Expr = Command Text [Arg] -- ^ A command with some arguments
          | And Expr Expr      -- ^ Serializes to @a && b@
          | Or Expr Expr       -- ^ Serializes to @a || b@
          | Then Expr Expr     -- ^ Serializes to @a; b@
          | Subshell Expr      -- ^ Serializes to (expr)

-- | Alias for @&&@ (resembling the Bash expression)
(&&.) :: Expr -> Expr -> Expr
(&&.) = And

-- | Alias for @||@ (resembling the Bash expression)
(||.) :: Expr -> Expr -> Expr
(||.) = Or

-- | Alias for @Then@ (resembling the @>>@ monad operator)
(>>.) :: Expr -> Expr -> Expr
(>>.) = Then

-- | Escape a piece of text. Currently only supports escaping double quotes. More to come?
escape :: Text -> Text
escape = replace "\"" "\\\""

-- | Determine if the text contains characters that are special for bash.
specialChar :: Text -> Bool
specialChar t = " " `isInfixOf` t || "<" `isInfixOf` t || ">" `isInfixOf` t

-- | Convert an argument to a text, optionally escaping it.
maybeSurround :: Arg -> Text
maybeSurround (RawArg t) = t
maybeSurround (LiteralArg t) | specialChar t = surround "\"" (escape t)
                             | otherwise     = escape t

-- | Evaluate an expression to a bash string.
evalExpr :: Expr -> Text
evalExpr (And     l r     ) = evalExpr l <> " && " <> evalExpr r
evalExpr (Then    l r     ) = evalExpr l <> " ; " <> evalExpr r
evalExpr (Or      l r     ) = evalExpr l <> " || " <> evalExpr r
evalExpr (Command c args  ) = c <> " " <> unwords (maybeSurround <$> args)
evalExpr (Subshell subExpr) = "(" <> evalExpr subExpr <> ")"

-- | Recurse through the expression, adding arguments to all @Command@ constructors found.
appendArgs :: [Arg] -> Expr -> Expr
appendArgs newArgs (Command t args) = Command t (args <> newArgs)
appendArgs newArgs (And l r) =
  And (appendArgs newArgs l) (appendArgs newArgs r)
appendArgs newArgs (Or l r) = Or (appendArgs newArgs l) (appendArgs newArgs r)
appendArgs newArgs (Then l r) =
  Then (appendArgs newArgs l) (appendArgs newArgs r)
appendArgs newArgs (Subshell e) = Subshell (appendArgs newArgs e)

-- | Recurse through the expression, adding @>/dev/null@ and @2>&1@ to surpress any output of the command.
devNullify :: Expr -> Expr
devNullify = appendArgs [RawArg ">/dev/null", RawArg "2>&1"]
