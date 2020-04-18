{-# LANGUAGE OverloadedStrings #-}
{-|
  Description: Functions that wrap @nix search@
Functions that wrap @nix search@
  -}
module NixManager.NixPackageSearch
  ( searchPackages
  )
where

import           Data.Text                      ( Text )
import           NixManager.Bash                ( Expr(Command)
                                                , Arg(LiteralArg)
                                                )
import           System.Exit                    ( ExitCode
                                                  ( ExitSuccess
                                                  , ExitFailure
                                                  )
                                                )
import           NixManager.Process             ( runProcessToFinish
                                                , poStdout
                                                , poStderr
                                                , poResult
                                                )
import           NixManager.Util                ( decodeUtf8
                                                , TextualError
                                                , fromStrictBS
                                                , addToError
                                                , showText
                                                )
import           Control.Lens                   ( (^?!)
                                                , (^.)
                                                , to
                                                , folded
                                                )
import           NixManager.NixPackage          ( NixPackage
                                                , readPackagesJson
                                                )
import           Data.Monoid                    ( First(getFirst) )

-- | Expression to call @nix search --json <search-term>@
nixSearchExpr :: Text -> Expr
nixSearchExpr term = Command "nix" ["search", LiteralArg term, "--json"]

-- | Call @nix search@ with a search parameter, return the parsed result
searchPackages :: Text -> IO (TextualError [NixPackage])
searchPackages t = do
  po <- runProcessToFinish Nothing (nixSearchExpr t)
  let
    processedResult = addToError
      "Error parsing output of \"nix search\" command. This could be due to changes in this command in a later version (and doesn't fix itself). Please open an issue in the nixos-manager repository. The error was: "
      (readPackagesJson (po ^. poStdout . fromStrictBS))
  case po ^?! poResult . to getFirst . folded of
    ExitSuccess      -> pure processedResult
    ExitFailure 1    -> pure processedResult
    ExitFailure code -> pure
      (Left
        (  "Error executing \"nix search\" command (exit code "
        <> showText code
        <> "): standard error output: "
        <> (po ^. poStderr . decodeUtf8)
        )
      )


