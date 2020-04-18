{-# LANGUAGE OverloadedStrings #-}
{-|
  Description: Contains functions related to collecting garbage as a user (e.g. “with home-manager”)
Contains functions related to collecting garbage as a user (e.g. “with home-manager”)
 -}
module NixManager.HMGarbage
  ( collectGarbage
  )
where

import           NixManager.Bash                ( Expr(Command) )
import           NixManager.Process             ( runProcess
                                                , noStdin
                                                , ProcessData
                                                )

-- | The bash expression for @nix-collect-garbage@
collectGarbageExpr :: Expr
collectGarbageExpr = Command "nix-collect-garbage" mempty

-- | Start the garbage collection
collectGarbage :: IO ProcessData
collectGarbage = runProcess noStdin collectGarbageExpr
