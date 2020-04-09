{-|
Description: NixOS manager's entry point
-}
module Main where

import NixManager.ManagerMain as NixMain

-- | Please move along to "NixManager.ManagerMain"
main :: IO ()
main = NixMain.nixMain
