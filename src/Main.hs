{-|
Description: NixOS manager's entry point
-}
module Main where

import NixManager.ManagerMain as NixMain

main :: IO ()
main = NixMain.nixMain
