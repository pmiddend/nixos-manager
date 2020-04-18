{-|
  Description: Contains "ProgramArguments" for the manager plus a parser for that
Contains "ProgramArguments" for the manager plus a parser for that
  -}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module NixManager.ProgramArguments
  ( ProgramArguments
  , paUseHomeManager
  , parseArguments
  )
where

import           Control.Lens                   ( makeLenses )
import           System.Environment             ( getArgs )

newtype ProgramArguments = ProgramArguments {
  _paUseHomeManager :: Bool
  }

makeLenses ''ProgramArguments

parseArguments :: IO ProgramArguments
parseArguments = getArgs >>= \case
  ("--home-manager" : _) -> pure (ProgramArguments True)
  _                      -> pure (ProgramArguments False)
