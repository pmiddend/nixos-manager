{-|
  Description: Contains "ProgramArguments" for the manager plus a parser for that
Contains "ProgramArguments" for the manager plus a parser for that
  -}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module NixManager.ProgramArguments
  ( ProgramArguments
  , parseArguments
  )
where

import           System.Environment             ( getArgs )
import           GHC.Generics                   ( Generic )

newtype ProgramArguments = ProgramArguments {
  useHomeManager :: Bool
  } deriving(Generic)

parseArguments :: IO ProgramArguments
parseArguments = getArgs >>= \case
  ("--home-manager" : _) -> pure (ProgramArguments True)
  _                      -> pure (ProgramArguments False)
