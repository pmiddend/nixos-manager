{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.HMServices.State
  ( State(..)
  , initState
  , _HomeManagerPresent
  )
where

import           NixManager.Services.StateData  ( StateData(StateData) )
import           NixManager.NixServiceOption    ( readOptionsFile )
import           NixManager.NixService          ( makeServices )
import           NixManager.HMServicesUtil      ( readPendingServicesFile
                                                , locateOptionsFile
                                                )
import           Data.Text                      ( Text )
import           NixManager.Util                ( showText )
import           Control.Lens                   ( makePrisms )

data State = NoHomeManager
           | InvalidHomeManager Text
           | HomeManagerPresent StateData

makePrisms ''State

initState :: IO State
initState = locateOptionsFile >>= \case
  Nothing              -> pure NoHomeManager
  Just optionsFileName -> readOptionsFile optionsFileName >>= \case
    Left e -> pure
      (InvalidHomeManager
        ("Your local options JSON file is corrupted. Please fix it, or delete it and run “home-manager switch” again. It’s stored in\n\n<tt>"
        <> showText optionsFileName
        <> "</tt>. The error is:\n\n<tt>"
        <> e
        <> "</tt>"
        )
      )
    Right options -> readPendingServicesFile >>= \case
      Left e -> pure
        (InvalidHomeManager
          ("Your local service Nix configuration is corrupted. Please fix it. The error is: <tt>"
          <> e
          <> "</tt>"
          )
        )
      Right services -> pure
        (HomeManagerPresent
          (StateData (makeServices options) Nothing services mempty 0)
        )
