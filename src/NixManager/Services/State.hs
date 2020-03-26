{-# LANGUAGE TemplateHaskell #-}
module NixManager.Services.State
  ( State(StateInvalidExpr, StateDownloading, StateDone, StateInvalidOptions)
  , _StateInvalidExpr
  , _StateInvalidOptions
  , _StateDownloading
  , _StateDone
  , sddCounter
  , sddVar
  , initState
  , StateDownloadingData(StateDownloadingData)
  )
where

import           Control.Lens                   ( makePrisms
                                                , makeLenses
                                                )
import           NixManager.Services.StateData  ( StateData(StateData) )
import           NixManager.Services.Download   ( DownloadState )
import           Data.Text                      ( Text )
import           NixManager.NixServiceOption    ( readOptionsFile
                                                , locateOptionsFile
                                                )
import           NixManager.NixService          ( makeServices
                                                , readServices
                                                )
import           NixManager.Util                ( MaybeError(Success, Error) )

data StateDownloadingData = StateDownloadingData {
    _sddCounter :: Int
  , _sddVar :: DownloadState
  }

makeLenses ''StateDownloadingData

data State = StateInvalidOptions (Maybe Text)
           | StateInvalidExpr Text
           | StateDownloading StateDownloadingData
           | StateDone StateData

makePrisms ''State

-- FIXME: Better happy path
initState :: IO State
initState = do
  optionsFile' <- locateOptionsFile
  case optionsFile' of
    Nothing          -> pure (StateInvalidOptions Nothing)
    Just optionsFile -> do
      options' <- readOptionsFile optionsFile
      case options' of
        Error   e       -> pure (StateInvalidOptions (Just e))
        Success options -> do
          services' <- readServices
          case services' of
            Error   e        -> pure (StateInvalidExpr e)
            Success services -> pure
              $ StateDone (StateData (makeServices options) Nothing services)
