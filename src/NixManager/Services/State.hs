{-|
  Description: Contains all the state for the Services tab
  -}
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
                                                , readLocalServiceFile
                                                )
import           NixManager.Util                ( TextualError )

-- | This contains the all data for the state “we’re currently downloading the services file”
data StateDownloadingData = StateDownloadingData {
    _sddCounter :: Int  -- ^ This field is necessary to “pulse” the GTK progress bar while building, see "NixManager.View.ProgressBar" for details
  , _sddVar :: DownloadState -- ^ The actual download state
  }

makeLenses ''StateDownloadingData

data State = StateInvalidOptions (Maybe Text) -- ^ Parsing the service options file failed for some reason
           | StateInvalidExpr Text -- ^ Parsing the services Nix expression failed for some reason
           | StateDownloading StateDownloadingData -- ^ We’re currently downloading the options file
           | StateDone StateData -- ^ We have a valid options file

makePrisms ''State

-- FIXME: Better happy path
-- | The initial Services tab state (needs to read the options file changes, hence the side-effect)
initState :: IO State
initState = do
  optionsFile' <- locateOptionsFile
  case optionsFile' of
    Nothing          -> pure (StateInvalidOptions Nothing)
    Just optionsFile -> do
      options' <- readOptionsFile optionsFile
      case options' of
        Left  e       -> pure (StateInvalidOptions (Just e))
        Right options -> do
          services' <- readLocalServiceFile
          case services' of
            Left e -> pure (StateInvalidExpr e)
            Right services ->
              pure $ StateDone
                (StateData (makeServices options) Nothing services mempty 0)
