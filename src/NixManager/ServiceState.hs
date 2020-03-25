{-# LANGUAGE TemplateHaskell #-}
module NixManager.ServiceState
  ( ServiceState
    ( ServiceStateInvalidExpr
    , ServiceStateDownloading
    , ServiceStateDone
    , ServiceStateInvalidOptions
    )
  , _ServiceStateInvalidExpr
  , _ServiceStateInvalidOptions
  , _ServiceStateDownloading
  , _ServiceStateDone
  , ssddCounter
  , ssddVar
  , initServiceState
  , ServiceStateDownloadingData(ServiceStateDownloadingData)
  )
where

import           Control.Lens                   ( makePrisms
                                                , makeLenses
                                                )
import           NixManager.ServiceStateData    ( ServiceStateData
                                                  ( ServiceStateData
                                                  )
                                                )
import           NixManager.ServiceDownload     ( ServiceDownloadState )
import           Data.Text                      ( Text )
import           NixManager.NixServiceOption    ( readOptionsFile
                                                , locateOptionsFile
                                                )
import           NixManager.NixService          ( makeServices
                                                , readServices
                                                )
import           NixManager.Util                ( MaybeError(Success, Error) )

data ServiceStateDownloadingData = ServiceStateDownloadingData {
    _ssddCounter :: Int
  , _ssddVar :: ServiceDownloadState
  }

makeLenses ''ServiceStateDownloadingData

data ServiceState = ServiceStateInvalidOptions (Maybe Text)
                  | ServiceStateInvalidExpr Text
                  | ServiceStateDownloading ServiceStateDownloadingData
                  | ServiceStateDone ServiceStateData

makePrisms ''ServiceState

-- FIXME: Better happy path
initServiceState :: IO ServiceState
initServiceState = do
  optionsFile' <- locateOptionsFile
  case optionsFile' of
    Nothing          -> pure (ServiceStateInvalidOptions Nothing)
    Just optionsFile -> do
      options' <- readOptionsFile optionsFile
      case options' of
        Error   e       -> pure (ServiceStateInvalidOptions (Just e))
        Success options -> do
          services' <- readServices
          case services' of
            Error   e        -> pure (ServiceStateInvalidExpr e)
            Success services -> pure $ ServiceStateDone
              (ServiceStateData (makeServices options) Nothing services)
