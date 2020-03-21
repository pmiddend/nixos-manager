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
  , ServiceStateDownloadingData(ServiceStateDownloadingData)
  )
where

import           Control.Lens                   ( makePrisms
                                                , makeLenses
                                                )
import           NixManager.ServiceStateData    ( ServiceStateData )
import           NixManager.ServiceDownload     ( ServiceDownloadState )
import           Data.Text                      ( Text )

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
