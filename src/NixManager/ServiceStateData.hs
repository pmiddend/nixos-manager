{-# LANGUAGE TemplateHaskell #-}
module NixManager.ServiceStateData
  ( ServiceStateData(ServiceStateData)
  , ssdServiceCache
  , ssdSelectedServiceIdx
  , ssdServiceExpression
  )
where

import           NixManager.NixService          ( NixService )
import           NixManager.NixExpr             ( NixExpr )
import           Control.Lens                   ( makeLenses )

data ServiceStateData = ServiceStateData {
    _ssdServiceCache :: [NixService]
  , _ssdSelectedServiceIdx :: Maybe Int
  , _ssdServiceExpression :: NixExpr
  }

makeLenses ''ServiceStateData
