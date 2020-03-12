{-# LANGUAGE TemplateHaskell #-}
module NixManager.ManagerEvent where

import           Data.Text                      ( Text )
import           Control.Lens                   ( makePrisms )


data ManagerEvent = ManagerEventClosed
      | ManagerEventSearchChanged Text
      | ManagerEventPackageSelected (Maybe Int)
      | ManagerEventTryInstall
      | ManagerEventTryInstallFinished [FilePath]
      deriving(Eq,Show)

makePrisms ''ManagerEvent
