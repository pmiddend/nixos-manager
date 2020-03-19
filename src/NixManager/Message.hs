{-# LANGUAGE TemplateHaskell #-}
module NixManager.Message
  ( MessageType
  , Message
  , messageType
  , messageText
  , errorMessage
  , infoMessage
  , _ErrorMessage
  , _InfoMessage
  )
where

import           Data.Text                      ( Text )
import           Control.Lens                   ( makeLenses
                                                , makePrisms
                                                )

data MessageType = ErrorMessage | InfoMessage deriving(Eq,Show)

makePrisms ''MessageType

data Message = Message { _messageType :: MessageType, _messageText :: Text  } deriving(Eq,Show)

makeLenses ''Message

errorMessage :: Text -> Message
errorMessage = Message ErrorMessage

infoMessage :: Text -> Message
infoMessage = Message InfoMessage
