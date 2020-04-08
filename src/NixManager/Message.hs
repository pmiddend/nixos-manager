{-|
  Description: Type for messages to be displayed in the GUI (errors, infos)
  -}
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

-- | Type of the message (determines the icon and/or background color)
data MessageType = ErrorMessage
                 | InfoMessage
                 deriving(Eq,Show)

makePrisms ''MessageType

-- | A message to be displayed in the GUI
data Message = Message {
    _messageType :: MessageType
  , _messageText :: Text
  }
  deriving(Eq,Show)

makeLenses ''Message

-- | Construct an error message
errorMessage :: Text -> Message
errorMessage = Message ErrorMessage

-- | Construct an info message
infoMessage :: Text -> Message
infoMessage = Message InfoMessage
