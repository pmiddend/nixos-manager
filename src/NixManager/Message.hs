{-|
  Description: Type for messages to be displayed in the GUI (errors, infos)
Type for messages to be displayed in the GUI (errors, infos)
  -}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
module NixManager.Message
  ( MessageType
  , Message
  , messageType
  , messageText
  , messageWidget
  , errorMessage
  , infoMessage
  , _ErrorMessage
  , _InfoMessage
  )
where

import           Data.Text                      ( Text )
import           Control.Lens                   ( makeLenses
                                                , makePrisms
                                                , has
                                                , (^.)
                                                )
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative             ( widget
                                                , classes
                                                , Attribute((:=))
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

-- | Create a nice-looking widget corresponding to the message given
messageWidget e = widget
  Gtk.Label
  [ #label := (e ^. messageText)
  , #useMarkup := True
  , classes
    [ if has (messageType . _ErrorMessage) e
        then "error-message"
        else "info-message"
    ]
  ]

