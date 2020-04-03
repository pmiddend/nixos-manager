module NixManager.Password
  ( Password(..)
  )
where

import           Data.Text                      ( Text )

newtype Password = Password {
  getPassword :: Text
  }
