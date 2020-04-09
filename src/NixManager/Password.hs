{-|
  Description: Provides the 'Password' type to wrap over 'Text'
  -}
module NixManager.Password
  ( Password(..)
  )
where

import           Data.Text                      ( Text )

-- | Wrapper for passwords (used for the @sudo@ prompt stuff)
newtype Password = Password {
  getPassword :: Text
  }
