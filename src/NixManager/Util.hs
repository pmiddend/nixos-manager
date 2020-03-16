module NixManager.Util where

import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Text.IO                   ( putStrLn )
import           Data.List                      ( unfoldr )
import           Prelude                 hiding ( putStrLn )

newtype ErrorMessage = ErrorMessage {
  _getErrorMessage :: Text
  }

errorMessageFromString :: String -> ErrorMessage
errorMessageFromString = ErrorMessage . pack

printError :: ErrorMessage -> IO ()
printError (ErrorMessage e) = putStrLn e

showText :: Show a => a -> Text
showText = pack . show

mwhen :: Monoid m => Bool -> m -> m
mwhen True  v = v
mwhen False _ = mempty

type Endo a = a -> a

splitRepeat :: Char -> String -> [String]
splitRepeat c = unfoldr f
 where
  f :: String -> Maybe (String, String)
  f "" = Nothing
  f x  = case span (/= c) x of
    (before, []       ) -> Just (before, "")
    (before, _ : after) -> Just (before, after)


