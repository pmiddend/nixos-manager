{-# LANGUAGE DeriveFunctor #-}
module NixManager.Util where

import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.List                      ( unfoldr )
import           Prelude                 hiding ( putStrLn )

data MaybeError e = Error Text
                  | Success e
                  deriving(Functor)

fromEither :: Either String e -> MaybeError e
fromEither (Left  e) = Error (pack e)
fromEither (Right v) = Success v

instance Applicative MaybeError where
  pure = Success
  (Error   v) <*> _           = Error v
  (Success _) <*> (Error   v) = Error v
  (Success f) <*> (Success v) = Success (f v)

instance Monad MaybeError where
  (Error   e) >>= _ = Error e
  (Success v) >>= f = f v

ifNothing v f = case v of
  Nothing -> f
  _       -> mempty

ifSuccessIO v f = do
  v' <- v
  ifSuccess v' f

addToError :: Text -> Endo (MaybeError a)
addToError prefix (Error t) = Error (prefix <> t)
addToError _      v         = v

ifSuccess
  :: Applicative f
  => MaybeError t
  -> (t -> f (MaybeError a))
  -> f (MaybeError a)
ifSuccess v f = case v of
  Error   e  -> pure (Error e)
  Success v' -> f v'
-- errorMessageFromString :: String -> ErrorMessage
-- errorMessageFromString = ErrorMessage . pack

-- printError :: ErrorMessage -> IO ()
-- printError (ErrorMessage e) = putStrLn e

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


