{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.Util where

import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Lazy          as BSL
import           Control.Concurrent             ( threadDelay )
import           Data.String                    ( IsString )
import           Data.Bifunctor                 ( first )
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Encoding       as TLE
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Data.List                      ( unfoldr )
import           Prelude                 hiding ( putStrLn )
import           Control.Lens                   ( Getter
                                                , to
                                                )
import qualified Data.Text.Encoding            as Encoding

data MaybeError e = Error Text
                  | Success e
                  deriving(Functor, Foldable)

instance Show e => Show (MaybeError e) where
  show (Error   e) = "error: " <> unpack e
  show (Success e) = "success: " <> show e

fromShowableError :: Show ex => Either ex e -> MaybeError e
fromShowableError = fromEither . first show


fromEither :: Either String e -> MaybeError e
fromEither (Left  e) = Error (pack e)
fromEither (Right v) = Success v

toEither :: MaybeError e -> Either Text e
toEither (Error   e) = Left e
toEither (Success e) = Right e

errorFallback :: e -> MaybeError e -> e
errorFallback v (Error   _) = v
errorFallback _ (Success v) = v

instance Applicative MaybeError where
  pure = Success
  (Error   v) <*> _           = Error v
  (Success _) <*> (Error   v) = Error v
  (Success f) <*> (Success v) = Success (f v)

instance Monad MaybeError where
  (Error   e) >>= _ = Error e
  (Success v) >>= f = f v

ifNothing :: Monoid p => Maybe a -> p -> p
ifNothing v f = case v of
  Nothing -> f
  _       -> mempty

ifSuccessIO
  :: Monad m => m (MaybeError t) -> (t -> m (MaybeError a)) -> m (MaybeError a)
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


predAnd :: (t -> Bool) -> (t -> Bool) -> t -> Bool
predAnd a b x = a x && b x

openTag :: (IsString s, Semigroup s) => s -> s
openTag t = "<" <> t <> ">"
closeTag :: (IsString s, Semigroup s) => s -> s
closeTag t = "</" <> t <> ">"

surroundSimple :: (IsString s, Semigroup s) => s -> s -> s
surroundSimple tag content = openTag tag <> content <> closeTag tag

threadDelayMillis :: Int -> IO ()
threadDelayMillis = threadDelay . (* 1000)

decodeUtf8 :: Getter ByteString Text
decodeUtf8 = to Encoding.decodeUtf8

encodeUtf8 :: Getter Text ByteString
encodeUtf8 = to Encoding.encodeUtf8

decodeUtf8Lazy :: Getter BSL.ByteString Text
decodeUtf8Lazy = to (TL.toStrict . TLE.decodeUtf8)

encodeUtf8Lazy :: Getter Text BSL.ByteString
encodeUtf8Lazy = to (TLE.encodeUtf8 . TL.fromStrict)

fromStrictBS :: Getter ByteString BSL.ByteString
fromStrictBS = to BSL.fromStrict
