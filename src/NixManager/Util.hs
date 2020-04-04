{-# LANGUAGE OverloadedStrings #-}
module NixManager.Util where

import           Data.Char                      ( isUpper
                                                , toLower
                                                )
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
                                                , drop
                                                , length
                                                , replace
                                                , singleton
                                                , snoc
                                                , null
                                                , foldl
                                                )
import           Data.List                      ( unfoldr )
import           Prelude                 hiding ( putStrLn
                                                , foldl
                                                , null
                                                , drop
                                                , length
                                                )
import           Control.Lens                   ( Getter
                                                , to
                                                )
import qualified Data.Text.Encoding            as Encoding

type TextualError = Either Text

fromShowableError :: Show ex => Either ex e -> TextualError e
fromShowableError = fromEither . first show

fromEither :: Either String e -> TextualError e
fromEither = first pack

toEither :: TextualError e -> Either Text e
toEither = id

errorFallback :: e -> TextualError e -> e
errorFallback v (Left   _) = v
errorFallback _ (Right v) = v

ifNothing :: Monoid p => Maybe a -> p -> p
ifNothing v f = case v of
  Nothing -> f
  _       -> mempty

ifSuccessIO
  :: Monad m => m (TextualError t) -> (t -> m (TextualError a)) -> m (TextualError a)
ifSuccessIO v f = v >>= either (pure . Left) f

addToError :: Text -> Endo (TextualError a)
addToError prefix = first (prefix <>)

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

kebapize :: Text -> Text -> Text
kebapize prefix =
  foldl
      (\prior c -> if null prior
        then singleton (toLower c)
        else if isUpper c then prior <> snoc "-" (toLower c) else snoc prior c
      )
      mempty
    . drop (length prefix)

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

replaceHtmlEntities :: Text -> Text
replaceHtmlEntities =
  replace "<" "&lt;" . replace ">" "&gt;" . replace "&" "&amp;"

filesEqual :: FilePath -> FilePath -> IO Bool
filesEqual a b = (==) <$> BSL.readFile a <*> BSL.readFile b
