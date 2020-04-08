{-|
Description: Random utilities for NixOS Manager

As a general rule, stuff defined here should not import anything from the manager itself.
-}
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
                                                , Iso'
                                                , iso
                                                , to
                                                )
import qualified Data.Text.Encoding            as Encoding

-- | Since we’re working with 'Text' as much as possible, we’re using a text based error type instead of the customary 'Either String'
type TextualError = Either Text

-- | Convert something showable to 'Text'. Notably 'String' and 'Exception' types.
fromShowableError :: Show ex => Either ex e -> TextualError e
fromShowableError = first showText

-- | Convert from a'String' error to 'Text'
fromEither :: Either String e -> TextualError e
fromEither = first pack

-- | More easily chain errors in IO computations
ifSuccessIO
  :: Monad m
  => m (TextualError t)
  -> (t -> m (TextualError a))
  -> m (TextualError a)
ifSuccessIO v f = v >>= either (pure . Left) f

-- | Add some (descriptive) prefix text to an error.
addToError :: Text -> Endo (TextualError a)
addToError prefix = first (prefix <>)

-- | Like 'Show' from the prelude, but produces a 'Text'
showText :: Show a => a -> Text
showText = pack . show

-- | Return a monoidal value if true, otherwise return 'mempty’
mwhen :: Monoid m => Bool -> m -> m
mwhen True  v = v
mwhen False _ = mempty

-- | A type representing an endomorphisms of types. This sometimes makes transformations more explicit.
type Endo a = a -> a

-- | Like 'Text.splitOn', but with 'String'
splitRepeat :: Char -> String -> [String]
splitRepeat c = unfoldr f
 where
  f :: String -> Maybe (String, String)
  f "" = Nothing
  f x  = case span (/= c) x of
    (before, []       ) -> Just (before, "")
    (before, _ : after) -> Just (before, after)

-- | '&&' for predicates
predAnd :: (t -> Bool) -> (t -> Bool) -> t -> Bool
predAnd a b x = a x && b x

-- | Create an opening HTML tag
openTag :: (IsString s, Semigroup s) => s -> s
openTag t = "<" <> t <> ">"

-- | Create a closing HTML tag
closeTag :: (IsString s, Semigroup s) => s -> s
closeTag t = "</" <> t <> ">"

-- | “Kebapize” a name, transforming e.g. “FooBarBaz” to “foo-bar-baz”
kebapize :: Text -> Text -> Text
kebapize prefix =
  foldl
      (\prior c -> if null prior
        then singleton (toLower c)
        else if isUpper c then prior <> snoc "-" (toLower c) else snoc prior c
      )
      mempty
    . drop (length prefix)

-- | Surround a text by something constant
surround :: Text -> Text -> Text
surround c e = c <> e <> c

-- | Surround a string by an HTML tag (without attributes)
surroundSimple :: (IsString s, Semigroup s) => s -> s -> s
surroundSimple tag content = openTag tag <> content <> closeTag tag

-- | The standard library’s 'threadDelay' takes a plain 'Int', which confuses me terribly.
threadDelayMillis :: Int -> IO ()
threadDelayMillis = threadDelay . (* 1000)

-- | Convert between a strict UTF-8 'ByteString' and 'Text'
decodeUtf8 :: Getter ByteString Text
decodeUtf8 = to Encoding.decodeUtf8

-- | Convert between a strict UTF-8 'ByteString' and 'Text'
encodeUtf8 :: Getter Text ByteString
encodeUtf8 = to Encoding.encodeUtf8

-- | Convert between a lazy UTF-8 'ByteString' and 'Text'
decodeUtf8Lazy :: Getter BSL.ByteString Text
decodeUtf8Lazy = to (TL.toStrict . TLE.decodeUtf8)

-- | Convert between a lazy UTF-8 'ByteString' and 'Text'
encodeUtf8Lazy :: Getter Text BSL.ByteString
encodeUtf8Lazy = to (TLE.encodeUtf8 . TL.fromStrict)

-- | Convert between a lazy 'ByteString' and a strict one
fromStrictBS :: Iso' ByteString BSL.ByteString
fromStrictBS = iso BSL.fromStrict BSL.toStrict

-- | Stupid function to convert HTML (or XML?) entities. Probably buggy as hell, but worked for now.
replaceHtmlEntities :: Text -> Text
replaceHtmlEntities =
  replace "<" "&lt;" . replace ">" "&gt;" . replace "&" "&amp;"

-- | Check if two files are bitwise-equal. Doesn’t consider memory a problem.
filesEqual :: FilePath -> FilePath -> IO Bool
filesEqual a b = (==) <$> BSL.readFile a <*> BSL.readFile b
