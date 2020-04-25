{-|
  Description: Provides a type, as well as a parser for the option type contained in @options.json@
Provides a type, as well as a parser for the option type contained in @options.json@
  -}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.NixServiceOptionType
  ( NixServiceOptionType(..)
  , parseNixServiceOptionType
  )
where

import           Data.List                      ( intercalate )
import           Data.Bifunctor                 ( first )
import           Control.Monad                  ( void )
import           Data.Functor                   ( ($>) )
import           Data.Void                      ( Void )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import qualified Text.Megaparsec.Char.Lexer    as L
import           Text.Megaparsec.Char           ( char
                                                , string
                                                )
import           Text.Megaparsec                ( Parsec
                                                , manyTill
                                                , sepBy
                                                , optional
                                                , errorBundlePretty
                                                , parse
                                                , (<|>)
                                                , (<?>)
                                                )
import           NixManager.Util(fromStringEither, TextualError, parseSafe)

-- | The @options.json@ file contains a type for the option. This is a mini-DSL which is typed here.
data NixServiceOptionType = NixServiceOptionInteger
                          | NixServiceOptionFloat
                          | NixServiceOptionAttributeSet (Maybe NixServiceOptionType)
                          | NixServiceOptionBoolean
                          | NixServiceOptionOr NixServiceOptionType NixServiceOptionType
                          | NixServiceOptionOneOfNumeric [Integer]
                          | NixServiceOptionOneOfString [Text]
                          | NixServiceOptionString
                          | NixServiceOptionList NixServiceOptionType
                          | NixServiceOptionLoa NixServiceOptionType
                          | NixServiceOptionPackage
                          | NixServiceOptionPath
                          | NixServiceOptionSubmodule
                          | NixServiceOptionUnspecified
                          | NixServiceOptionNull
                          deriving(Eq)

instance Show NixServiceOptionType where
  show NixServiceOptionInteger  = "integer"
  show NixServiceOptionString   = "string"
  show NixServiceOptionFloat    = "float"
  show (NixServiceOptionList t) = "list of " <> show t <> "s"
  show (NixServiceOptionLoa  t) = "list or attribute set of " <> show t <> "s"
  show NixServiceOptionBoolean  = "boolean"
  show NixServiceOptionPackage  = "package"
  show (NixServiceOptionAttributeSet (Just t)) =
    "attribute set of " <> show t <> "s"
  show (NixServiceOptionAttributeSet Nothing) = "attribute set"
  show NixServiceOptionPath                   = "path"
  show NixServiceOptionSubmodule              = "submodule"
  show NixServiceOptionUnspecified            = "unspecified"
  show NixServiceOptionNull                   = "null"
  show (NixServiceOptionOneOfNumeric xs) =
    "one of " <> intercalate ", " (show <$> xs)
  show (NixServiceOptionOneOfString xs) =
    "one of " <> intercalate ", " (unpack <$> xs)
  show (NixServiceOptionOr a b) = show a <> " or " <> show b


-- | Parsec type for the parser.
type Parser = Parsec Void Text

-- | Parser for decimals (needed for “one of 1, 2”)
decimalParser :: Parser Integer
decimalParser = L.decimal

-- | Parser for string literals (needed for “one of "foo", "bar"”)
stringLiteral :: Parser String
stringLiteral =
  (char '\"' *> manyTill L.charLiteral (char '\"')) <?> "string literal"

-- | Actual service option parser (probably sort of incomplete)
serviceOptionTypeParser :: Parser NixServiceOptionType
serviceOptionTypeParser =
  let
    booleanParser :: Parser NixServiceOptionType
    booleanParser = string "boolean" $> NixServiceOptionBoolean <?> "boolean"
    floatParser :: Parser NixServiceOptionType
    floatParser =
      string "floating point number" $> NixServiceOptionFloat <?> "float"
    stringParser :: Parser NixServiceOptionType
    stringParser = string "string" $> NixServiceOptionString <?> "string"
    nullParser :: Parser NixServiceOptionType
    nullParser = string "null" $> NixServiceOptionNull <?> "null"
    packageParser :: Parser NixServiceOptionType
    packageParser = string "package" $> NixServiceOptionPackage <?> "package"
    pathParser :: Parser NixServiceOptionType
    pathParser = string "path" $> NixServiceOptionPath <?> "path"
    submoduleParser :: Parser NixServiceOptionType
    submoduleParser =
      string "submodule" $> NixServiceOptionSubmodule <?> "submodule"
    unspecifiedParser :: Parser NixServiceOptionType
    unspecifiedParser =
      string "unspecified" $> NixServiceOptionUnspecified <?> "unspecified"
    loaParser :: Parser NixServiceOptionType
    loaParser = do
      void (string "list or attribute set of ")
      expr <- expressionParser
      void (string "s")
      pure (NixServiceOptionLoa expr)
    listParser :: Parser NixServiceOptionType
    listParser = do
      void (string "list of ")
      expr <- expressionParser
      void (string "s")
      pure (NixServiceOptionList expr)
    attributeSetUntyped =
      string "attribute set" $> NixServiceOptionAttributeSet Nothing
    attributeSetTyped = do
      void (string "attribute set of ")
      t <- expressionParser
      void (string "s")
      pure (NixServiceOptionAttributeSet (Just t))
    attributeSetParser :: Parser NixServiceOptionType
    attributeSetParser = attributeSetTyped <|> attributeSetUntyped
    oneOfParser :: Parser NixServiceOptionType
    oneOfParser = do
      void (string "one of ")
      stringSuffix  <- optional (stringLiteral `sepBy` string ", ")
      numericSuffix <- optional (decimalParser `sepBy` string ", ")
      case stringSuffix of
        Nothing -> case numericSuffix of
          Just numbers -> pure (NixServiceOptionOneOfNumeric numbers)
          Nothing ->
            fail "\"one of\" with neither strings nor integers, not supported"
        Just strings -> pure (NixServiceOptionOneOfString (pack <$> strings))
    integerParser =
      optional (decimalParser *> string " bit ")
        *> optional (string "unsigned " <|> string "signed ")
        *> string "integer"
        *> optional
             (  optional (string ";")
             *> string " between "
             *> decimalParser
             *> string " and "
             *> decimalParser
             *> string " (both inclusive)"
             )
        $> NixServiceOptionInteger
    atomicsParser =
      nullParser
        <|> booleanParser
        <|> floatParser
        <|> stringParser
        <|> packageParser
        <|> pathParser
        <|> submoduleParser
        <|> unspecifiedParser
        <|> (integerParser <?> "integer")
        <|> (oneOfParser <?> "\"one of\" expression")
        <|> (loaParser <?> "\"list or attribute set of\" expression")
        <|> (listParser <?> "\"list of\" expression")
        <|> (attributeSetParser <?> "\"attribute set\" expression")
    expressionParser = do
      prefix <- atomicsParser
      suffix <- optional (string " or " *> expressionParser)
      case suffix of
        Nothing      -> pure prefix
        Just suffix' -> pure (NixServiceOptionOr prefix suffix')
  in
    expressionParser

-- | Parse an @options.json@ option type
parseNixServiceOptionType :: Text -> TextualError NixServiceOptionType
parseNixServiceOptionType = parseSafe serviceOptionTypeParser "NixOS type expression"

