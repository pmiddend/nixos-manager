{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveTraversable #-}
module NixManager.NixExpr
  ( parseNixFile
  , NixExpr(..)
  , evalSymbols
  , NixFunction
  , nfArgs
  , nfExpr
  , _NixNull
  , _NixFunctionDecl
  , _NixSymbol
  , _NixList
  , _NixString
  , _NixBoolean
  , _NixSet
  , _NixInt
  , _NixFloat
  , writeNixFile
  )
where

import           NixManager.Util
import           Data.Bifunctor                 ( first )
import           Control.Monad                  ( void )
import           Data.Functor                   ( ($>) )
import           Text.Megaparsec                ( Parsec
                                                , manyTill
                                                , sepBy
                                                , many
                                                , errorBundlePretty
                                                , try
                                                , parse
                                                , (<|>)
                                                , (<?>)
                                                )
import           Prelude                 hiding ( readFile
                                                , unwords
                                                , writeFile
                                                )
import           Data.Void                      ( Void )
import           Data.Text.IO                   ( readFile
                                                , writeFile
                                                )
import           Data.Text                      ( Text
                                                , intercalate
                                                , unwords
                                                , pack
                                                )
import           Data.Map.Strict                ( Map
                                                , fromList
                                                , toList
                                                )
import qualified Text.Megaparsec.Char.Lexer    as L
import           Text.Megaparsec.Char           ( space1
                                                , letterChar
                                                , alphaNumChar
                                                , char
                                                , string
                                                )
import           Control.Applicative            ( empty )
import           Control.Lens                   ( makePrisms
                                                , makeLenses
                                                , to
                                                , (^..)
                                                , traversed
                                                , (^.)
                                                )

data NixFunction = NixFunction {
    _nfArgs :: [Text]
  , _nfExpr :: NixExpr
  } deriving(Show)


data NixExpr = NixList [NixExpr]
             | NixSet (Map Text NixExpr)
             | NixFunctionDecl NixFunction
             | NixSymbol Text
             | NixString Text
             | NixBoolean Bool
             | NixInt Integer
             | NixFloat Double
             | NixNull
             deriving(Show)

makePrisms ''NixExpr
makeLenses ''NixFunction

evalSymbols :: NixExpr -> [Text]
evalSymbols (NixSymbol r) = [r]
evalSymbols _             = []

prettyPrint :: NixExpr -> Text
prettyPrint NixNull            = "null"
prettyPrint (NixFloat   f    ) = showText f
prettyPrint (NixInt     f    ) = showText f
prettyPrint (NixBoolean True ) = "true"
prettyPrint (NixBoolean False) = "false"
prettyPrint (NixString  s    ) = "\"" <> s <> "\""
prettyPrint (NixSymbol  s    ) = s
prettyPrint (NixFunctionDecl fn) =
  "{ "
    <> intercalate "," (fn ^. nfArgs)
    <> " }: "
    <> (fn ^. nfExpr . to prettyPrint)
prettyPrint (NixList xs) =
  "[ " <> unwords (xs ^.. traversed . to prettyPrint) <> " ]"
prettyPrint (NixSet m) =
  "{\n"
    <> foldMap (\(k, v) -> "  " <> k <> " = " <> prettyPrint v <> ";\n")
               (toList m)
    <> "}"

type Parser = Parsec Void Text

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

sc :: Parser ()
sc = L.space space1 lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

float :: Parser Double
float = lexeme L.float

integer :: Parser Integer
integer = lexeme L.decimal

signedInteger :: Parser Integer
signedInteger = L.signed sc integer

signedFloat :: Parser Double
signedFloat = L.signed sc float

stringLiteral :: Parser String
stringLiteral =
  (char '\"' *> manyTill L.charLiteral (char '\"')) <?> "string literal"

listParser :: Parser NixExpr
listParser = do
  void (lexeme (char '['))
  exprs <- many (lexeme exprParser)
  void (char ']')
  pure (NixList exprs)

stringParser :: Parser NixExpr
stringParser = NixString . pack <$> stringLiteral

boolParser :: Parser NixExpr
boolParser =
  ((string "true" $> NixBoolean True) <|> (string "false" $> NixBoolean False))
    <?> "boolean"

intParser :: Parser NixExpr
intParser = NixInt <$> signedInteger <?> "int"

functionDeclParser :: Parser NixExpr
functionDeclParser = do
  void (lexeme (char '{'))
  symbols <- lexeme symbolParser' `sepBy` lexeme (char ',')
  void (lexeme (char '}'))
  void (lexeme (char ':'))
  NixFunctionDecl . NixFunction symbols <$> exprParser


floatParser :: Parser NixExpr
floatParser = NixFloat <$> signedFloat <?> "float"

nullParser :: Parser NixExpr
nullParser = (string "null" $> NixNull) <?> "null"

symbolChar :: Parser Char
symbolChar = alphaNumChar <|> char '.' <|> char '-'

symbolParser' :: Parser Text
symbolParser' =
  string "..." <|> (pack <$> ((:) <$> letterChar <*> many symbolChar))

symbolParser :: Parser NixExpr
symbolParser = NixSymbol <$> symbolParser' <?> "symbol"

setParser :: Parser NixExpr
setParser = do
  void (lexeme (char '{'))
  let bindingParser = do
        key <- lexeme symbolParser' <?> "set key"
        void (lexeme (char '='))

        value <- exprParser <?> "set value"
        void (lexeme (char ';'))
        pure (key, value)
  bindings <- fromList <$> many bindingParser
  void (lexeme (char '}'))
  pure (NixSet bindings)

exprParser :: Parser NixExpr
exprParser =
  listParser
    <|> try functionDeclParser
    <|> setParser
    <|> stringParser
    <|> nullParser
    <|> boolParser
    <|> try floatParser
    <|> try intParser
    <|> symbolParser

parseNixFile :: FilePath -> IO (Either String NixExpr)
parseNixFile fn = first errorBundlePretty . parse exprParser fn <$> readFile fn

writeNixFile :: FilePath -> NixExpr -> IO ()
writeNixFile fp = writeFile fp . prettyPrint
