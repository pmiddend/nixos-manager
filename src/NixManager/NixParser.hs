{-# LANGUAGE OverloadedStrings #-}
module NixManager.NixParser where

import           Control.Monad                  ( void )
import           Data.Functor                   ( ($>) )
import           Text.Megaparsec                ( Parsec
                                                , manyTill
                                                , sepBy
                                                , many
                                                , try
                                                , (<|>)
                                                , (<?>)
                                                )
import           Data.Void                      ( Void )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Map.Strict                ( Map
                                                , fromList
                                                )
import qualified Text.Megaparsec.Char.Lexer    as L
import           Text.Megaparsec.Char           ( space1
                                                , letterChar
                                                , alphaNumChar
                                                , char
                                                , string
                                                )
import           Control.Applicative            ( empty )


data NixExpr = NixList [NixExpr]
             | NixSet (Map Text NixExpr)
             | NixFunctionDecl [Text] NixExpr
             | NixSymbol Text
             | NixString Text
             | NixBoolean Bool
             | NixInt Integer
             | NixFloat Double
             | NixNull
             deriving(Show)

type Parser = Parsec Void Text

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

sc :: Parser ()
sc = L.space space1 lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

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
  symbols <- symbolParser' `sepBy` lexeme (char ',')
  void (lexeme (char '}'))
  void (lexeme (char ':'))
  expr <- exprParser
  pure (NixFunctionDecl symbols expr)


floatParser :: Parser NixExpr
floatParser = NixFloat <$> signedFloat <?> "float"

nullParser :: Parser NixExpr
nullParser = (string "null" $> NixNull) <?> "null"

symbolChar :: Parser Char
symbolChar = alphaNumChar <|> char '.'

symbolParser' :: Parser Text
symbolParser' = pack <$> ((:) <$> letterChar <*> many symbolChar)

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
