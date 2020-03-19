{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveTraversable #-}
module NixManager.NixExpr
  ( parseNixFile
  , NixExprF(..)
  , NixExpr
  , evalSymbols
  , NixFunction
  , nfArgs
  , nfExpr
  , _NixNull'
  , _NixFunctionDecl'
  , _NixSymbol'
  , _NixList'
  , _NixString'
  , _NixBoolean'
  , _NixSet'
  , writeNixFile
  , _NixInt'
  , _NixFloat'
  )
where

import           NixManager.Util
import           Data.Foldable                  ( fold )
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
import           Data.Fix                       ( Fix(Fix)
                                                , cata
                                                )
import           Control.Lens                   ( makePrisms
                                                , makeLenses
                                                , prism'
                                                , Prism'
                                                , (^.)
                                                )

data NixFunction a = NixFunction {
    _nfArgs :: [Text]
  , _nfExpr :: a
  } deriving(Show, Functor, Traversable, Foldable)

makeLenses ''NixFunction

data NixExprF r = NixList [r]
             | NixSet (Map Text r)
             | NixFunctionDecl (NixFunction r)
             | NixSymbol Text
             | NixString Text
             | NixBoolean Bool
             | NixInt Integer
             | NixFloat Double
             | NixNull
             deriving(Show, Functor, Traversable, Foldable)

type NixExpr = Fix NixExprF

makePrisms ''NixExprF

-- FIXME: We need a Prism or something for Fix, but I don't know how to write one.
_NixNull' :: Prism' NixExpr ()
_NixNull' = prism' (\_ -> Fix NixNull) f
 where
  f (Fix NixNull) = Just ()
  f _             = Nothing

_NixFloat' :: Prism' NixExpr Double
_NixFloat' = prism' (Fix . NixFloat) f
 where
  f (Fix (NixFloat x)) = Just x
  f _                  = Nothing

_NixInt' :: Prism' NixExpr Integer
_NixInt' = prism' (Fix . NixInt) f
 where
  f (Fix (NixInt x)) = Just x
  f _                = Nothing

_NixBoolean' :: Prism' NixExpr Bool
_NixBoolean' = prism' (Fix . NixBoolean) f
 where
  f (Fix (NixBoolean x)) = Just x
  f _                    = Nothing

_NixString' :: Prism' NixExpr Text
_NixString' = prism' (Fix . NixString) f
 where
  f (Fix (NixString x)) = Just x
  f _                   = Nothing

_NixSymbol' :: Prism' NixExpr Text
_NixSymbol' = prism' (Fix . NixSymbol) f
 where
  f (Fix (NixSymbol x)) = Just x
  f _                   = Nothing

_NixFunctionDecl' :: Prism' NixExpr (NixFunction NixExpr)
_NixFunctionDecl' = prism' (Fix . NixFunctionDecl) f
 where
  f (Fix (NixFunctionDecl t)) = Just t
  f _                         = Nothing

_NixSet' :: Prism' NixExpr (Map Text NixExpr)
_NixSet' = prism' (Fix . NixSet) f
 where
  f (Fix (NixSet m)) = Just m
  f _                = Nothing

_NixList' :: Prism' NixExpr [NixExpr]
_NixList' = prism' (Fix . NixList) f
 where
  f (Fix (NixList m)) = Just m
  f _                 = Nothing

evalSymbols :: NixExprF [Text] -> [Text]
evalSymbols (NixSymbol r) = [r]
evalSymbols e             = fold e

prettyPrint :: NixExprF Text -> Text
prettyPrint NixNull            = "null"
prettyPrint (NixFloat   f    ) = showText f
prettyPrint (NixInt     f    ) = showText f
prettyPrint (NixBoolean True ) = "true"
prettyPrint (NixBoolean False) = "false"
prettyPrint (NixString  s    ) = "\"" <> s <> "\""
prettyPrint (NixSymbol  s    ) = s
prettyPrint (NixFunctionDecl fn) =
  "{ " <> intercalate "," (fn ^. nfArgs) <> " }: " <> (fn ^. nfExpr)
prettyPrint (NixList xs) = "[ " <> unwords xs <> " ]"
prettyPrint (NixSet m) =
  "{ " <> foldMap (\(k, v) -> k <> " = " <> v <> "; ") (toList m) <> " }"

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
  pure (Fix (NixList exprs))

stringParser :: Parser NixExpr
stringParser = Fix . NixString . pack <$> stringLiteral

boolParser :: Parser NixExpr
boolParser =
  (   (string "true" $> Fix (NixBoolean True))
    <|> (string "false" $> Fix (NixBoolean False))
    )
    <?> "boolean"

intParser :: Parser NixExpr
intParser = Fix . NixInt <$> signedInteger <?> "int"

functionDeclParser :: Parser NixExpr
functionDeclParser = do
  void (lexeme (char '{'))
  symbols <- lexeme symbolParser' `sepBy` lexeme (char ',')
  void (lexeme (char '}'))
  void (lexeme (char ':'))
  Fix . NixFunctionDecl . NixFunction symbols <$> exprParser


floatParser :: Parser NixExpr
floatParser = Fix . NixFloat <$> signedFloat <?> "float"

nullParser :: Parser NixExpr
nullParser = (string "null" $> Fix NixNull) <?> "null"

symbolChar :: Parser Char
symbolChar = alphaNumChar <|> char '.' <|> char '-'

symbolParser' :: Parser Text
symbolParser' =
  string "..." <|> (pack <$> ((:) <$> letterChar <*> many symbolChar))

symbolParser :: Parser NixExpr
symbolParser = Fix . NixSymbol <$> symbolParser' <?> "symbol"

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
  pure (Fix (NixSet bindings))

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
writeNixFile fp = writeFile fp . cata prettyPrint
