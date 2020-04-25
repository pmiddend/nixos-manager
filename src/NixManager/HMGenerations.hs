{-|
  Description: Contains code to read and manipulate home-manager’s generations
Contains code to read and manipulate home-manager’s generations
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module NixManager.HMGenerations
  ( readGenerations
  , removeGeneration
  , GenerationLine
  , glDate
  , glId
  , activateGeneration
  , glDatePretty
  , glPath
  )
where

import Data.Validation(Validation(Failure))
import           System.Exit                    ( ExitCode(ExitFailure) )
import           NixManager.Process             ( runProcessToFinish
                                                , noStdin
                                                , poStdout
                                                , poStderr
                                                , poResult
                                                )
import           Control.Lens                   ( (^?!)
                                                , to
                                                , makeLenses
                                                , folded
                                                , (^.)
                                                )
import           Data.Monoid                    ( getFirst )
import           NixManager.Bash                ( Expr(Command)
                                                , Arg(LiteralArg)
                                                )
import           NixManager.Util                ( TextualError
                                                , parseSafe
                                                , showText
                                                , decodeUtf8
                                                , addToError
                                                )
import           Control.Monad                  ( void )
import           Data.Void                      ( Void )
import           Text.Megaparsec                ( Parsec
                                                , takeWhile1P
                                                , parse
                                                , errorBundlePretty
                                                , sepEndBy
                                                )
import           Text.Megaparsec.Byte           ( char
                                                , newline
                                                , string
                                                )
import           Data.ByteString                ( ByteString )
import           Data.ByteString.Char8          ( unpack )
import           Data.Char                      ( ord )
import           Data.Bifunctor                 ( first )
import           Data.Time.Clock                ( UTCTime )
import           Data.Time.LocalTime            ( TimeZone
                                                , localTimeToUTC
                                                , LocalTime
                                                , getZonedTime
                                                , ZonedTime
                                                , zonedTimeZone
                                                , zonedTimeToUTC
                                                )
import           Data.Time.Format               ( parseTimeM
                                                , defaultTimeLocale
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Text.Time.Pretty               ( prettyTimeAuto )

-- | One home-manager generation line
data GenerationLine = GenerationLine {
    _glDate :: UTCTime -- ^ The parsed activation date for the generation
  , _glDatePretty :: Text -- ^ The prettified, human-readable date for the generation
  , _glId :: ByteString -- ^ The generation’s id, here as a text, since I wasn’t sure if it’s always numeric
  , _glPath :: ByteString -- ^ The generation’s path, which is vital for activating it
  } deriving(Show)

makeLenses ''GenerationLine

-- | Parsec type for the parser.
type Parser = Parsec Void ByteString

-- | Roughly parse a word
wordParser :: Parser ByteString
wordParser = takeWhile1P Nothing (/= fromIntegral (ord ' '))

-- | Parse anything that's not EOL
nonEolParser :: Parser ByteString
nonEolParser = takeWhile1P Nothing (/= fromIntegral (ord '\n'))

-- | Parse home-manager’s time format 
parseTime :: String -> Maybe LocalTime
parseTime = parseTimeM False defaultTimeLocale "%0Y-%m-%d %H:%M"

-- | Parse home-manager’s time format as UTC
generationTimeParser :: TimeZone -> Parser UTCTime
generationTimeParser tz = do
  yymmdd <- wordParser
  void (char (fromIntegral (ord ' ')))
  hhmm      <- wordParser
  localTime <- maybe (fail "wrong date/time format for generation")
                     pure
                     (parseTime (unpack yymmdd <> " " <> unpack hhmm))
  pure (localTimeToUTC tz localTime)

-- | Parse a whole generation line, given the current date, time and time zone
generationLineParser :: ZonedTime -> Parser GenerationLine
generationLineParser now = do
  time <- generationTimeParser (zonedTimeZone now)
  let timePretty = pack (prettyTimeAuto (zonedTimeToUTC now) time)
  GenerationLine time timePretty
    <$> (string " : id " *> wordParser)
    <*> (string " -> " *> nonEolParser)

-- | Parse @home-manager generations@, given the current date, time and time zone
parseGenerations :: ZonedTime -> ByteString -> TextualError [GenerationLine]
parseGenerations now = parseSafe
  (generationLineParser now `sepEndBy` newline)
  "home-manager generations"


-- | Remove a specific generation
removeGeneration :: GenerationLine -> IO ()
removeGeneration genLine = void $ runProcessToFinish noStdin $ Command
  "home-manager"
  ["remove-generations", LiteralArg (genLine ^. glId . decodeUtf8)]

-- | Activate a specific generation
activateGeneration :: GenerationLine -> IO ()
activateGeneration genLine = void $ runProcessToFinish noStdin $ Command
  ((genLine ^. glPath . decodeUtf8) <> "/activate")
  []

-- | Read all generations
readGenerations :: IO (TextualError [GenerationLine])
readGenerations = do
  nowZoned <- getZonedTime
  po <- runProcessToFinish noStdin (Command "home-manager" ["generations"])
  case po ^?! poResult . to getFirst . folded of
    ExitFailure code -> pure
      (Failure
        (  "Error executing generations query for home-manager. Exit code was: "
        <> showText code
        <> ". The stderr output was:\n\n"
        <> (po ^. poStderr . decodeUtf8)
        )
      )
    _ -> pure
      (addToError "Couldn't parse generations output: "
                  (parseGenerations nowZoned (po ^. poStdout))
      )
