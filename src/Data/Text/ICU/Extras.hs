{-# LANGUAGE CPP                         #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Data.Text.ICU.Extras (
  match,
  findAndReplace,
#ifdef TEST
  Segment(..),
  parseReplacement
#endif
) where

import Prelude hiding (span)
import Control.Applicative ((<$>), (*>), (<|>))
import Control.Error (hush)
import Control.Monad (ap)
import Data.Attoparsec.Text (Parser, char, digit, takeWhile1, string, many', parseOnly)
import Data.Functor.Infix ((<$$>), (<&>))
import Data.Maybe (isJust, fromJust)
import Data.Monoid (Monoid(mconcat))
import Data.Text (Text)
import Data.Text.ICU (regex', find, findAll, group, span, Match, suffix, groupCount)

match :: Text -> Maybe (Text -> Bool)
match = isJust <$$> find <$$> hush . regex' []

findAndReplace :: Text -> Text -> Maybe (Text -> Maybe Text)
findAndReplace pattern replacement = do
  findAnd <- findAll <$$> hush $ regex' [] pattern
  replace <- flip runReplacement <$> parseReplacement replacement
  return $ replace . findAnd

type Replacement = [Segment]

data Segment = Reference Int | Literal Text
  deriving (Show, Eq)

parseReference :: Parser Segment
parseReference = char '$' *> digit <&> Reference . read . return

parseLiteral :: Parser Segment
parseLiteral = Literal <$> takeWhile1 (/= '$')

parseLiteralDollar :: Parser Segment
parseLiteralDollar = Literal <$> string "$"

parseSegment :: Parser Segment
parseSegment = parseLiteral <|> parseReference <|> parseLiteralDollar

parseReplacement :: Text -> Maybe Replacement
parseReplacement = hush . parseOnly (many' parseSegment)

runReplacement :: [Match] -> Replacement -> Maybe Text
runReplacement matches replacement = mconcat <$$> invert . adornSuffix (last matches) $ do
  match <- matches
  Just (span match) : map (dereference $ flip group match) replacement

adornSuffix :: Match -> ([Maybe Text] -> [Maybe Text])
adornSuffix match = (++ [ap (flip suffix) groupCount match])

dereference :: (Int -> Maybe Text) -> Segment -> Maybe Text
dereference group = \case
  Reference n -> group n
  Literal str -> Just str

invert :: [Maybe a] -> Maybe [a]
invert xs | all isJust xs = Just $ fromJust <$> xs
          | otherwise     = Nothing
