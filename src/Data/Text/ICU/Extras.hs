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
import Control.Applicative ((<$>), (<*>), (*>), (<|>))
import Control.Error (hush)
import Data.Attoparsec.Text (Parser, char, digit, takeWhile1, string, many', parseOnly)
import Data.Char (digitToInt)
import Data.Functor.Infix ((<$$>), (<&>))
import Data.Maybe (isJust)
import Data.Monoid (Monoid(mappend, mconcat))
import Data.Text (Text)
import Data.Text.ICU (Regex, regex', find, findAll, group, span, Match, suffix, groupCount)

regex :: Text -> Maybe Regex
regex = hush . regex' []

match :: Text -> Maybe (Text -> Bool)
match = (isJust <$$> find) <$$> regex

findAndReplace :: Text -> Text -> Maybe (Text -> Maybe Text)
findAndReplace pattern replacement = (.)
  <$> runReplacement `fmap` parseReplacement replacement
  <*> findAll `fmap` regex pattern

type Replacement = [Segment]

data Segment = Reference Int | Literal Text deriving (Show, Eq)

-- JavaScript admits at most nine capture groups, so this is the correct
-- (and likely intended) interpretation.
parseReference :: Parser Segment
parseReference = char '$' *> digit <&> Reference . digitToInt

parseLiteral :: Parser Segment
parseLiteral = Literal <$> takeWhile1 (/= '$')

parseLiteralDollar :: Parser Segment
parseLiteralDollar = Literal <$> string "$"

parseSegment :: Parser Segment
parseSegment = parseLiteral <|> parseReference <|> parseLiteralDollar

parseReplacement :: Text -> Maybe Replacement
parseReplacement = hush . parseOnly (many' parseSegment)

-- In the language of ICU, given a match:
--
-- * 'span'     : Text preceding the given match.
-- * 'suffix n' : Suffix of the /n/th capturing group in the match.
--
-- To reconstitute strings in replacement, we 'prefix' all matches
-- with their spans, use their groups to 'dereference' replacements
-- (references are replaced with corresponding captures, literals are
-- inserted as is), cat the results in order, and finally append the
-- suffix of the terminal capturing group ('getSuffix'). When there are
-- no explicit groups, we use the zeroeth; that is, the text following
-- the final match.

runReplacement :: Replacement -> [Match] -> Maybe Text
runReplacement replacement matches = do
  prefix  <- sequence $ matches >>= \match ->
    Just (span match) : dereference match replacement
  mappend (mconcat prefix) <$> getSuffix matches

dereference :: Match -> Replacement -> [Maybe Text]
dereference match = fmap $ \case
  Reference n -> group n match
  Literal str -> Just str

getSuffix :: [Match] -> Maybe Text
getSuffix [] = Just mempty
getSuffix ms = flip suffix <*> groupCount $ last ms
