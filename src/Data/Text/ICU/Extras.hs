{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE ViewPatterns                #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Data.Text.ICU.Extras (
  match,
  findAndReplace
) where

import Control.Applicative ((<$>), (*>), (<*>), (<|>))
import Control.Monad ((<=<))
import Control.Error (hush)
import Data.Attoparsec.Text (Parser, char, digit, takeWhile1, string, many', parseOnly)
import Data.Functor.Infix ((<$$>), (<&>))
import Data.Maybe (isJust, fromJust)
import Data.Monoid (Monoid(mappend,mconcat))
import Data.Text (Text)
import Data.Text.ICU (regex', find, group, Match)

match :: Text -> Maybe (Text -> Bool)
match = isJust <$$> find <$$> hush . regex' []

findAndReplace :: Text -> Text -> Maybe (Text -> Maybe Text)
findAndReplace pattern replacement = do
  findAnd <- find <$$> hush $ regex' [] pattern
  replace <- flip runReplacement <$> parseReplacement replacement
  return $ replace <=< findAnd

type Replacement = [Segment]

data Segment = Reference Int | Literal Text deriving (Show)

parseReference :: Parser Segment
parseReference = char '$' *> digit <&> Reference . read . return

parseLiteral :: Parser Segment
parseLiteral = Literal <$> takeWhile1 (/= '$')

parseLiteralWithPrecedingDollar :: Parser Segment
parseLiteralWithPrecedingDollar = Literal <$$> mappend <$> string "$" <*> takeWhile1 (/= '$')

parseSegment :: Parser Segment
parseSegment = parseLiteral <|> parseReference <|> parseLiteralWithPrecedingDollar

parseReplacement :: Text -> Maybe Replacement
parseReplacement = hush . parseOnly (many' parseSegment)

runReplacement :: Match -> Replacement -> Maybe Text
runReplacement (flip group -> group) = mconcat <$$> invert . map dereference
  where dereference = \case
          Reference n -> group n
          Literal str -> Just str
        invert xs = if all isJust xs
          then Just $ fromJust <$> xs
          else Nothing
