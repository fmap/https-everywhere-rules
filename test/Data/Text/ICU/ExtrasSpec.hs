{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Text.ICU.ExtrasSpec (spec) where

import Control.Applicative ((<$>))
import Control.Lens ((&))
import Data.Text (Text)
import Data.Text.ICU.Extras (match, findAndReplace, Segment(..), parseReplacement)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "match" $ do
    it "Should be Nothing if provided an invalid regular expression." $ do
      match "(" `shouldBe` Nothing
    it "Should yield a match function if provided a regular expression." $ do
     ("xa" &) <$> match "x" `shouldBe` Just True
     ("xa" &) <$> match "y" `shouldBe` Just False
  describe "parseReplacement" $ do
    it "Should decompose a replacement string into a sequence [Segment]." $ do
      parseReplacement "foo$1bar$4$1" `shouldBe` Just [Literal "foo", Reference 1, Literal "bar", Reference 4, Reference 1]
    it "Should correctly parse successive '$'s" $ do
      parseReplacement "$$1" `shouldBe` Just [Literal "$", Reference 1]
      parseReplacement "$$" `shouldBe` Just [Literal "$", Literal "$"]
    it "Should identity no more than nine capture groups." $ do
      parseReplacement "$10" `shouldBe` Just [Reference 1, Literal "0"]
  describe "findAndReplace" $ do
    it "Should find and replace based upon a regular expression and pattern." $ do
      ("barqux" &) <$> findAndReplace "(bar)" "$1baz" `shouldBe` Just (Just "barbazqux")
      ("barqux" &) <$> findAndReplace "(qux)" "baz$1" `shouldBe` Just (Just "barbazqux")
      ("barqux" &) <$> findAndReplace "u" "uu" `shouldBe` Just (Just "barquux")

instance Show (Text -> Bool) where
  show _ = "Text -> Bool"

instance Eq (Text -> Bool) where
  _ == _ = False
