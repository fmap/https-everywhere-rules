{-# LANGUAGE OverloadedStrings #-}

module Data.HTTPSEverywhere.Rules.Internal.ParserSpec (
  spec
) where

import Data.HTTPSEverywhere.Rules.Internal.Types (Target(..))
import Data.HTTPSEverywhere.Rules.Internal.Parser (parseTarget)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "parseTarget" $ do
    let parseTarget' = \target domain -> ($ domain) . getTarget $ parseTarget target
    it "*.facebook.com should match s-static.ak.facebook.com" $ do
      parseTarget' "*.facebook.com" "s-static.ak.facebook.com" `shouldBe` True
    it "google.* should match google.com.id" $ do
      parseTarget' "google.*" "google.com.id" `shouldBe` True
    it "*.google.* should not match google.com.id" $ do
      parseTarget' "*.google.*" "google.com.id" `shouldBe` False
