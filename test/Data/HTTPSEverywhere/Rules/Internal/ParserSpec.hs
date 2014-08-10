{-# LANGUAGE OverloadedStrings #-}

module Data.HTTPSEverywhere.Rules.Internal.ParserSpec (
  spec
) where

import Data.HTTPSEverywhere.Rules.Internal.Parser (parseTarget)
import Data.HTTPSEverywhere.Rules.Internal.Types (Target(..))
import Data.Maybe (fromJust)
import Network.URI (parseURI)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "parseTarget" $ do
    let checkTarget    = \target domain -> ($ domain) . getTarget $ parseTarget target
        unsafeParseURL = fromJust . parseURI
    it "*.facebook.com should match s-static.ak.facebook.com" $ do
      let url = unsafeParseURL "http://s-static.ak.facebook.com"
      checkTarget "*.facebook.com" url `shouldBe` True
    it "google.* should match google.com.id" $ do
      let url = unsafeParseURL "http://google.com.id"
      checkTarget "google.*" url `shouldBe` True
    it "*.google.* should not match google.com.id" $ do
      let url = unsafeParseURL "http://google.com.id"
      checkTarget "*.google.*" url `shouldBe` False
