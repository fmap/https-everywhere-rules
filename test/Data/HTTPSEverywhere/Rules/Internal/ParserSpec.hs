{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.HTTPSEverywhere.Rules.Internal.ParserSpec (
  spec
) where

import Prelude hiding (unlines)
import Data.HTTPSEverywhere.Rules.Internal.Parser (parseTarget, parseRuleSets)
import Data.HTTPSEverywhere.Rules.Internal (hasTargetMatching, hasExclusionMatching, hasTriggeringRuleOn)
import Data.HTTPSEverywhere.Rules.Internal.Types (Target(..), RuleSet(..))
import Data.Maybe (fromJust, isJust)
import Data.Text.Lazy (Text, unlines)
import Network.URI (parseURI)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = do
  let unsafeParseURL = fromJust . parseURI
  describe "parseTarget" $ do
    let checkTarget = \target domain -> ($ domain) . getTarget $ parseTarget target
    it "*.facebook.com should match s-static.ak.facebook.com" $ do
      let url = unsafeParseURL "http://s-static.ak.facebook.com"
      checkTarget "*.facebook.com" url `shouldBe` True
    it "google.* should match google.com.id" $ do
      let url = unsafeParseURL "http://google.com.id"
      checkTarget "google.*" url `shouldBe` True
    it "*.google.* should not match google.com.id" $ do
      let url = unsafeParseURL "http://google.com.id"
      checkTarget "*.google.*" url `shouldBe` False
  describe "parseRuleSets" $ do
    let rulesets = parseRuleSets fixture
    it "Should parse a ruleset out of the EFF fixture." $ do
      rulesets `shouldSatisfy` not . null
    let effRuleSet = head rulesets
    it "Should parse out the name of the EFF fixture." $ do
      ruleSetName effRuleSet `shouldBe` "EFF"
    it "Should parse out two targets from the EFF fixture." $ do
      length (ruleSetTargets effRuleSet) `shouldBe` 2
    it "Should parse out two rules from the EFF fixture." $ do
      length (ruleSetRules effRuleSet) `shouldBe` 2
    it "Should parse out one exclusion from the EFF fixture." $ do
      length (ruleSetExclusions effRuleSet) `shouldBe` 1

    -- BE WARY: the succeeding may fail due to bugs in the parent module, which
    -- has no tests yet.

    it "A target in the EFF fixture should match eff.org" $ do
      let url = unsafeParseURL "http://eff.org/thing"
      effRuleSet `hasTargetMatching` url `shouldBe` True
    it "A rule in the EFF fixture should trigger on eff.org" $ do
      let url = unsafeParseURL "http://eff.org/thing"
      effRuleSet `hasTriggeringRuleOn` url `shouldSatisfy` isJust
    it "An exclusion in the EFF fixture should trigger on action.eff.org" $ do
      let url = unsafeParseURL "http://action.eff.org/thing"
      effRuleSet `hasExclusionMatching` url `shouldBe` True

fixture :: Text
fixture = unlines $
  [ "<ruleset name=\"EFF\">"
  , "  <target host=\"eff.org\" />"
  , "  <target host=\"*.eff.org\" />"
  , ""
  , "  <exclusion pattern=\"^http://action\\.eff\\.org/\"/>"
  , "  <rule from=\"^http://eff\\.org/\" to=\"https://eff.org/\"/>"
  , "  <rule from=\"^http://([^/:@]*)\\.eff\\.org/\" to=\"https://$1.eff.org/\"/>"
  , "</ruleset>"
  ]

instance Show RuleSet where
  show _ = "<RuleSet>"
