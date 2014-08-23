{-# LANGUAGE OverloadedStrings #-}

module Data.HTTPSEverywhere.Rules.InternalSpec (
  spec
) where

import Prelude hiding (unlines)
import Control.Monad (join)
import Data.Functor ((<$>))
import Data.HTTPSEverywhere.Rules.Internal (hasTargetMatching, hasTriggeringRuleOn, hasExclusionMatching)
import Data.HTTPSEverywhere.Rules.Internal.Types (RuleSet(..), Target(..), Exclusion(..), Rule(..))
import Data.Maybe (fromJust, catMaybes)
import Data.Text (pack, unpack)
import Data.Text.ICU.Extras (match, findAndReplace)
import Network.URI (parseURI)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  let unsafeParseURL = fromJust . parseURI
  describe "hasTargetMatching" $ do
    it "Should be True for matching targets." $ do
      let url = unsafeParseURL "http://eff.org/thing"
      fixture `hasTargetMatching` url `shouldBe` True
    it "Should be False for mis-matching targets." $ do
      let url = unsafeParseURL "http://gnu.org/thing"
      fixture `hasTargetMatching` url `shouldBe` False
  describe "hasTriggeringRuleOn" $ do
    it "Should be 'Just https://...' for matching rules." $ do
      let url = unsafeParseURL "http://eff.org/thing"
      fixture `hasTriggeringRuleOn`  url `shouldBe` parseURI "https://eff.org/thing"
    it "Should be Nothing for mis-matching rules." $ do
      let url = unsafeParseURL "http://gnu.org/thing"
      fixture `hasTriggeringRuleOn` url `shouldBe` Nothing
  describe "hasExclusionMatching" $ do
    it "Should be True for matching targets." $ do
      let url = unsafeParseURL "http://action.eff.org/thing"
      fixture `hasExclusionMatching` url `shouldBe` True
    it "Should be False for mis-matching targets." $ do
      let url = unsafeParseURL "http://eff.org/thing"
      fixture `hasExclusionMatching` url `shouldBe` False

-- We avoid the parser here, for purposes of isolation:

fixture :: RuleSet
fixture = RuleSet
  { ruleSetName = "EFF"
  , ruleSetTargets = catMaybes
      [ Target . checkURI <$> match "eff\\.org"
      , Target . checkURI <$> match ".*\\.eff\\.org"
      ]
  , ruleSetExclusions = catMaybes
      [ Exclusion . checkURI <$> match "^http://action\\.eff\\.org/"
      ]
  , ruleSetRules = catMaybes
      [ Rule . changeURI <$> findAndReplace "^http://eff\\.org/" "https://eff.org/"
      , Rule . changeURI <$> findAndReplace "^http://([^/:@]*)\\.eff\\.org/" "https://$1.eff.org/"
      ]
  , ruleSetCookieRules =
      []
  } 
  where checkURI = (. pack . show)
        changeURI f = join . fmap (parseURI . unpack) . checkURI f
