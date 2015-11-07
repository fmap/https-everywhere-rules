{-# LANGUAGE CPP #-}

module Data.HTTPSEverywhere.Rules.Internal (
  getRulesetsMatching,
  havingRulesThatTrigger,
  havingCookieRulesThatTrigger,
  setSecureFlag,
#ifdef TEST
  hasTargetMatching,
  hasTriggeringRuleOn,
  hasExclusionMatching
#endif
) where

import Prelude hiding (readFile, filter)
import Control.Applicative ((<*>), (<$>))
import Control.Lens ((&))
import Control.Monad ((<=<), join)
import Data.Bool (bool)
import Data.Functor.Infix ((<$$>))
import Data.List (find)
import Data.Maybe (isJust)
import Network.HTTP.Client (Cookie(..))
import Network.URI (URI)
import Pipes (Pipe, Producer, for, each, await, yield, lift, (>->))
import Pipes.Prelude (filter)

import Data.HTTPSEverywhere.Rules.Internal.Parser (parseRuleSets)
import qualified Data.HTTPSEverywhere.Rules.Internal.Raw as Raw (getRule, getRules)
import Data.HTTPSEverywhere.Rules.Internal.Types (RuleSet(..), Target(..), Exclusion(..), Rule(..), CookieRule(..))

getRulesets :: Producer RuleSet IO ()
getRulesets = lift Raw.getRules
          >>= flip (for . each) (flip (for . each) yield <=< lift . (parseRuleSets <$$> Raw.getRule))

getRulesetsMatching :: URI -> Producer RuleSet IO ()
getRulesetsMatching url = getRulesets
                      >-> filter (flip hasTargetMatching url)
                      >-> filter (not . flip hasExclusionMatching url)

havingRulesThatTrigger :: URI -> Pipe RuleSet (Maybe URI) IO ()
havingRulesThatTrigger url = flip hasTriggeringRuleOn url <$> await 
                         >>= maybe (havingRulesThatTrigger url) (yield . Just)

havingCookieRulesThatTrigger :: Cookie -> Pipe RuleSet Bool IO ()
havingCookieRulesThatTrigger cookie = flip hasTriggeringCookieRuleOn cookie <$> await
                                  >>= bool (havingCookieRulesThatTrigger cookie) (yield True)

hasTargetMatching :: RuleSet -> URI -> Bool
hasTargetMatching ruleset url = getTargets ruleset <*> [url] & or
  where getTargets = getTarget <$$> ruleSetTargets

hasExclusionMatching :: RuleSet -> URI -> Bool
hasExclusionMatching ruleset url = getExclusions ruleset <*> [url] & or
  where getExclusions = getExclusion <$$> ruleSetExclusions

hasTriggeringRuleOn :: RuleSet -> URI -> Maybe URI -- Nothing ~ False
hasTriggeringRuleOn ruleset url = getRules ruleset <*> [url] & find isJust & join
  where getRules = getRule <$$> ruleSetRules

hasTriggeringCookieRuleOn :: RuleSet -> Cookie -> Bool
hasTriggeringCookieRuleOn ruleset cookie = getCookieRules ruleset <*> [cookie] & or
  where getCookieRules = getCookieRule <$$> ruleSetCookieRules

setSecureFlag :: Cookie -> Cookie
setSecureFlag cookie = cookie { cookie_secure_only = True }
