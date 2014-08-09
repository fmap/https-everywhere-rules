module Data.HTTPSEverywhere.Rules.Internal.Types (
  RuleSet(..),
  Target(..),
  Rule(..),
  Exclusion(..),
  CookieRule(..)
) where

import Data.Text (Text)
import Network.HTTP.Client (Cookie)

newtype Rule       = Rule       { getRule :: Text -> Maybe Text   }
newtype Target     = Target     { getTarget :: Text -> Bool       }
newtype Exclusion  = Exclusion  { getExclusion :: Text -> Bool    }
newtype CookieRule = CookieRule { getCookieRule :: Cookie -> Bool }

data RuleSet = RuleSet
  { ruleSetName        :: Text
  , ruleSetTargets     :: [Target]
  , ruleSetRules       :: [Rule]
  , ruleSetExclusions  :: [Exclusion]
  , ruleSetCookieRules :: [CookieRule]
  }
