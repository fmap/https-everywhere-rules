module Data.HTTPSEverywhere.Rules.Internal.Types (
  RuleSet(..),
  Target(..),
  Rule(..),
  Exclusion(..),
  CookieRule(..)
) where

import Data.Text (Text)
import Network.HTTP.Client (Cookie)
import Network.URI (URI)

newtype Rule       = Rule       { getRule :: URI -> Maybe URI     }
newtype Target     = Target     { getTarget :: URI  -> Bool       }
newtype Exclusion  = Exclusion  { getExclusion :: URI -> Bool     }
newtype CookieRule = CookieRule { getCookieRule :: Cookie -> Bool }

data RuleSet = RuleSet
  { ruleSetName        :: Text
  , ruleSetTargets     :: [Target]
  , ruleSetRules       :: [Rule]
  , ruleSetExclusions  :: [Exclusion]
  , ruleSetCookieRules :: [CookieRule]
  }
