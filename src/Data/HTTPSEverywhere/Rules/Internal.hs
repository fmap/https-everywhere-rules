module Data.HTTPSEverywhere.Rules.Internal (
  getRulesets
) where

import Prelude hiding (readFile)
import Control.Monad ((<=<))
import Data.Functor.Infix ((<$$>))
import Data.HTTPSEverywhere.Rules.Raw (getRule, getRules)
import Pipes (Producer, for, each, yield, lift)

import Data.HTTPSEverywhere.Rules.Internal.Parser (parseRuleSets)
import Data.HTTPSEverywhere.Rules.Internal.Types (RuleSet)

getRulesets :: Producer RuleSet IO ()
getRulesets = lift getRules >>= flip (for . each) (flip (for . each) yield <=< lift . (parseRuleSets <$$> getRule))
