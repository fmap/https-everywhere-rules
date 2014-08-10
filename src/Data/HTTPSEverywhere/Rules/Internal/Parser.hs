{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

module Data.HTTPSEverywhere.Rules.Internal.Parser (
  parseRuleSets,
#ifdef TEST
  parseTarget
#endif
) where

import Prelude hiding (head, last, tail, init)
import Control.Lens (toListOf, only, to, (^..), (^.), (&), (<&>), _Just)
import Control.Monad (join)
import Data.Functor.Infix ((<$>),(<$$>))
import Data.Maybe (catMaybes, fromJust, fromMaybe)
import Data.String.Conversions (cs)
import qualified Data.Text as Strict (Text)
import Data.Text (append, head, last, tail, init, replace)
import qualified Data.Text.Lazy as Lazy (Text)
import Network.HTTP.Client (Cookie(..))
import Network.URI (URI(uriAuthority), URIAuth(uriRegName), parseURI)
import Text.Taggy.Lens (html, allNamed, attr, Element)

import Data.HTTPSEverywhere.Rules.Internal.Types (RuleSet(..), Target(..), Rule(..), Exclusion(..), CookieRule(..))
import Data.Text.ICU.Extras (match, findAndReplace)

parseRuleSets :: Lazy.Text -> [RuleSet]
parseRuleSets = catMaybes <$$> toListOf $ html . allNamed (only "ruleset") . to parseRuleSet

parseRuleSet :: Element -> Maybe RuleSet
parseRuleSet xml = xml ^. attr "name" <&> \ruleSetName -> do
  let ruleSetTargets     = xml ^.. allNamed (only "target") . attr "name" . _Just . to parseTarget
      ruleSetRules       = xml ^.. allNamed (only "rule") . to parseRule & catMaybes
      ruleSetExclusions  = xml ^.. allNamed (only "exclusion") . attr "pattern" . _Just . to parseExclusion & catMaybes
      ruleSetCookieRules = xml ^.. allNamed (only "securecookie") . to parseCookieRule & catMaybes
  RuleSet ruleSetName ruleSetTargets ruleSetRules ruleSetExclusions ruleSetCookieRules

parseTarget :: Strict.Text -> Target
parseTarget = Target . checkRegName . fromJust . match . fromWildcard . replace "." "\\."
  where fromWildcard str
          | head str == '*' = flip append ".*" $ tail str
          | last str == '*' = append ".*" $ init str
          | otherwise       = str
        checkRegName predicate = fromMaybe False . (predicate <$$> getRegName)
        getRegName = cs . uriRegName <$$> uriAuthority

parseRule :: Element -> Maybe Rule
parseRule element = do
  pattern     <- element ^. attr "from"
  replacement <- element ^. attr "to"
  substitute  <- findAndReplace pattern replacement 
  return . Rule $ join . fmap (parseURI . cs) . substitute . cs . show

parseExclusion :: Strict.Text -> Maybe Exclusion
parseExclusion = Exclusion . (. cs . show) <$$> match

parseCookieRule :: Element -> Maybe CookieRule
parseCookieRule element = CookieRule <$> do
  hostMatches <- element ^. attr "host" . to (>>= match)
  nameMatches <- element ^. attr "name" . to (>>= match)
  return $ \Cookie{..} -> hostMatches (cs cookie_name) && nameMatches (cs cookie_domain)
