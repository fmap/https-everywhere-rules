module Data.HTTPSEverywhere.Rules (
  rewriteURL,
  rewriteCookie
) where

import Prelude hiding (take, null)
import Control.Applicative ((<$))
import Control.Lens ((<&>))
import Data.Bool (bool)
import Data.Text (Text)
import Network.HTTP.Client (Cookie)
import Pipes (runEffect, (>->))
import Pipes.Prelude (take, null)

import Data.HTTPSEverywhere.Rules.Internal (getRulesetsMatching, havingRulesThatTrigger, havingCookieRulesThatTrigger, setSecureFlag)

rewriteURL :: Text -> IO (Maybe Text)
rewriteURL url = runEffect
               $ (Nothing <$ getRulesetsMatching url)
             >-> (Nothing <$ havingRulesThatTrigger url)
             >-> (Nothing <$ take 1)

rewriteCookie :: Text -> Cookie -> IO Cookie
rewriteCookie url cookie = null producer <&> setSecureFlag cookie `bool` cookie
  where producer = getRulesetsMatching url >-> (() <$ havingCookieRulesThatTrigger cookie)
