module Data.HTTPSEverywhere.Rules (
  rewriteURL,
  rewriteCookie
) where

import Prelude hiding (take, null)
import Control.Applicative ((<$))
import Control.Lens ((<&>))
import Data.Bool (bool)
import Network.HTTP.Client (Cookie)
import Network.URI (URI)
import Pipes (runEffect, (>->))
import Pipes.Prelude (take, null)

import Data.HTTPSEverywhere.Rules.Internal (getRulesetsMatching, havingRulesThatTrigger, havingCookieRulesThatTrigger, setSecureFlag)

rewriteURL :: URI -> IO (Maybe URI)
rewriteURL url = runEffect
               $ (Nothing <$ getRulesetsMatching url)
             >-> (Nothing <$ havingRulesThatTrigger url)
             >-> (Nothing <$ take 1)

rewriteCookie :: URI -> Cookie -> IO Cookie
rewriteCookie url cookie = null producer <&> setSecureFlag cookie `bool` cookie
  where producer = getRulesetsMatching url >-> (() <$ havingCookieRulesThatTrigger cookie)
