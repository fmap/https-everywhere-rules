module Data.HTTPSEverywhere.Rules (
  rewriteURL,
  rewriteCookie
) where

import Prelude hiding (null, head)
import Control.Lens ((<&>),(&))
import Data.Bool (bool)
import Network.HTTP.Client (Cookie)
import Network.URI (URI)
import Pipes ((>->))
import Pipes.Prelude (head, null)
import Control.Monad (join)
import Data.HTTPSEverywhere.Rules.Internal (getRulesetsMatching, havingRulesThatTrigger, havingCookieRulesThatTrigger, setSecureFlag)

rewriteURL :: URI -> IO (Maybe URI)
rewriteURL url = getRulesetsMatching url >-> havingRulesThatTrigger url & head <&> join

rewriteCookie :: URI -> Cookie -> IO Cookie
rewriteCookie url cookie = null producer <&> setSecureFlag cookie `bool` cookie
  where producer = getRulesetsMatching url >-> havingCookieRulesThatTrigger cookie
