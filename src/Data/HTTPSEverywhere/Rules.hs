module Data.HTTPSEverywhere.Rules (
  rewriteURL,
  rewriteCookie
) where

import Data.Text (Text)
import Network.HTTP.Client (Cookie)

import Data.HTTPSEverywhere.Rules.Internal ()

rewriteURL :: Text -> IO Text
rewriteURL = undefined

rewriteCookie :: Cookie -> IO Cookie
rewriteCookie = undefined
