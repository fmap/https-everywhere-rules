[https-everywhere-rules](https://github.com/fmap/https-everywhere-rules)
========================================================================

Haskell package providing high-level access to [HTTPS Everywhere][1]
rulesets. It aims to make it easier to specify: "use secure HTTP
connections when possible."

  [1]: https://www.eff.org/https-everywhere

```haskell
λ: :m + Data.HTTPSEverywhere.Rules Network.URI
λ: let Just eff = parseURI "http://www.eff.org/document/eff-and-aclu-amicus-brief-klayman"
λ: rewriteURL eff
https://www.eff.org/document/eff-and-aclu-amicus-brief-klayman
λ: :m + Network.HTTP.Client Network.HTTP.Client.TLS Control.Applicative Control.Lens
λ: (req, hub) <- (,) <*> getUri <$> parseUrl "https://github.com/fmap/https-everywhere-rules"
λ: newManager tlsManagerSettings >>= httpNoBody req <&> destroyCookieJar . responseCookieJar >>= mapM (rewriteCookie hub)
[Cookie {cookie_name = "_gh_sess", cookie_value = "eyJzZXNzaW9uX2lkIjoiNjBlM2FiOTIxNTdhZTNhNDE5YWQ0ZTk4ZWQzNDRjMjEiLCJzcHlfcmVwbyI6ImZtYXAvaHR0cHMtZXZlcnl3aGVyZS1ydWxlcyIsInNweV9yZXBvX2F0IjoxNDA4ODk2OTM2LCJfY3NyZl90b2tlbiI6IktkbTlwN2JqNGptVmhrYjFIUm9BbkV0a1JTQXRDUXJid2g4VWo4N1g0Q1U9In0%3D--d378daa262b8c12bb82246d5de6b3adc353a3db7", cookie_expiry_time = 3013-12-25 00:00:00 UTC, cookie_domain = "github.com", cookie_path = "/", cookie_creation_time = 2014-08-24 16:15:37.815144 UTC, cookie_last_access_time = 2014-08-24 16:15:37.815144 UTC, cookie_persistent = False, cookie_host_only = True, cookie_secure_only = True, cookie_http_only = True},Cookie {cookie_name = "logged_in", cookie_value = "no", cookie_expiry_time = 2034-08-24 16:15:36 UTC, cookie_domain = "github.com", cookie_path = "/", cookie_creation_time = 2014-08-24 16:15:37.815144 UTC, cookie_last_access_time = 2014-08-24 16:15:37.815144 UTC, cookie_persistent = True, cookie_host_only = False, cookie_secure_only = True, cookie_http_only = True}]
```

The rules database used here varies with the installed version of
[`https-everywhere-rules-raw`][2], which is versioned consistently with
upstream releases.

  [2]: https://github.com/fmap/https-everywhere-rules-raw
