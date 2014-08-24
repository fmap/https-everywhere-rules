[https-everywhere-rules](https://github.com/fmap/https-everywhere-rules)
========================================================================

![](https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTqn7bFPbRaxk-XU6PWLnbTvo7MtjGMFQw4RA4SZ0u23vzUS7AD5Q)

Haskell package providing high-level access to [HTTPS Everywhere][1]
rulesets. This makes it easier to tell other programs: "I'd like if you
used secure HTTP connections when possible."

```haskell
λ: :m + Data.HTTPSEverywhere.Rules Network.URI
λ: let Just eff = parseURI "http://www.eff.org/document/eff-and-aclu-amicus-brief-klayman"
λ: rewriteURL eff
Just https://www.eff.org/document/eff-and-aclu-amicus-brief-klayman
```

  [1]: https://www.eff.org/https-everywhere
