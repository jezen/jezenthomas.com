---
title: Implementing a Content Security Policy in Yesod
date: 2019-09-21
location: Gdynia, Poland
tags: haskell
banner: bar.jpg
---

If you use Mozilla's [Observatory][0] website security scanner, you'll quickly
find you won't get an A+ grade without correctly implementing a _Content
Security Policy_.

A <abbrev title="Content Security Policy">CSP</abbrev> is just a HTTP response
header, so you'd think it would be trivial to add. Unfortunately, that isn't
the case. There are currently three different CSP versions, and as usual the
different browser vendors can't agree on how this feature should be implemented
or even what _name_ to use for the header.

It also doesn't help that documentation you might assume to be authoritative is
sometimes [misleading][1]. In fact, following various tutorials on the Internet
— many of which are contradictory — initially lead me to implement a CSP using
host-based whitelisting. The benefit of this approach is that you can just
hardcode your whitelist, and implement your CSP in nginx without touching your
application. There are a number of drawbacks though; not only are host-based
whitelists cumbersome to maintain, they're also generally insecure as evidenced
in a [Google research paper][2]. The short story is that it's [better to use a
nonce-based approach][3].

This presents its own set of problems though. For a nonce to be effective, it
must be:

- Encoded in Base64
- Randomly generated on every request
- Present in both the CSP header _and_ the HTML tag of the script to which
  you're granting execution permission

The first two constraints are trivial to solve — we can use the `uuid` and
`base64-bytestring` libraries to produce our nonces.

```haskell
import           ClassyPrelude.Yesod
import qualified Data.ByteString.Base64 as B64
import           Data.UUID              (toASCIIBytes)
import           Data.UUID.V4           (nextRandom)

generateRequestNonce :: MonadHandler m => m Text
generateRequestNonce = do
  uuid <- liftIO nextRandom
  return $ decodeUtf8 $ B64.encode $ toASCIIBytes uuid
```

The `nextRandom` function generates our random value. The quality of the
randomness of this value doesn't really matter, so we _could_ have used
`randomIO` from the `System.Random` module here. The reason I decided against
using that function is that it's quite slow. There are other libraries
available for generating random values with good performance, but I'm already
using UUIDV4 values in the rest of my application.

The `toASCIIBytes` function turns our random `UUID` value into a `ByteString`,
which is the type that our Base64 encoding function expects. Finally, we use
`decodeUtf8` to turn our bytestring into a `Text` value, as this is the
specific string-like type that Yesod's HTML tag and response header functions
expect.

Now that we have our nonce, how do we shoehorn it into the script tags we wish
to whitelist? Typically when writing an application in Yesod, you'd use the
`addScript` function to generate the script tags and insert them in the
document. This won't work though, because the nonce should be inserted into its
own HTML attribute in that script tag. Luckily, Yesod provides us a way to
generate a script tag with arbitrary additional attributes.

```haskell
getHomeR :: Handler Html
getHomeR = do
  nonce <- generateRequestNonce
  defaultLayout do
    addScriptAttrs (StaticR js_jquery_js) [("nonce", nonce)]
    $(widgetFile "home")
```

This works, but the ergonomics aren't great and it adds quite a lot of noise.
Fortunately Haskell makes it easy to abstract away this kind of thing, so we
can write our own small helper functions to clean this up. The same principle
applies whether we're adding scripts we host ourselves, or scripts hosted
elsewhere.

```haskell
addScriptCSP :: MonadWidget m => Route (HandlerSite m) -> m ()
addScriptCSP route = do
  nonce <- generateRequestNonce
  addScriptAttrs route [("nonce", unCSPNonce nonce)]

addScriptRemoteCSP :: MonadWidget m => Text -> m ()
addScriptRemoteCSP uri = do
  nonce <- generateRequestNonce
  addScriptRemoteAttrs uri [("nonce", nonce)]

addScriptEitherCSP :: MonadWidget m => Either (Route (HandlerSite m)) Text -> m ()
addScriptEitherCSP = either addScriptCSP addScriptRemoteCSP

getHomeR :: Handler Html
getHomeR = defaultLayout do
  addScriptCSP $ StaticR js_jquery_js
  addScriptRemoteCSP "https://cdn.ywxi.net/js/1.js"
  $(widgetFile "home")
```

This takes care of all the JavaScript libraries we wish to import, but there is
one more crucial script tag left to cover. When we compose widgets together
which contain some Julius (Shakespeare-flavoured JavaScript) code, Yesod joins
those disparate bits of widget-specific code together into a file typically
called `autogen-xxxxxxxx.js`, where those `x`s are [I think] the hash of the
file.

This is done in the core of the Yesod framework, so you can't directly override
this behaviour. As of [`yesod-core-1.6.16`][4] however, there's a new typeclass
method that allows us to add arbitrary attributes to that generated tag too. We
can use this method together with the nonce generator function we wrote
earlier.

```haskell
instance Yesod App where

  jsAttributesHandler = do
    nonce <- generateRequestNonce
    return [("nonce", nonce)]

  -- Here you would have your own implementations for
  -- approot, yesodMiddleware, defaultLayout, etc.
```

Now that we have our nonce in all our HTML script tags, we also need to stuff
it into the `script-src` directive of our CSP response header. We _could_ use
the `addHeader` function in every handler in our application, but that would be
cumbersome to maintain and adds unnecessary noise. Instead, we can add this
response header to _every_ request from just one place using a middleware.

```haskell
instance Yesod App where

  yesodMiddleware =
    defaultYesodMiddleware . addCSPMiddleware
```

The middleware function that we want to write will need to know the value of
the nonce we generated earlier in the request lifecycle. Usually when you want
to store some state in the application memory, you'd add a new field to the
`App` record, initialise it when the application starts up, and manipulate the
value it holds from your handler functions. In our case however, we don't
actually want the value to exist outside the lifecycle of a single request so
this is a great opportunity to take advantage of Yesod's per-request caching.

```haskell
cspCommon :: [Text]
cspCommon =
  [ "img-src 'self' data:"
  , "object-src 'none'"
  , "form-action 'self'"
  , "frame-ancestors 'none'"
  , "base-uri 'self'"
  , "report-uri /report-csp"
  ]

csp :: Maybe Text -> Text
csp mNonce =
  intercalate ";" $ cspCommon <> [unwords scriptSrc]
  where mkNonce n = "'nonce-" <> n <> "'"
        scriptSrc =
          [ "script-src"
          , "'self'"
          , "about:"
          , "'strict-dynamic'"
          ] <> maybe [] ((:[]) . mkNonce) mNonce

addCSPMiddleware :: (HandlerFor m) a -> (HandlerFor m) a
addCSPMiddleware handler = do
  res   <- handler
  nonce <- cacheGet
  addHeader "Content-Security-Policy" $ csp nonce
  return res
```

How this works is rather clever. You might be curious about the `cacheGet`
function. How does it know exactly what to fetch from the cache? We didn't even
tell it which key to use for the lookup! Well, the per-request cache stores
data by _type_, and Haskell can _infer_ the type of cached value we're
interested in because we've passed our cached `nonce` value to the `csp`
function which expects a `Maybe Text` argument.

Since values are cached by type, it logically follows that we can only cache
_one_ value for any given type. If we wanted to cache some other `Text` value,
we'd be stuck. The way around this is to wrap our `Text` value with a
`newtype`.

```haskell
newtype CSPNonce = CSPNonce { unCSPNonce :: Text }
  deriving (Eq, Ord)
```

Making this change necessitates we also change the functions we wrote earlier.
Our generator will not only have to create a value of the correct type, but it
will also need to add that value to the per-request cache. Since this function
will potentially be called multiple times during a single request, we should
make it idempotent by having it first check the cache, and using the `CSPNonce`
value if it already exists. Again, type inference is working here (notice we
specified the type in the function's type signature) to magically select the
correct cache key.

```haskell
generateRequestNonce :: MonadHandler m => m CSPNonce
generateRequestNonce = do
  mNonce <- cacheGet
  case mNonce of
    Just nonce -> return nonce
    Nothing -> do
      uuid <- liftIO nextRandom
      let nonce = mkNonce uuid
      cacheSet nonce
      return nonce
  where
    mkNonce =
      CSPNonce . decodeUtf8 . B64.encode . toASCIIBytes
```

I won't show the final implementations of the other functions, because it's
mostly a case changing `Text` to `CSPNonce` and unwrapping that newtype in a
couple of places. If you're following along at home, you can just let the
compiler guide you to the end. There are a couple of final steps for a more
complete implementation, but these do nothing to further explain the mechanics
of the above approach and are left as an exercise for the reader. They pertain
to CSP not working correctly in [Mobile] Safari. More specifically:

- Safari uses a non-standard header name. Use the `ua-parse` library to
  determine the user's browser and set the header name appropriately.
- Safari does not support the `'strict-dynamic'` directive, so you need
  host-based whitelisting anyway. Use another newtype and cache a set of
  `CSPHost` values.

When you do implement your own CSP, be sure to check your syntax with Google's
[CSP evaluator][5], and also make good use of the `report-uri` directive so you
can see if some of your content is unintentionally blocked for some users in
production.

[0]: https://observatory.mozilla.org
[1]: https://github.com/Fyrd/caniuse/issues/5092
[2]: https://static.googleusercontent.com/media/research.google.com/en//pubs/archive/45542.pdf
[3]: https://websec.be/blog/cspstrictdynamic/
[4]: https://github.com/yesodweb/yesod/pull/1622
[5]: https://csp-evaluator.withgoogle.com/
