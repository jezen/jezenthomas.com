---
title: Tracing User Requests in Yesod
date: 2019-07-09
location: Gdynia, Poland
tags: haskell
banner: crowd.jpg
---

Recently I decided I want to invest in my Yesod application's logging system.
Yesod provides sane defaults for logging using the Apache style and this is
enough for most applications, however in my case I had a few extra demands.
Namely, the logs should:

- Contain an authenticated user's ID so we have full visibility into every
  user's behaviour
- Generate a unique ID for each request, so requests can be discussed
  internally
- Show the request's total duration, which will help us find performance
  bottlenecks
- Be formatted in JSON for ease of parsing with other tools

Most of these needs could be addressed with functionality the relevant
libraries included in a typical scaffolded Yesod site already provide. One of
my needs necessitated a little more work. More on that in a moment.

First, let's take a look at how logging is typically set up in a scaffolded
Yesod site. For our purposes, all we need to care about is what we define as
our `outputFormat`. The value we use here should be different depending on
whether you're running the application in development or production — unless of
course you _want_ to see JSON logs in development.

```haskell
-- ./src/Application.hs

makeLogWare :: App -> IO Middleware
makeLogWare foundation =
  mkRequestLogger def
    { outputFormat =
      if appDetailedRequestLogging $ appSettings foundation
        then Detailed True
        else Apache $
               if appIpFromHeader $ appSettings foundation
               then FromFallback
               else FromSocket
    , destination = Logger $ loggerSet $ appLogger foundation
    }
```

Yesod applications are set to use detailed logging in development by default.
This format is easier to visually parse than either Apache or JSON logs, but
would be far too noisy in production. The `True` in `Detailed True` means we
want the logs to use colours. A more descriptive type here might have been
useful, but that's by the by. Here's how those detailed logs look (here without
colour):

```txt
GET /auth/page/simple/login
  Accept: text/html,application/xhtml+xml,application/xml;q…
  Status: 200 OK 0.007939s
GET /favicon.ico
  Accept: image/webp,image/apng,image/*,*/*;q=0.8
  Status: 200 OK 0.001512s
```

Ok, so that's just fine, but we're more interested now in the production logs.
We don't want the Apache format, so that's what we'll change. The available
data constructors can be found in the [wai-extra][0] package. You'll see
`Apache IPAddrSource` and `Detailed Bool`, both of which we're familiar with
from the scaffolding. There are a few others however with "custom output
format" in their names which are a little more useful.

The penultimate data constructor has an associated JSON formatter called
`formatAsJSON`. As it turns out, this combination of data constructor and
formatting function (and also a custom middleware, but that's internal)
includes the request duration. So that's two of my requirements taken care of!
I _believe_ logging the request duration is based on [this prior art][1] by
Maximilian Tagher — the implementation looks identical.

So if we have the means to provide a custom formatter, why don't we just add
the ID of an authenticated user there? Well, here's the thing. Request logging
happens at the WAI level, which is the outer layer of the system. To read the
authenticated user's ID we need to check against the authentication system, but
authentication happens at the Yesod level, which is an inner layer of the
system. So first, we need some way to pass arbitrary data from the inner Yesod
layer to the outer WAI layer. I think request and response headers are a
suitable mechanism for transferring this kind of data, and we can use the
[`addHeader`][2] function to easily add any response header we want. We also want
this response header to be applied to _every_ request, so we should use a
middleware to add this functionality to all handlers.

Adding Yesod middleware is trivial:

```haskell
-- ./src/Foundation.hs

addUserIdToResponseHeadersMiddleware :: Handler a -> Handler a
addUserIdToResponseHeadersMiddleware handler = do
  mUser <- maybeAuth
  let renderEntityKey = toStrict . decodeUtf8 . Data.Aeson.encode
  maybe (pure ()) (addHeader "User-ID" . renderEntityKey . entityKey) mUser
  handler
```

In this middleware — which is just a polymorphic handler which composes with
any other handler — we use `maybeAuth` to get a value of `Maybe (Entity User)`.
If the user is _not_ authenticated, this value will be `Nothing`, in which case
we _do_ nothing. That's the `pure ()` part. If the user _is_ authenticated, we
extract their ID from the Persistent `Entity` type, render it as JSON (and do
some mundane Haskell string munging), and add it to a response header with a
key of `User-ID`.

We also need to _apply_ our middleware, which is just a case of tacking it onto
whatever other middleware chain we already have.

```haskell
-- ./src/Foundation.hs

instance Yesod App where
  -- ...Other stuff is probably here

  yesodMiddleware = defaultYesodMiddleware
    >> addUserIdToResponseHeadersMiddleware

  -- ...More stuff
```

That's great, but there's a problem: How do we get these response headers in
our request log formatter? I went through the source, and unfortunately none of
the formatting middlewares actually provided the response headers to the log
formatter. I sent a pull request which adds another `OutputFormat` data
constructor, along with its associated custom middleware and log formatter to
the `wai-extra` package, and it was [merged earlier today][3]. So as of version
`3.0.27`, you can use this data constructor:

```haskell
type OutputFormatterWithDetailsAndHeaders
   = ZonedDate -- ^ When the log message was generated
  -> Request -- ^ The WAI request
  -> Status -- ^ HTTP status code
  -> Maybe Integer -- ^ Response size
  -> NominalDiffTime -- ^ Duration of the request
  -> [S8.ByteString] -- ^ The request body
  -> B.Builder -- ^ Raw response
  -> [Header] -- ^ The response headers
  -> LogStr
```

…together with the new `formatAsJSONWithHeaders` function. So our `makeLogWare`
function can now be changed to this:

```haskell
-- ./src/Application.hs

makeLogWare :: App -> IO Middleware
makeLogWare foundation =
  mkRequestLogger def
    { outputFormat =
      if appDetailedRequestLogging $ appSettings foundation
        then Detailed True
        else OutputFormatterWithDetailsAndHeaders formatAsJSONWithHeaders
    , destination = Logger $ loggerSet $ appLogger foundation
    }
```

This will now render all request logs in production as JSON, along with the
request duration and any response headers. It will also redact any request
header with a key of `Cookie`, or any response header with a key of
`Set-Cookie`.

Almost there. Now we just want to generated a unique ID for each request as
it's coming in, and stuff that ID in a request header. We can do this easily
enough at the outer WAI layer.

```haskell
-- ./src/Application.hs

addRequestId :: Middleware
addRequestId app req sendRes = do
  requestId <- UUID.toASCIIBytes <$> UUID.nextRandom
  let hs = ("Request-ID", requestId) : WAI.requestHeaders req
  app (req { WAI.requestHeaders = hs }) sendRes
```

The first line uses the `uuid` package to generate a UUIDv4, and then converts
it to a human-readable bytestring. We then use the record update syntax to
construct a new request value `req'` based on the old value `req`. The new
value just has the new header prepended to the list of existing request
headers. We then use the new `req'` value for the rest of the request
lifecycle.

As before, don't forget to add this new middleware to the appropriate chain.
This would be our WAI middleware chain, _not_ the Yesod middleware chain we
used earlier.

```haskell
-- ./src/Application.hs

makeApplication :: App -> IO Application
makeApplication foundation = do
  logWare  <- makeLogWare foundation
  appPlain <- toWaiAppPlain foundation
  return $ addRequestId $ logWare $ defaultMiddlewaresNoLogging appPlain
```

Go forth, and trace users.

[0]: http://hackage.haskell.org/package/wai-extra-3.0.27/docs/Network-Wai-Middleware-RequestLogger.html#t:OutputFormat
[1]: https://stackoverflow.com/a/26146218/704015
[2]: http://hackage.haskell.org/package/yesod-core-1.6.14/docs/Yesod-Core-Handler.html#v:addHeader
[3]: https://github.com/yesodweb/wai/pull/762
