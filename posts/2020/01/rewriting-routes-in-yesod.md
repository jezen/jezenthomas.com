---
title: Rewriting Routes in Yesod
date: 2020-01-22
location: Kaliningrad, Russia
excerpt:
  Routes that are clear and comprehensible to the user are fundamental to good
  web application design. With a bit of care, we can tidy up some routes that
  would typically be noisy in a Yesod application.
tags: haskell
banner: writing.jpg
---

User authentication in Yesod typically works through a plugin system, also
known as a _subsite_. This is handy because it means your web application can
support multiple methods of authentication simultaneously and you can also swap
out one system for another relatively painlessly.

One drawback of this system however is the routes end up looking quite verbose.
For example, a user of your website might expect the login route to be located
at `/login`. With the plugin system however, that page will be located at
`/auth/page/foo/login`, where `foo` is the name of the plugin the user intends
to use.

Since Yesod runs on [WAI][wai], we can solve the problem of verbose routes by
rewriting them with a WAI middleware. This is pretty straightforward and of
course works for more than just authentication routes, though they make for a
good example.

The first step is pulling in the right library. The functions we want to use
are in [wai-extra][wai-extra], so add that to your cabal file if it isn't
already there. If you used a template like [yesod-postgres][template] to start
your project, you likely already have this package available.

Next we need to override the way Yesod renders routes for us. The Yesod
typeclass exposes a method called [`urlParamRenderOverride`][override]. We can
use this to translate a typesafe route into whatever other string
representation we need, while also retaining any query string parameters passed
along.

The example below changes the way auth routes from the
[yesod-auth-simple][plugin] auth plugin are rendered. Your own implementation
may slightly differ.

```haskell
instance Yesod App where

  -- other typeclass method overrides

  urlParamRenderOverride y r _ =
    let root = fromMaybe "" (appRoot (appSettings y))
        toRoute p = Just $ uncurry (joinPath y root) (p, [])
     in case r of
          (AuthR LoginR)               -> toRoute ["login"]
          (AuthR LogoutR)              -> toRoute ["logout"]
          (AuthR (PluginR "simple" p)) -> toRoute p
          _                            -> Nothing

  -- yet more custom stuff
```

Finally, we want to intercept user requests to the shortened paths and expand
them to their full form under the hood. This is where our WAI middleware comes
in. Pay special attention to the final pattern match of the `rw` function — you
want all other routes in your application to pass through unaltered.

```haskell
import Network.Wai.Middleware.Rewrite (rewritePureWithQueries)

rewriteAuthRoutes :: Middleware
rewriteAuthRoutes = rewritePureWithQueries rw
  where
    plugin :: [Text]
    plugin = ["auth", "page", "simple"]

    rw :: PathsAndQueries -> RequestHeaders -> PathsAndQueries
    rw (["register"], _) _                  = (plugin <> ["register"], [])
    rw (["confirm", token], _) _            = (plugin <> ["confirm", token], [])
    rw (["confirmation-email-sent"], _) _   = (plugin <> ["confirmation-email-sent"], [])
    rw (["login"], _) _                     = (plugin <> ["login"], [])
    rw (["reset-password"], _) _            = (plugin <> ["reset-password"], [])
    rw (["reset-password-email-sent"], _) _ = (plugin <> ["reset-password-email-sent"], [])
    rw (["logout"], _) _                    = (["auth", "logout"], [])
    rw (path, qs) _                         = (path, qs)

makeApplication :: App -> IO Application
makeApplication foundation = do
  logWare  <- makeLogWare foundation
  appPlain <- toWaiAppPlain foundation
  initState foundation
  let middlewares = defaultMiddlewaresNoLogging
                >>> logWare
                >>> rewriteAuthRoutes
  return $ middlewares appPlain
```

Connect your custom WAI middleware to our middleware chain when building the
application, and everything should work as you expect.


[wai]: https://hackage.haskell.org/package/wai
[wai-extra]: https://hackage.haskell.org/package/wai-extra
[template]: https://github.com/commercialhaskell/stack-templates/blob/master/yesod-postgres.hsfiles
[override]: http://hackage.haskell.org/package/yesod-core-1.6.17/docs/Yesod-Core.html#v:urlParamRenderOverride
[plugin]: https://github.com/riskbook/yesod-auth-simple
