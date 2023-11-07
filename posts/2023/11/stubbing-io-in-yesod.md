---
title: Stubbing I/O in Yesod
date: 2023-11-05
location: Kaş, Türkiye
excerpt:
  An easy way to stub I/O actions in Yesod request handler code with dependency
  injection.
tags: haskell
image: stubbing-io-in-yesod.jpg
---

Here's the scenario.

You're writing a web application in Yesod. In one of your request handler
functions, you need to run some IO action. This might be to make an HTTP
request against an online weather service, or this might be to charge someone's
credit card, or even just to generate some random number.

Taking the latter as an example, imagine we want to generate a random number
and then respond to the user's request by reporting whether the randomly
generated number is even or odd.

We might write code which looks like this.

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Application where

import System.Random
import Yesod

data App = App

mkYesod "App" [parseRoutes|
/random RandomR GET
|]

instance Yesod App

getRandomR :: Handler Value
getRandomR = do
  n <- liftIO randomNumber
  returnJson $ isEven n
  where
    randomNumber :: IO Int
    randomNumber = randomRIO (1, 100)

    isEven :: Int -> String
    isEven n = if even n then "even" else "odd"

main :: IO ()
main = warp 3000 App
```

This is a complete Yesod application. We can run this locally and it will be
listening for requests on port 3000. When we send requests there, we can see
our application dutifully responding with whether or not the randomly generated
number was even or odd.

```sh
$ curl http://localhost:3000/random
"even"

$ curl http://localhost:3000/random
"odd"
```

This is all well and good, but how do we write an automated test for this? We
can't control the randomness of our pseudo-random number generator. Similarly,
if instead of generating a random number this were an HTTP request to attempt
to charge someone's credit card with some payment provider, _e.g._, Stripe,
then we wouldn't be able to write a reliable automated test for this because we
don't control Stripe's servers.

What we need to do is to _stub_ out this IO action. This means that instead of
running the real implementation during the test, we swap it out for a fake
version that we _can_ control.

One simple approach for this is with _dependency injection_.

Instead of defining our `randomNumber` function alongside our request handler,
we can declare it as part of our application's foundational data type.

```haskell
data App = App
  { appRandomNumber :: IO Int
  }
```

When we initialise our application, we construct our `App` value with the real
implementation of our function.

```haskell
main :: IO ()
main = warp 3000 $ App randomNumber
  where
    randomNumber = -- real implementation
```

Since our `randomNumber` function is no longer defined alongside our request
handler, we'll now need to ask for that function from within the handler
instead.

```haskell
getRandomR :: Handler Value
getRandomR = do
  n <- liftIO =<< getsYesod appRandomNumber
  returnJson $ isEven n
  where
    isEven :: Int -> String
    isEven n = -- …
```

This behaves exactly as it did before, but now we're able to swap out our
`randomNumber` function for a fake version in an automated test with
`testModifySite`.

```haskell
withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
  pure (App randomNumber, id)

stub :: YesodDispatch a => (a -> a) -> YesodExample a ()
stub f = testModifySite (\app -> pure (f app, id))

spec :: Spec
spec = withApp $ do

  describe "GET /random" $ do

    it "works with even numbers" $ do
      stub (\a -> a { appRandomNumber = pure 66 })
      get RandomR
      statusIs 200
      bodyEquals "\"even\""

    it "works with odd numbers" $ do
      stub (\a -> a { appRandomNumber = pure 17 })
      get RandomR
      statusIs 200
      bodyEquals "\"odd\""
```

Of course, the usual warnings apply. There are problems that come along with
stubbing out functions — if your stub doesn't accurately reflect what that
function _actually_ does, then your test is only giving you false confidence.

People often say that stubbing is bad and that you shouldn't do it. I don't
think this advice is useful. Yes, your tests and application logic should be
pure as far as you can help it. But sometimes you really _do_ need a stub.

A working example of this approach is [available here][0].

[0]: https://github.com/jezen/yesod-stubbing-example
