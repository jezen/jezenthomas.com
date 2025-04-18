---
title: Test Spies in Haskell
date: 2025-04-18
location: Sopot, Poland
excerpt:
    Learn how to spy on function calls in Yesod tests using a stubbed
    dependency and a bit of mutable state.
tags: haskell
image: test-spies-in-haskell/cover.jpg
---

When testing a web application, you often want to make sure that a certain
email _would_ be sent — without actually sending it. How do you test that?

Take something like a transactional email: a user signs up, resets their
password, or completes a purchase — and your application needs to send a
notification. You don’t care how the email gets sent — just that it was
triggered correctly.

The simplest implementation for the email-sending function might look like this.

```haskell
sendEmail :: Recipient -> IO ()
sendEmail recipient =
  -- some email sending code here…
```

All this function needs to know is where to send the email. How it sends it
isn’t important here — and it’s not what we’re testing. What we care about is
how it’s called.

This function might be called from a request handler, and it would be that
handler function's responsibility to decide how to call the email sending
function.

```haskell
postThingR :: Handler ()
postThingR = do
  liftIO $ sendEmail "user@example.com"
```

We can track how a function is called with a [_test spy_][0].

> Spies are stubs that also record some information based on how they were called.

You can emulate a test spy using the same [stubbing method][1] I wrote about
earlier. Instead of calling `sendEmail` directly, you'll retrieve it from
somewhere that can be swapped out at runtime — like your application's
settings.

```haskell
postThingR :: Handler ()
postThingR = do
  sendEmail <- getsYesod appSendEmail
  liftIO $ sendEmail "user@example.com"
```

The idea is to swap the implementation for one that records the arguments that
the function was called with, and then check what was recorded in the test's
assertion phase. We can use mutable state — such as an `IORef` or `TVar` — to
record how the function was called. Here, we’ll keep it simple with `IORef`.

```haskell
it "notifies the right person" $ do

  -- Arrange
  callsRef <- liftIO $ newIORef []
  stub $ \app -> app {
    appSendEmail = \recipient ->
      liftIO $ modifyIORef' callsRef $ \cs -> recipient : cs
    }

  -- Act
  post ThingR

  -- Assert
  liftIO $ do
    calls <- readIORef callsRef
    calls `shouldBe` ["user@example.com"]
```

This test swaps in a fake email function, triggers the handler, and checks that
the expected recipient was recorded.

With this pattern, you can test side effects without relying on real services —
making your tests fast, isolated, and reliable.

[0]: https://martinfowler.com/bliki/TestDouble.html
[1]: /2023/11/stubbing-io-in-yesod/
