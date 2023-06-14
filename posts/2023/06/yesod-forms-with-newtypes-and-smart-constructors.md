---
title: Yesod Forms, Newtypes, and Smart Constructors
date: 2023-06-14
location: Łódź, Poland
excerpt:
  Yesod form fields operate on primitive types, but you'll want to narrow those
  types into something more meaningful. Here are a couple of functions you'll
  need to achieve that.
tags: haskell
image: yesod-forms-newtypes-and-smart-constructors.jpg
---

Say we're writing a web application, and we're modelling a login form.

If the types in your system are primitive, you don't need to do much to parse
them from values outside your system boundary, _i.e._, those submitted by a
user through a web form.

It's easy enough to use a `textField` for each field in our login form.

```haskell
-- Assume this type synonym exists for all examples in this article
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- A login form with a single field
data LoginForm = LoginForm
  { loginFormEmail :: Text
  }

loginForm :: Form LoginForm
loginForm extra =  do
  email <- mreq textField "" Nothing
  pure (LoginForm <$> fst email, $(widgetFile "login"))
```

No surprises here.

## With Newtypes

What if our types aren't exactly `Text` values, but are some kind of
equivalent type? For example, what if we're representing our email value with a
newtype which wraps the underlying text value?

In Yesod, a `Field` cannot be a functor so it's not obvious how to reuse a
`textField` and make it produce an `Email` value instead.

Fortunately, Yesod's [form library][0] provides [`convertField`][1] to handle
this case. You apply this function to a couple of functions for converting to
and from your newtype, and a form field you wish to wrap.

```haskell
newtype Email = Email { unEmail :: Text }

data LoginForm = LoginForm
  { loginFormEmail :: Email
  }

loginForm :: Form LoginForm
loginForm extra =  do
  email <- mreq (convertField Email unEmail textField) "" Nothing
  pure (LoginForm <$> fst email, $(widgetFile "login"))
```

## With Smart Constructors

Newtype wrappers are better than working directly with primitive types, but by
themselves they don't provide a great deal of type safety because the wrapped
data isn't any more constrained than when it's unwrapped.

To solve that, we would reach for a [smart constructor][2].

```haskell
-- Explicitly exclude the value constructor
module Email (Email, unEmail, email) where

-- The newtype wrapper without a record field
newtype Email = Email Text

-- Unwrap the newtype
unEmail :: Email -> Text
unEmail (Email email) = email

-- The smart constructor
email :: Text -> Maybe Email
email t
  | "@" `isInfixOf` t = Just (Email t)
  | otherwise = Nothing
```

What if we're using the smart constructor pattern and our newtypes can't be
naïvely constructed? We can't use `convertField` because the types won't line
up.

Again, Yesod conveniently provides [`checkMMap`][3] for transforming some
existing field into one that both performs validation _and_ converts the
datatype. This way we can use our smart constructor in the field directly
instead of having to define validation rules in two places.

This function wants to ultimately produce an `Either msg b` but our smart
constructor only produces a `Maybe b`. We can use the [`note`][4] function to
promote it and provide a friendly error message.

```haskell
module Main where

import Email

data LoginForm = LoginForm
  { loginFormEmail :: Email
  }

loginForm :: Form LoginForm
loginForm extra = do
  email <-
    let msg = asText "Invalid email"
        checkEmail = pure . note msg . email
     in mreq (checkMMap checkEmail unEmail textField) "" Nothing
  pure (LoginForm <$> fst email, $(widgetFile "login"))
```

Since `checkMMap` runs in the `Handler` monad, you can also run IO actions or
database transactions as part of the validation step. For example, you could
query the database and check that the email address you're trying to log in
with actually exists.

If you want a little more assurance, it might be worth writing a property-based
test which asserts that your functions to convert to and from your newtype
successfully [roundtrip][5].

Yesod's form library is actually pretty powerful and satisying. It could
perhaps do with more examples of what good looks like, and hopefully this short
article helps. I've found that my code is generally neater when I'm able to
manage parsing/validation together at the web form level.

[0]: https://hackage.haskell.org/package/yesod-form
[1]: https://hackage.haskell.org/package/yesod-form-1.7.4/docs/Yesod-Form-Functions.html#v:convertField
[2]: https://kowainik.github.io/posts/haskell-mini-patterns#smart-constructor
[3]: https://hackage.haskell.org/package/yesod-form-1.7.4/docs/Yesod-Form-Functions.html#v:checkMMap
[4]: https://hackage.haskell.org/package/errors-2.3.0/docs/Control-Error-Util.html#v:note
[5]: https://hedgehog.qa/article/haskell-round-trip
