---
title: Derived Instances Can Break Smart Constructors, Too
date: 2023-06-09
location: Kraków, Poland
excerpt:
  Smart constructors help guard against invalid data flowing through your
  system and prevent the need for more pervasive defensive coding tactics. You
  just need to be aware of the language details which can circumvent the
  protection they provide.
tags: haskell
image: relational.jpg
---

You know that _primitive obsession_ is an anti-pattern.

```haskell
-- Bad, because it's "stringly typed"
validPassword :: Text -> Bool
validPassword password = -- …
```

We're using a `Text` value to represent a user's password. This is bad because
there are so many possible `Text` values which would not be valid passwords.

For example, `"letmein"` is too short to be a valid password.

Ideally, you want to parse values into narrower types at the boundaries of your
system. This saves you from having to program defensively throughout your
codebase.

To this end, you introduce a new type which wraps a text value and models the
concept of a password.

```haskell
newtype Password = Password { unPassword :: Text }

validPassword :: Password -> Bool
validPassword password = -- …
```

This is better, but it's still pretty weak. We've introduced a different name
in the type signature so it's harder to confuse this value for some other
text value. The problem is that you can construct a `Password` value from
literally _any_ text value. This leads to invalid values floating through your
system.

We can fix that by writing a [smart constructor][0].

```haskell
-- Careful what we export from this module. Hide that constructor.
module Model.Password
  ( Password    -- abstract, hiding the constructor
  , unPassword  -- unwrap a password
  , password    -- only way to build a password
  ) where

-- Our type wraps a Text value. No record field here!
newtype Password = Password Text

-- Unwrap a 'Password'
unPassword :: Password -> Text
unPassword (Password p) = p

-- Try to construct a 'Password'
password :: Text -> Either Text Password
password t
  | length t < 8    = Left "Password is too short"
  | length t > 64   = Left "Password is too long"
  | t == "password" = Left "Password is too predictable"
  | otherwise       = Right (Password t)
```

This module uses explicit exports, because it's important that the value
constructor for `Password` remains internal to this module. The only way the we
can construct a `Password` value outside of this module is by applying the
exposed `password` function.

The `newtype` declaration does not have a record field, because [record fields
break smart constructors][1]. Instead, we introduce another simple function for
unwrapping the `newtype` to get to the text value underneath[^1].

The `password` function guards against a few invalid cases and allows us to
construct a valid `Password` value[^2]. This is what people mean when they say
_correct by construction_.

This is a solid improvement over the _stringly typed_ approach that we started
with, but there's another potential pitfall that I ran into while refactoring
some code recently.

In a web application you often want to serialise and deserialise values between
different representations, _e.g._, JSON, XML, URI path pieces, _etc._ This is
typically done with typeclasses — you have a typeclass called `FromJSON` with a
polymorphic method which parses some JSON into some other type, and then you
write an _instance_ of that typeclass for each type that you'd like to apply
that parsing function to.

When you have a `newtype` which wraps some primitive text value, it can be
tempting to ask the compiler to _derive_ the `FromJSON` instance using the
`GeneralizedNewtypeDeriving` language extension. It's less code to write and
maintain, and in many cases you will indeed want a typeclass instance that is
the same as the instance of the underlying type.

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

newtype Password = Password Text
  deriving FromJSON -- here's your problem
```

When you're using smart constructors however, this will lead to invalid data
flowing through your system because like record fields, this is also a way to
circumvent the validation performed by the smart constructor.

Potentially adding to the confusion, we're not protected by our careful use of
module exports here. Typeclass instances in Haskell are always exported and
imported between modules!

Fixing this is simple — just manually write the instance.

```haskell
{-# LANGUAGE LambdaCase #-}

newtype Password = Password Text

instance FromJSON Password where
  parseJSON = \case
    (String p) ->
      case password p of
        Left err ->
          fail $ "Could not parse Password: " <> unpack p <> "; " <> err <> "."
        Right pass' -> pure pass'
    _ -> fail "Could not parse Password - was not a String"
```

Despite these two pitfalls, smart constructors are a good return on investment.
Just be sure you don't accidentally introduce ways to circumvent the protection
that they provide.


[^1]: You probably never want to do this with a _password_, but you often need
      to unwrap a value like this to do something with the underlying value, like
      print it on the screen (or, a web page).
[^2]: You could also have this produce a `Maybe Password`, but the extra
      context around why validation can fail here is interesting.

[0]: https://kowainik.github.io/posts/haskell-mini-patterns#smart-constructor
[1]: https://taylor.fausak.me/2018/03/16/record-fields-break-smart-constructors/
