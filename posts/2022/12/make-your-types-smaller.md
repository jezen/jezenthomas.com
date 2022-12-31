---
title: Make Your Types Smaller
date: 2022-12-31
location: Kraków, Poland
excerpt:
  Persistent models tend to be where primitive obsession creeps in. Make your
  types more specific so you avoid paying the cost of having to sprinkle
  validation code throughout your system.
image: make-your-types-smaller.jpg
tags: haskell, minimalism
---

Dragons lie at the boundaries of systems.

But where those boundaries lie is too often in a software developer's blind spot.

A clear system boundary in a web application is a form. We know not to trust
user input, and so we diligently validate — both client-side and server-side —
the user's submission.

Once the submission has crossed _that_ threshold however, I think we lower our guard.

The database also exists beyond a system boundary. Programming language
peculiarities should probably not leak into the database, and vice versa.

It is generally understood how to use techniques such as [smart
constructors][0] to ensure a value floating through the system is always valid,
but I think _when_ to use a technique like this is something that programmers
have difficulty developing an intuition for.

It may be because of the _adjacency_ of the database conceptually to the data
types that model the values the database will eventually contain; but I have
noticed — across several open-source projects — that [primitive obsession][1]
creeps in when defining persistent models.

It's not an unreasonable thought pattern: you know you want to model a `User`
in your system. Your `User` model will be persisted in the `users` table. Your
`User` has a `name` field, which will be persisted in a column with a `VARCHAR`
(or `TEXT`) type.

So you define your persistent model this way:

```
User
  name Text
  dateOfBirth Day
  email Text
  UniqueEmail email
  -- etc…
```

The problem of course is that the [domain][2] for the `name` field (and also
the email field) is much larger than we want. Even taking into account
[falsehoods programmers believe about names][3], a name is not just _any_ text
value. We need to enforce some rules. We need to reduce our problem space. We
need to _make it smaller_.

For example, we wouldn't want a user with an empty string for a name:

```haskell
User
  { name = ""
  , dateOfBirth = -- …
  -- etc…
```

We also wouldn't want _this_ monstrosity[^1]:

```haskell
User
  { name = "V̥̝̣̤͇̮̣̦ͮͬ̇͌̕͟l̲̩̠̬͆ͪ͒͌̿ͧ̅͊͘a̷̙̾́̐͌̀ͥ̂̅͝ḓͤͣ̅͂̂ͩ̆͡ò̲̙͙̗̳̻̠̀l̥̮͈̫̻̤̞̿͛ͧ̄͒͋̅̂ͩ͘f̸̮̩̫̺̾͊̌̌ͫ̀͟ͅ ̥ͪ͋͞P̟̻̝͕̩͎̞ͭ̾ͧ͗̆̉u̶͍̱̭͎̓͋̓͂͗ͧͯ͡ͅͅt̸̯̜̟̥̋ͬͦ͂͆͘͟l̯͉͉̤ͣe̱̟̮̖̋ͦ̌͒͂͆ͪ͌͘r͚͛̒͗̔͊̚͘"
  , dateOfBirth = -- …
  -- etc…
```

It's all very well telling ourselves that this wouldn't happen to us because we
are running a comprehensive validation function when processing the form submission
that ingests this data, but the reality is that in a non-trivial business your
database is going to have more than one entry point. Expediency and technical debt
are facts of life.

A persistent model with a bunch of fields representing primitive values like
`Text` is a code smell. When we see it, it's telling us that we should _make
our types smaller_. That is to say, more specific.

Perhaps what we want instead is something like this:

```haskell
[st|
  -- Our User type with more specific types in its fields
  User
    name Username -- This type is smaller!
    dateOfBirth Day
    email Email
    UniqueEmail email
    -- etc…
|]

-- Introduce a Username type which wraps a text value
-- Keep this in a different module, and be sure not to expose the constructor!
newtype Username  = Username { unUsername :: Text }

-- The "smart constructor" which enforces validation rules
-- Only expose this one!
mkUsername :: Text -> Maybe Username
mkUsername t
  | t == "" = Nothing
  | failsSomeOtherValidationRule t = Nothing
  | otherwise = Just (Username t)

-- Teach our program to marshal values over the system boundary
instance PersistField Username where
  toPersistValue = -- unwrap value
  fromPersistValue = -- parse value into narrower type
```

It may seem more expensive to use a more specific type because you then need to
take the time to teach your program how to marshal values across that
application/database boundary, but I think this one-time cost is cheaper than
having to code defensively in perpetuity.

[0]: https://wiki.haskell.org/Smart_constructors
[1]: https://wiki.c2.com/?PrimitiveObsession
[2]: https://en.wikibooks.org/wiki/A-level_Computing/AQA/Paper_2/Fundamentals_of_functional_programming/Basics_of_functional_programming
[3]: https://www.kalzumeus.com/2010/06/17/falsehoods-programmers-believe-about-names/

[^1]: Fun Fact: For quite a long time, Twitter happily accepted Zalgo input in tweets, allowing anyone to turn the timelines of other users into an incomprehensible mess.
