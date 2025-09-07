---
title: Rich Hickey Doesn't Know Types
date: 2019-04-09
location: Sopot, Poland
excerpt:
  Typing preferences are one thing, but the implication that parametricity is
  effectively useless is just a bridge too far.
tags: clojure, haskell
banner: hickey.jpg
---

Clojure inventor Rich Hickey argues in one of his talks that:

> `a -> a`; `[a] -> [a]`; It means nothing! It tells you *nothing*!

 — Rich Hickey, [Effective Programs][effective_programs].

If we take him to mean "`a -> a`", this is actually one of the simplest ways to
understand parametricity.

At first glance, it looks like a function of type `a -> a` could do anything.
But parametricity rules that out. If you pass in a `1`, it can’t return `2` —
because the function must also work for *any other type*, not just numbers.
There’s no general “+1” operation across all types. Likewise, concatenation
only works for types with the right structure. In fact, the only lawful
implementation is the identity function:

```haskell
id :: a -> a
id x = x
```

The signature also tells us there are no effects involved: there’s no `IO`
present, so the function can’t read from or write to the outside world.

Now consider `[a] -> [a]`. Yes, technically there are infinitely many
implementations (`a`, `a ++ a`, `a ++ a ++ a`, …), but they’re still highly
constrained. You can reverse a list, drop elements, take elements — but you
cannot inspect or manufacture new `a` values. Parametricity tells us a
great deal.

A true shuffle would need a source of randomness, _e.g._:

```haskell
f :: Seed -> [a] -> [a] -- pass a seed
-- or
f :: [a] -> IO [a]      -- obtain randomness in IO
```

This is the opposite of “nothing”: the signatures rule out far more than they
permit, and those exclusions are exactly what make them informative.

As Kris Jenkins put it in [Communicating in Types][communicating_in_types]:

> When you have a type error — and you _always_ get a type error — the question
> is whether you get it from QA, or your users, or your compiler.

Type systems like Haskell's aren't a panacea, and there are _some_ things that
a language with a dynamic type system can do that are harder to achieve in
Haskell (though what this means in the context of a business is a separate
discussion). But dismissing these general types as uninformative overlooks the
guarantees parametricity provides.

[effective_programs]: https://youtu.be/2V1FtfBDsLU?t=4020
[communicating_in_types]: https://vimeo.com/302682323
