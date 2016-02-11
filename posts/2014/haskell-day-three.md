---
title: Haskell Day Three
date: 2014-09-01
description: The Mind Meld
excerpt: Exploring the syntax and ideas of Haskell and how it compares to other languages from different paradigms.
tags: haskell
---

The final phase of Haskell studies felt less like going out with a bang, and
more like collapsing over a dusty keyboard. The author took a deeper look at
the language’s type system and ways to extend it with user-defined types.
Classes were also lightly covered (which are unlike OO classes and more similar
to Clojure protocols or Java interfaces). Monads were the primary focus, which
“allow different kinds of computational strategies”. Don’t ask me what that
means.

I mostly skim-read these final few pages of the book. It felt as though monads
were posed as the final hurdle; if you can understand monads, then
congratulations. You can now pat yourself on the back, close the book, and go
home.

<div id="toc"></div>

1. [Monads](#monads)
2. [Thoughts](#thoughts)

### Monads

The explanation of monads in *Seven Languages in Seven Weeks* could have been
better; I only vaguely understood what they are and what they’re for after my
first read-through. The first self-study task in the book is to find a few
monad tutorials, which lead me to read James Coglan’s article on [implementing
monads in
JavaScript](https://blog.jcoglan.com/2011/03/05/translation-from-haskell-to-javascript-of-selected-portions-of-the-best-introduction-to-monads-ive-ever-read/).
At a high-level, monads are a mechanism to make functions composable. Coglan
summarises this better than I can:

> “So what is a monad? Well, it’s a design pattern. It says that whenever you
> have a class of functions that accept one type of thing and return another
> type of thing, there are two functions that can be applied across this class
> to make them composable.”

I’m confident that plain-English explanations like that coupled with a few
simple examples will convert more of the OOP congregation to The Church of
Functional Programming. Excuse me while I repeat that for the
academically-inclined:

*__Dear Functional-Programming zealots:__ We love the enthusiasm for the
paradigm, but overstating the complexity of monads does not help us normal folk
learn how to use them.*

Here’s the description of the first challenge:

> Write a function that looks up a hash table value that uses the `Maybe`
> monad. Write a hash that stores other hashes, several levels deep. Use the
> `Maybe` monad to retrieve an element for a hash key several levels deep.

I wasn’t quite sure where to begin with this; perhaps I should define a new
tree-type structure and a function that returns either a leaf of the tree, or
*maybe nothing*. I didn’t manage to get anything to compile, and when I
compared my attempt to what someone else had written, I figured I was way
off-base.

If I have to lookup key values in Haskell one day, I know the language already
has this feature built in. In any case, here’s a working implementation from
[Frédéric Dumont’s excellent
blog](http://blog.wakatta.jp/blog/2011/11/19/seven-languages-in-seven-weeks-haskell-day-3/).

~~~haskell
module Lookup where

my_lookup key [] = Nothing
my_lookup key ((k, value):rest)
  | key == k  = Just value
  | otherwise = lookup key rest

testData = [(1, []), (2, [("a", [("i", "tada!")]), ("b", [("j", "nope")])]), (3, [("c", [("k", "tada!")])])]
~~~

Frédéric also managed to provide answers to the other challenges too, and I
think they’re out of my league for now. Perhaps this is something I ought to
return to on a rainy day.

### Thoughts

To support my Haskell studies, I attended a Haskell meetup here in Göteborg to
hear some talks from great minds, and to meet people who use Haskell often or
even professionally. While I enjoyed my evening, I’m somewhat less convinced
that Haskell is a practical tool for building software. The idea of referential
transparency and the absense of side-effects pleases my obsessive-compulsion,
but I don’t believe it’s practical in the real world.

When it comes time again to dig deeper into functional programming languages, I
think my focus will be on Clojure or perhaps some other lisp. I’m keeping my
mind open to be shown otherwise.

<a class="previous-post" href="/seven-languages/haskell-day-two">« Haskell: <i>Spock’s Great Strength</i></a>
