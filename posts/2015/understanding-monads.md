---
title: Understanding Monads
date: 2015-11-13
excerpt: Repeat after me&colon; A monad is not a burrito. A monad is not a burrito. A monad is…
tags: haskell
---

<span class="run-in"><span class="drop">Y</span>esterday I did not
understand</span> Monads, Functors, or Applicatives. Today I understand all
three. Not well enough to explain them to a six year old, but well enough to put
them to use in the code that I write.

I first encountered Monads about 14 months ago, while working through the
Haskell chapter of *Seven Languages in Seven Weeks*. The author presented Monads
as an alien concept, the comprehension of which is elusive to all but the
hairiest of neckbeards. I'd like to assert that this does not have to be the
case.

Why was I intimidated by the Monad? I can attribute the fear to a few reasons,
in no particular order:

## Hype

In the same way that companies adopt immature JavaScript libraries and
frameworks after their virtues are extolled by Hacker News pundits, software
people tend to overstate the terror of the Monad which is then internalised by
the impressionable people trying to learn functional programming.

Learning Monads, Functors, and Applicatives is a relatively big pill to swallow,
but it's not *that* hard. It does take some time, and I'm quite certain that any
*Learn Monads in 10 Minutes!* tutorial is going to have to gloss over many
important penny-drop moments for the sake of brevity. Sometimes undertaking a
large project (in this case learning a collection of new concepts) requires some
naïvety to prevent the fear of failure from manifesting itself as an inability
to start working. This leads me to my next point…

## Lack of context

The Monad tutorials I have read until today seem to mostly focus on the Monad,
and it's an unfair emphasis. After having overcome this hurdle myself, I can now
tell you that *of course* learning Monads is going to be difficult when you lack
the context of the *why* behind the *what*. In order to understand why we need
Monads, we first need to understand why we need Functors and Applicatives. In
all cases, the mechanics of each concept are not hugely complicated. Simple
explanations with examples of *why* each concept needs to exist is what is
sorely needed.

If you do not first understand Functors and Applicatives and what they are used
for, you will have very little of the context needed to learn what a Monad is
for.

## Poor use of language

After having bought into the terror of the Monad, some programmers proceed to
explain the concept through an overly simplified analogy, as if this somehow
compensates for the fear the pundits have instilled in us from the beginning.
Monads are sometimes described in terms of *things*, but they are not things. To
talk about Monads is to talk about context and behaviour. A Monad is not a
burrito.

---

How did I overcome these challenges and finally learn to understand the Monad? I
bought, downloaded, and read *Maybe Haskell* by Pat Brisbin on a long-haul
flight. Don't be put off by the fact my flight was long-haul; the book is short.
I'm just sleep-deprived and a slow reader.

It's important the book is short. A more traditional Haskell tutorial spends an
eternity presenting a million ways to manipulate lists, and I am bored to
indifference before ever coming near the chapters on Functors, Applicatives, and
Monads.

I don't know Brisbin personally, and I in no way benefit from praising his short
book so highly. The point I would like to carry across is that as a reader, I
want the story straight. It's unbearable to continue reading when an author is
dancing around the issue; constantly flirting and selling the benefits and
elegance of the way Haskell works. Brisbin's book gave it to me straight.

I'm driven to write all this because I am excited about what this new knowledge
arms with with. There are some powerful and totally uncomplicated ideas that
emerge from learning about `<$>` (fmap) and `<*>` (apply), the most prominent of
which is that functions should assume they are receiving valid inputs. Most of
the Ruby and JavaScript code I have written until now has been littered with
`nil` or `undefined` checks, and this defensive style of programming quickly
becomes a total mess. I have learned that the `nil` checks should be moved out
to the boundaries of the system; the data should be validated in *one* place.

Even if you don't care to learn Haskell, I implore you. Go read that book.
