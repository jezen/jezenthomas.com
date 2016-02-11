---
title: Haskell Day One
date: 2014-08-08
description: Logical
excerpt: Exploring the syntax and ideas of Haskell and how it compares to other languages from different paradigms.
tags: haskell
---

And so, we come to the final chapter of *Seven Languages in Seven Weeks*. The
first day covered some of Haskell’s syntax, and the features it has in common
with the other functional languages in the book. As I so far understand it, the
idiomatic way of approaching problems in Haskell is very similar to the way
things are done in Prolog. The list comprehension syntax is almost identical to
Erlang. Haskell has syntactic sugar like Ruby’s ranges, and lazy evaluation
like Clojure.

<div id="toc"></div>

1. [Filter a list](#filter-a-list)
2. [Reverse a list](#reverse-a-list)
3. [Compute combinations](#compute-combinations)
4. [A childhood multiplication table](#a-childhood-multiplication-table)
5. [Thoughts](#thoughts)

### Filter a list

> How many different ways can you find to write `allEven`?

In the book, we’re provided with an interesting recursive solution to the
problem of taking a list of integers and returning a list with all the odd
numbers removed.

~~~haskell
module Main where
  allEven :: [Integer] -> [Integer]
  allEven [] = []
  allEven (h:t) = if even h then h:allEven t else allEven t
~~~

This approach strongly reminds me of the way problems are typically solved in
Prolog. Perhaps there’s a direct influence there. While interesting, it’s not
the approach I’d intuitively reach for — though my brain seems not to be wired
for clever recursive stuff yet.

Haskell does Erlang-style list comprehensions which have filters built in. Here
we take each number in our list and plug it into our new list if the remainder
is zero when dividing the number by two.

~~~haskell
[ x | x <- [1,2,3,4,5], mod x 2 == 0 ]
~~~

As I dig deeper with my Google-fu, I learn that Haskell provides a delicious
function called `filter` that takes a condition and a list, returning a list
with the elements that satisfy the condition. It’s so conversational, you could
almost mistake it for Ruby.

*Haskell cultists: Your death-threats will not scare me; I am half-Polish.*

~~~haskell
filter even [1..5]
~~~

### Reverse a list

> Write a function that takes a list and returns the same list in reverse.

Haskell provides a `reverse` function that works predictably, so implementing
one of my own seems a pointless exercise. Nevertheless, we can fashion a
Prolog-style recursive solution.

~~~haskell
module Main where
  backwards [] = []
  backwards (h:t) = backwards t ++ [h]
~~~

### Compute combinations

> Write a function that builds two-tuples with all possible combinations of two
> of the colours black, white, blue, yellow, and red.

 My example here is taken almost verbatim from the book. The function takes a
list `l` and binds its elements to `a` and `b` in all possible combinations. We
then remove duplicates by filtering where `a` is not equal to `b`.

~~~haskell
module Main where
  combinations l = [(a,b) | a <- l, b <- l, a /= b]
~~~


### A childhood multiplication table

> Write a list comprehension to build a childhood multiplication table. The
> table would be a list of of three-tuples where the first two are integers
> from 1-12 and the third is the product of the first two.

Yet another problem suspiciously well-suited to Haskell. We bind a couple of
ranges to `a` and `b` and return a three-tuple consisting of `a`, `b`, and the
product of the two.

~~~haskell
[(a, b, a*b) | a <- [1..12], b <- [1..12]]
~~~

### Thoughts

So far I’m enjoying Haskell and I think my initial anxiety was unjustified.
It’s a powerful tool and makes short work of complex problems, though this
first set of challenges are probably not comprehensive enough to reflect that.
I’m struggling most with Haskell’s type system; writing an appropriate type
definition for the `backwards` function had me stumped for a while, so I
discarded it and left the types to be inferred by the compiler.

One of the problems from this day of study involved implementing a depth-first
graph traversal algorithm. I didn’t study computer science at university; I
went to music school. For that reason, I’ll save this challenge for another
day.

My concern is that Haskell will be awkward when I need to accomplish everyday
things like accumulate state and do I/O. Even Simon Peyton Jones — one of the
creators of Haskell — has said that the language is [“safe, but useless”](https://www.youtube.com/watch?v=iSmkqocn0oQ).

<a class="previous-post" href="/seven-languages/clojure-day-three">« Clojure: <i>An Eye for Evil</i></a>
<a class="next-post" href="/seven-languages/haskell-day-two">Haskell: <i>Spock’s Great Strength</i> »</a>
