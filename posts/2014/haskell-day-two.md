---
title: Haskell Day Two
date: 2014-08-23
description: Spock’s Great Strength
excerpt: Exploring the syntax and ideas of Haskell and how it compares to other languages from different paradigms.
tags: haskell
---

The second day of Haskell studies revisits functional programming concepts that
we’ve seen in other languages already. What I’m finding interesting is that the
previous functional languages give you some breathing room, whereas Haskell
forces you into a purely functional mindset.

For example: every Haskell function has only one parameter. It may look as
though functions can have multiple parameters, but what’s happening behind the
scenes is that each parameter is being split into its own partially-applied
function. Every multiple-parameter function in Haskell is *curried*. Mr. Spock
nods in approval.

<div id="toc"></div>

1. [Sorting lists](#sorting-lists)
2. [Parsing numbers](#parsing-numbers)
3. [Function composition and lazy evaluation](#function-composition-and-lazy-evaluation)
4. [Currying](#currying)
5. [Thoughts](#thoughts)

### Sorting lists

Unsurprisingly, Haskell comes with list-sorting functions out of the box. You
can fire up GHCI and try:

~~~haskell
Prelude> Data.List.sort [2,3,1]
[1,2,3]
~~~

> Write a sort that takes a list and returns a sorted list.

I have limited knowledge of sorting algorithms, and bubblesort is the only one
I could explain or express in code. I spent some time trying to implement a
bubblesort in Haskell, but I’m fairly certain you need to store some
intermediate values *somewhere*, which is one of those things that Haskell just
doesn’t do.

I found an implementation on [Rosetta
Code](http://rosettacode.org/wiki/Sorting_algorithms/Bubble_sort#Haskell) which
I’ll paste in. Do I understand how it works? Yes, kind of, but not well enough
to explain it to a six year old. It doesn’t look very much like what I had come
up with on my own, so I feel that I’m kind of being thrown in at the deep end
here.

~~~haskell
bsort :: Ord a => [a] -> [a]
bsort s = case _bsort s of
               t | t == s    -> t
                 | otherwise -> bsort t
  where _bsort (x:x2:xs) | x > x2    = x2:(_bsort (x:xs))
                         | otherwise = x:(_bsort (x2:xs))
        _bsort s = s
~~~

I read/heard somewhere — I don’t remember where — that functional programming
is naturally more intuitive because it allows you to solve problems at a higher
level of abstraction than OOP. Call me naïve, but I don’t think of implementing
sorting algorithms as a high-level problem. Perhaps the value in this exercise
is in emphasising the idea that different languages are better suited to
certain types of problems. If I really wanted to come close to the machine and
think about sorting algorithms, I would probably reach for a tool like C
instead.

### Parsing numbers

> Write a Haskell function to convert a string to a number. The string should
> be in the form of $2,345,678.99 and can possibly have leading zeros.

I began this problem late at night; short on patience and with an abundance of
sarcasm. I wrote a function that takes exactly the described string, and
returns exactly that value as a floating point number.

~~~haskell
module Main where
  parseFloat :: String -> Float
  parseFloat "$2,345,678.99" = 2345678.99
~~~

I’m not a Star Trek fan, but my understanding of Mr. Spock is that he lacks a
sense of humour. The language-to-film-character simile couldn’t have been
stronger here because instead of acknowleding that my function was
light-hearted and poorly-written, I was given back *exactly* what I asked of
the compiler — degree of accuracy included.

~~~haskell
*Main> parseFloat "$2,345,678.99"
2345679.0
~~~

When I remove the type signature, Haskell does actually return me the value I
expect. This makes me ask myself “Why do I try to tell the compiler what I want
when it always knows better than me?”

Try again. My Google-fu helped me find a way to cast a string of numbers to an
actual number with `read <value> :: <type>`, and a way to `filter` unwanted
characters from a string.

~~~haskell
module Main where
  import Data.Char
  parseCurrency str =
    read (filter (\c -> isNumber c || '.' == c) str) :: Double
~~~

My naïve function works for numeric strings in one format. To make it more
robust, I could add a pattern match that returns a condescending message to the
user for unexpected input. Then, naturally:

~~~bash
git commit -am "I’m off to the bar"
git push --force origin master
~~~

### Function composition and lazy evaluation

> Write a function that takes an argument `x` and returns a lazy sequence that
> has every third number, starting with `x`. Then, write a function that
> includes every fifth number, beginning with `y`. Combine these functions
> through composition to return every eighth number, beginning with `x + y`.

Haskell’s lazy sequences are in the form of a recursive list-build, which I
find quite elegant. The book provides an example which gives me a good starting
point for the first two functions.

~~~haskell
module Main where
  everyThird x = x:(everyThird (x + 3))
  everyFifth y = y:(everyFifth (y + 5))
~~~

Each of these functions calls itself and recursively appends the input plus
three or five to a list starting with the input. If for whatever reason I
actually wanted a function that provides a lazy sequence in steps of eight, I
would follow the above form and add eight to the input. This defeats the
purpose of the exercise, so I went away and thought about how this could work.

My `everyThird` and `everyFifth` functions are expecting integers as input, but
they return lists. This means I can’t simply pipe the output of one function to
the input of another. I would have to somehow extract an integer from the list
it produces.

If my `everyEighth` function is to start from `1`, I’m going to want to add
three to it with the `everyThird` function. I extract a list of two numbers
from `everyThird`, and then take one which has had the addition applied.

~~~haskell
last (take 2 (everyThird 1))
~~~

Sure enough, this returns `4`. We can nest this as a parameter we pass to
`everyFifth`, and then do the same trick to extract the result. Here’s the
entire function with nesting:

~~~haskell
everyEighth z =
  z:(everyEighth
      (last
        (take 2
          (everyThird
            (last
              (take 2
                (everyFifth z)))))))
~~~

Next time someone bitches about lisp languages having too many parentheses, show them that.

Haskell’s function composition — at least, as I understand it — is some
syntactic sugar we can use to make deeply nested function calls easier to read.
Here’s how the function looks when we flatten it out with the dot operator:

~~~haskell
everyEighth z =
  z:(everyEighth
    . last
    . take 2
    . everyThird
    . last
    . take 2
    . everyFifth $ z)
~~~

This is an improvement, but after asking some Haskell developers on IRC, I
learned we can improve this further with the list index operator (`!!`). List
are zero-indexed, so I’m accessing index `1`:

~~~haskell
everyEighth z =
  z:(everyEighth
    . (!!1)
    . everyThird
    . (!!1)
    . everyFifth $ z)
~~~

### Currying

> Use a partially applied function to define a function that will return half
> of a number and another that will append `\n` to the end of any string.

Writing partially-applied functions is trivial in Haskell. In fact, if you look
behind the scenes it turns out pretty much everything is done with
partially-applied functions.

~~~haskell
module Main where
  half = (/ 2)
  newline = (++ "\n")
~~~

### Thoughts

I remember reading that Haskell’s learning curve is not steeper than other
languages, it’s just longer. I can see that. I’m enjoying the constraint of
having to write code in a purely functional style; much more than writing
hybrid code with Scala. Perhaps my mind will change when I come to doing IO in
Haskell. Perhaps not.

<a class="previous-post" href="/seven-languages/haskell-day-one">« Haskell: <i>Logical</i></a>
<a class="next-post" href="/seven-languages/haskell-day-three">Haskell: <i>The Mind Meld</i> »</a>
