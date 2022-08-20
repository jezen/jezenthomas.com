---
title: Solving a Maths Riddle with Bad Haskell
date: 2022-05-03
location: Kraków, Poland
tags: haskell
excerpt:
  Sometimes you need to get some quick and dirty scripting work done. Sometimes
  Haskell is an excellent tool for this.
image: numbers.jpg
---

At a previous company off-site, a colleague shared a mathematical brain teaser
with the team:

> With the numbers `123456789`, make them add up to `100`. They must stay in
> the same order but you can use addition, subtraction, multiplication,
> division, brackets etc. All numbers must be used exactly once.

I'm admittedly not great at arithmetic, and I didn't make much progress when
trying to brute-force a solution in my head. Naturally I decided to reach for
my big hammer: Haskell.

The first idea I had to attack this problem was to enumerate all possible
expressions you would have as a result of combining the different permitted
mathematical operators, slotted between each of the digits.

Whenever I need to generate all the combinations of things, I usually reach for
Haskell's list comprehensions. For example, if you wanted to generate all
possible pairs of the numbers 1, 2, and 3, you might write the following list
comprehension:

```haskell
[ (x, y) | x <- [1..3], y <- [1..3] ]

-- evaluates to:
--   [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]
```

Applying this approach to the maths riddle is essentially the same:

```haskell
let ops = [ '+', '-', '/', '*' ]
 in [ [ '1', a, '2', b, '3', c, '4', d, '5', e, '6', f, '7', g, '8', h, '9' ]
      | a <- ops, b <- ops
      , c <- ops, d <- ops
      , e <- ops, f <- ops
      , g <- ops, h <- ops
      ]
```

Each of the digits and mathematical operators are represented as character
values, because Haskell lists are _homogenous_ — all values must be of the same
type. The `let` binding cleans up what would otherwise be quite a bit of
tedious repetition.

Evaluating this expression generates a whole bunch of mathematical expressions
encoded in strings — 65,536 expressions to be exact. The expressions look like
this:

```haskell
[ "1*2*3*4/5-6-7*8-9"
, "1*2*3*4/5-6-7*8/9"
, "1*2*3*4/5-6-7*8*9"
, "1*2*3*4/5-6/7+8+9"
-- etc.
```

Now that we have a collection of expressions, we need to evaluate each of them
and find those which evaluate to `100`. Evaluating string values as code is
something that even a decade ago in the JavaScript world would have been
frowned upon. JavaScript provides the `eval()` function for this, but
conventional wisdom states that `eval()` is _evil_, and should _never_ be used.

I beg to differ.

But even still, `eval()` is just this gross hacky thing that only exists in
languages like JavaScript, right? Surely you can't do the same thing in a pure,
ivory-tower language like Haskell, right?! As it turns out, you can!

If you add the [`hint`][0] package, you can do something like this:

```haskell
import Language.Haskell.Interpreter

f = runInterpreter $ do
  setImports ["Prelude"]
  eval "3 + 5" -- evaluates to `Right "8"`
```

Pretty cool, if not somewhat heretical.

Essentially all that's left to do is plug the 65,536 mathematical expression
strings into the machinery that evaluates them, and filter the results to only
those where the result is `100`. Here's what I came up with:

```haskell
import Control.Monad (forM)
import Language.Haskell.Interpreter

expressions :: [String]
expressions =
  let ops = [ '+', '-', '/', '*' ]
   in [ [ '1', a, '2', b, '3', c, '4', d, '5', e, '6', f, '7', g, '8', h, '9' ]
        | a <- ops, b <- ops
        , c <- ops, d <- ops
        , e <- ops, f <- ops
        , g <- ops, h <- ops
        ]

result = runInterpreter $ do
  setImports ["Prelude"]
  exprs <- forM expressions evaluate
  pure $ filter (\(_, a) -> a == "100") $ fromRight [] exprs
  where
  evaluate expr = eval expr >>= \a -> pure (expr, a)
```

The above is an expanded version of what I originally wrote. When I was playing
with this, I actually wrote it as a one-liner directly in GHCi, which is a
similar experience to composing a Unix command line.

Running this produces 14 possible answers, and that's without enumerating all
those possible answers that would result from changing the order of operations
with brackets.

\$$ 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 * 9 \$$
\$$ 1 + 2 + 3 - 4 * 5 + 6 * 7 + 8 * 9 \$$
\$$ 1 + 2 - 3 * 4 + 5 * 6 + 7 + 8 * 9 \$$
\$$ 1 + 2 - 3 * 4 - 5 + 6 * 7 + 8 * 9 \$$
\$$ 1 + 2 * 3 + 4 * 5 - 6 + 7 + 8 * 9 \$$
\$$ 1 - 2 + 3 * 4 * 5 + 6 * 7 + 8 - 9 \$$
\$$ 1 - 2 + 3 * 4 * 5 - 6 + 7 * 8 - 9 \$$
\$$ 1 - 2 * 3 + 4 * 5 + 6 + 7 + 8 * 9 \$$
\$$ 1 - 2 * 3 - 4 + 5 * 6 + 7 + 8 * 9 \$$
\$$ 1 - 2 * 3 - 4 - 5 + 6 * 7 + 8 * 9 \$$
\$$ 1 * 2 * 3 + 4 + 5 + 6 + 7 + 8 * 9 \$$
\$$ 1 * 2 * 3 - 4 * 5 + 6 * 7 + 8 * 9 \$$
\$$ 1 * 2 * 3 * 4 + 5 + 6 + 7 * 8 + 9 \$$
\$$ 1 * 2 * 3 * 4 + 5 + 6 - 7 + 8 * 9 \$$

This is how my machine looks like while generating answers. It's a good way to
make the other customers in Starbucks feel uneasy.

![Generating thousands of mathematical expressions in GHCi.](/static/img/numbers.gif)

Is my approach an efficient way to find the possible answers? No. Do I care
enough to optimise my approach? Also no.

Who said Haskell can't do quick and dirty scripting work?

After submitting my answers, my colleague rejected my approach because I "used
a program", and "that's cheating."

ಠ_ಠ

[0]: https://hackage.haskell.org/package/hint
