---
title: When Test Coverage Isn't Enough
date: 2025-05-23
location: Sibiu, Romania
excerpt:
  Test‑coverage metrics can hit 100 %, yet critical bugs still slip through.
  Property‑based testing and type‑driven design show why – and what to do about
  it
tags: haskell
image: when-test-coverage-is-not-enough/cover.jpg
---

Robert Roskam made a point on social media about the inadequacy of test
coverage.

> 100% test coverage is not enough. Here's an example as to why in Python:
>
> ```python
> def division(x, y):
>  return x / y
> ```
>
> If you pass in `0` for `y`, it'll raise an error. You have to know to test for
> the value specifically. Test coverage won't save you here. With 100% line
> coverage or branch coverages, it won't be found. That's how 100% test
> coverage is not enough.

So, if test coverage is not enough, what can we do here?

Well, at least a couple of things.

## Surfacing Failure Cases Automatically

It wouldn't be sensible to try testing our division function against every
possible input, because the set of possible inputs is effectively unbounded.
Fortunately, we don't have to. We can instead use a property-based testing
library like [hedgehog][] to generate inputs to our division function and show
us the minimal way to reproduce an error when it occurs.

This example is in Haskell, and only accounts for integral numbers. Otherwise,
it's a near enough direct translation of the Python version.

```haskell
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

division x y = x `div` y

prop_division_identity = property $ do
  x <- forAll $ Gen.int Range.linearBounded
  y <- forAll $ Gen.int Range.linearBounded
  let q = division x y -- quotient
      r = x `mod` y    -- remainder
  q * y + r === x
```

We're relying on the **integer division law** to test our function.

The integer division law says:

> When you divide one whole number by another, you get a **quotient** and a
> **remainder**. If you multiply the quotient by the divisor, then add the
> remainder, you get back the original number.

This law holds for `divisor ≠ 0`.

So, the test generates a range of integers, applies the division function to
them, and checks that the result always adheres to the integer division law.

Running this in GHCi quickly surfaces the problem.

```
━━━ Main ━━━
  ✗ prop_division_identity failed at div.hs:15:13
    after 1 test.
    shrink path: 1:

       ┏━━ div.hs ━━━
    10 ┃ prop_division_identity = property $ do
    11 ┃   x <- forAll $ Gen.int Range.linearBounded
       ┃   │ 0
    12 ┃   y <- forAll $ Gen.int Range.linearBounded
       ┃   │ 0
    13 ┃   let q = division x y -- quotient
    14 ┃       r = x `mod` y    -- remainder
    15 ┃   q * y + r === x
       ┃   ^^^^^^^^^^^^^^^
       ┃   │ ━━━ Exception (ArithException) ━━━
       ┃   │ divide by zero
```

In this case, we didn't have to explicitly check what happens when the divisor
is zero.

Furthermore — and perhaps more importantly — writing this test has forced us to
_think_ about both the types of the inputs and the range of those inputs. I
didn't want to have to specify the upper and lower bounds for the range of
inputs, but not all numeric types have explicit bounds. That led me to choose
to constrain my `division` function to only support integers.

We can see that limitation directly in GHCi.

```
λ minBound :: Int
-9223372036854775808

λ minBound :: Float

<interactive>:49:1: error: [GHC-39999]
    • No instance for ‘Bounded Float’ arising from a use of ‘minBound’
    • In the expression: minBound :: Float
      In an equation for ‘it’: it = minBound :: Float
```

This also guides us towards thinking about edge cases. If we have to consider
division on floating point numbers, then the obvious challenge is going to be
in specifying to what degree of precision we expect to see in our results. We
would then have to change the assertion in the test so that it accepts a result
to a specific acceptable degree of accuracy.

Again, the test encourages us to _think_, which in turn leads to better
design. That's true whether you're writing a small function like `division`, or
a larger, integrated path through your system. This is why TDD is often thought
of as Test Driven **Design**, rather than just Test Driven Development.

## Making Failure Impossible

Now that we've surfaced the error, how do we design our function to make this
kind of failure impossible?

One common approach is to express the failure case in the output. This
typically means wrapping the output in a Maybe to represent that the result may
not exist.

```haskell
division :: Int -> Int -> Maybe Int
division x 0 = Nothing
division x y = Just (x `div` y)
```

This works, but it necessarily implies that the caller of this code will have
to handle the possibility of failure. This change will then ripple through the
rest of the codebase, which is noise that you probably don't need.

Alternatively, you can use the [smart constructor][] pattern to constrain the
_input_ and have the changes ripple back towards the boundary of your system,
which is usually a better design choice. In the case of our `division`
function, this would mean ensuring at compile time that the function will only
accept a non-zero divisor.

```haskell
division :: Int -> NonZero Int -> Int
division x (NonZero y) = x `div` y
```

Finally, we could of course [return zero in case of failure][0]. Which of these
options is the right one is debatable, but the point is that both the tests and
the types have driven us to think about the design of our system.

---

Heads up! I'm writing a tool to validate HTML at scale. [Check it out][htmlqa].

[hedgehog]: https://hackage.haskell.org/package/hedgehog
[smart constructor]: https://kowainik.github.io/posts/haskell-mini-patterns
[0]: https://www.hillelwayne.com/post/divide-by-zero/
[htmlqa]: /2025/05/dont-skip-html-validation/
