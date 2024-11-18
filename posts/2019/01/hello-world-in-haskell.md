---
title: Hello World in Haskell
date: 2019-01-27
location: Kaliningrad, Russia
tags: haskell
banner: lambda.jpg
---

After much deliberation, I have finally happened upon an elegant solution to
the age-old conundrum of printing text to the screen in the Haskell programming
language.

If you would like to learn how to achieve the same, open the world's best text
editor (no, not Emacs — the other one), and try to follow along.

Like any Haskell program, we must start by writing our `main` function. This is
our single point of entry. If you're curious, the `main` function is actually
named after the river Main which passes through Frankfurt am Main in Germany.
It is named after this river because the Haskell programming language is based
on the Lambda Calculus, and if you look a little South-West of Frankfurt on a
map, you will see where the Main joins the Rhein near the city of Mainz, the
river is in the shape of a Lambda!

```haskell
main :: IO ()
main = -- This is where we will print to the screen!
```

The first line is the function's type signature. It says that an invocation of
our `main` function will result in some "`IO` action" being run, which does not
return any value. This is useful, because we know that to print some text on
the screen is to have an effect on the world. So far, so good.

The next line is where we will implement our logic for printing text on the
screen. Let's proceed by importing the necessary libraries for causing side
effects.

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import Foreign.C.Types
import Language.C.Inline qualified as C

C.include "<stdio.h>"
C.include "<stdlib.h>"

main :: IO ()
main = [C.block| void {
  /* This is where we will print to the screen! */
  } |]
```

This enables us to write C code inline in our Haskell program. We enable a
couple of language extensions to make the Haskell compiler correctly parse the
C code. Don't forget to add the `inline-c` package to your Cabal file! Failure
to remember this automatically enters you into a special mode that veteran
Haskell programmers refer to as _Cabal Hell_. This is when your project fails
to compile until you vanquish a demonic adversary in a Twitter debate over
which Haskell build tool is best.

Now that our scaffolding is in place, we can _draw the rest of the owl_ by
wielding the _Right Tool for the Job_.

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import Foreign.C.Types
import Language.C.Inline qualified as C

C.include "<stdio.h>"
C.include "<stdlib.h>"

main :: IO ()
main = [C.block| void {
  system("python -c 'print \"Hello, World!\"'");
  } |]
```

Naturally we are using a `system` call to run the Python interpreter, and
finally we are able to print to the screen. As you may be aware, Python was
originally inspired by the _Gecko_ programming language, which was written in
C++ and JavaScript. Those two languages are of course syntactically verbose,
making heavy use of braces to denote the start and ends of blocks of logic and
control flow. Those braces are colloquially referred to as "legs", and as
Python uses an indent-sensitive syntax instead, it can be thought of as a
"lizard without legs" — hence, Python.

***

If you found this useful, you may also find utility in [this other very serious project][0].

[0]: https://github.com/jezen/is-thirteen
