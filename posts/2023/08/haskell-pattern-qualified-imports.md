---
title: "Haskell Pattern: Design for Qualified Imports"
date: 2023-08-17
location: Kraków, Poland
excerpt:
  A simple yet thoughtful and robust convention for naming library functions in
  Haskell.
tags: haskell
image: haskell-pattern-design-for-qualified-imports.jpg
---

When you're writing library code, design for qualified imports.

Let's say we're writing a module which is concerned with parsing email
addresses. Inside this `Email` module, a function named `parseEmail` would be
carrying more context in its name than is necessary. The name `parse` would be
sufficient.

When applying our parsing function in some _other_ module, the name needs to be
more descriptive so that it is more obvious what kind of thing the parsing
function parses.

We also might need to disambiguate this parsing function from another parsing
function that may be in scope.

✘ `Email.parseEmail` is redundant.

✘ `parseEmail` makes it hard to know where the function is defined.

✔ `Email.parse` is easy to read.

✔ `Email.parse` makes it clear what kind of thing we're parsing.

✔ `Email.parse` makes it clear where that function is defined.


```haskell
import Acme.Email qualified as Email -- exports `parse`, not `parseEmail`

main :: IO ()
main = print $ Email.parse "foo@bar.com"
```

The module describes the context we're in. Sometimes we need to make
that context clearer, and sometimes the context is already clear enough.
