---
title: A 10x Speedup in GHCi
date: 2025-03-07
location: Koh Samui, Thailand
excerpt:
    Running tests in GHCi can be slow due to single-threaded execution, but
    enabling multi-core parallelism can significantly improve performance.
tags: haskell
image: 10x-speedup-in-ghci/cover.jpg
---

Many of us at Supercede use GHCi directly when developing Haskell projects. It
runs the local development server, and the automated test suite, and serves as
a scratch pad for ad hoc expression evaluation and type inspection.

We make heavy use of integrated tests with the yesod-test framework.

One of our projects now has more than 500 tests.

Running the tests in GHCi takes about 150 seconds. Not terrible, but not great
either. Certainly not fast enough to encourage developers to practice TDD and
run the entire suite regularly.

```
Finished in 150.2305 seconds
508 examples, 0 failures, 3 pending
```

However, running the same test suite with `cabal test`[^1] takes only about
12 seconds.

That's a significant improvement!

```
Finished in 11.7001 seconds
508 examples, 0 failures, 3 pending
```

As it turns out, the difference is in concurrency in the runtime system. The
project's cabal file includes some GHC options which enable multi-core
parallelism and allows the program to run with all available CPU cores.

```
test-suite test
  type:               exitcode-stdio-1.0
  main-is:            Main.hs
  hs-source-dirs:     test
  ghc-options:
    -threaded -rtsopts -with-rtsopts=-N
    -- there are more options here, but they're unrelated
```

So the test suite runs slowly in GHCi, because the tests are running on a
single thread. It's possible to start GHCi with multi-core parallelism by
running `ghci +RTS -N` from the shell, but this is fiddly to write, and I don't
think writing a shell alias is the right thing to do. The project's development
environment should _just work_ for everyone who contributes to the project, and
they shouldn't need to create shell aliases or tinker with these inputs in
order to enjoy the improved performance.

Fortunately, it's possible to change RTS settings at runtime, so I've added
these lines to end of the project's `.ghci` file.

```haskell
import GHC.Conc
n <- getNumProcessors
setNumCapabilities (max 1 (n - 1))
```

First we use `getNumProcessors` to get the number of CPUs the machine has,
and then we set the number of Haskell threads that can run simultaneously.
As recommended in [the documentation][0], we leave a core free to avoid
contention with other processes.

Running the same test suite in GHCi with this RTS configuration takes ~12-20
seconds, which is on average about a 10x improvement. There will also be some
differences with GHCi interpreting rather than compiling code, but I think at
this scale the difference is insignificant.

While this isn't related to parallelism, the last thing we do in our `.ghci`
file is to enable the display of timing and memory stats, plus the inferred
type of the variable bound for each statement.

```
:set +s +t
```

We do this last, because otherwise when entering GHCi we'll see timing and
memory stats for every comment written in the `.ghci` file, _i.e._,
several lines like this:

```
(0.00 secs, 0 bytes)
(0.00 secs, 0 bytes)
(0.00 secs, 0 bytes)
(0.00 secs, 0 bytes)
(0.00 secs, 0 bytes)
(0.00 secs, 0 bytes)
```

---

One fairly obvious way to not have to think about any of this would be to use
`cabal repl` which is essentially a managed way to launch GHCi, though that
comes with its own configuration concerns. I like the quick startup time of
GHCi, and I like having a deeper understanding of what the interpreter is or
isn't doing.

I've been happily using GHCi for the past decade, and I'm not currently
convinced that adding a wrapper around the interpreter would make my
development workflow any simpler. There is an argument to be made for cabal's
dependency resolution, but I'm using Nix to manage packages anyway.

[^1]: We actually run our checks with `nix flake check -L`, but this uses cabal under the hood.

[0]: https://hackage.haskell.org/package/base-4.21.0.0/docs/Control-Concurrent.html#v:setNumCapabilities
