---
title: My Technological Regrets
date: 2025-01-30
location: Đà Nẵng, Vietnam
excerpt:
    After several years working on Supercede, I’ve had time to reflect on some
    technical choices we made. Some of them worked well, others I’d approach
    differently if I were starting again. Here’s what I regret, and why.
tags: haskell
image: regrets/cover.jpg
---

After several years working on Supercede, I’ve had time to reflect on some of
the technical choices we made. Some of them worked well, others I’d approach
differently if I were starting again. Here’s what I regret, and why.

## Elm Was the Wrong Choice for Our UI

When I started working on Supercede, I had the idea that any more complex parts
of the UI should be written in Elm, rather than with React or perhaps just
managing all DOM interaction manually with plain JavaScript.

If you need to write in a Single Page Application (SPA) style, I think Elm is
probably the best possible choice. I would absolutely choose Elm if I were
writing a greenfield SPA today.

It doesn’t matter that Elm doesn’t have typeclasses, and it doesn’t matter that
Elm doesn’t have the same kind of churn that the rest of the JavaScript world
has come to expect. These are both features. I’m familiar with some of the
criticisms levelled at Elm, but chief, [this ain’t it][0].

The reasons why I have come to regret Elm are not inherent to Elm itself. The
problems are in product design, and what that necessarily implies about the
system design. As it turns out, Supercede just doesn’t [currently] need any of
the user interface written in SPA style. This is enterprise software for
reinsurance professionals. Most of the interactivity should be modelled with
server-rendered web forms. Kind of like how web applications were written 20
years ago.

If you design your application as a backend which serves some JSON and a SPA
which consumes and operates on that data, you have two separate systems. To
marshal values as they flow between the boundaries of those two systems, you
need to somehow manage the synchronisation of the types of those values on both
sides (and that’s true also of dynamically-typed languages), and you need to
manage how those values serialise and deserialise, and you probably need to
duplicate some of the business logic and parsing/validation logic. You might —
as we did at Supercede — introduce a code generation mechanism which runs as
part of your build step and enforces that all of these things remain
synchronised. But this is more code to compile. More to maintain. More to
document. More to constantly be aware of. It doesn’t come for free.

Alternatively, you could choose to not have these problems by designing your
product in a different way. This sounds like a constraint, and it is. But
that’s a good thing! Design is all about constraints. Constraints liberate. If
you can’t design within constraints, then you’re just a bad designer.

## Event Sourcing Added Complexity We Didn't Need

We knew from the beginning that auditability would be an important property of
our system. Event sourcing — where the current state of the system is derived
by replaying a chronological sequence of events — seems an obvious choice for
this.

The problem is that event sourcing is _hard_.

Sure, it’s not insurmountably hard. You can quickly gain a conceptual intuition
for how event sourcing should work when it’s explained to you, and you can
readily draw parallels to other systems which work more or less the same way,
like distributed version control, or your bank account.

But the complexity of designing and maintaining an event sourced system doesn’t
come for free. It’s not even cheap. When you want to introduce a change to the
system, you need to think much harder about threading that change through each
component of the event sourcing system. You need to think harder about when
effects happen. Should running a projection also send transactional email?
Probably not. Other effects? Hard to say.

You can learn the theory. You can learn the skills. But it’s _probably_ too
much to take on for a startup when they’re still trying to find product/market
fit. Your time is better spent building features that paying customers actually
care about. And most customers — especially for a product like Supercede —
really care about auditability. But they _don’t_ care whether that auditability
is implemented with event sourcing, and not, say, some audit tables.

There are cases where event sourcing is absolutely the right approach, and
should be coupled with deeper measures of integrity like [WORM drives][1], but
those cases are less common than the software conference circuit might have you
believe. Much like microservices, I suppose.

## Over-Reliance on PostgreSQL Slowed Development

This regret isn’t about PostgreSQL per se. It’s more about tightly coupling to
database-specific implementation details, and the consequences of such a
coupling.

One of the consequences is slower test execution. Most of Supercede’s test code
is written in an integrated style using the yesod-test library. Between each
test, we create a new application state and reset the database so that
individual tests do not affect the execution environment of other tests.
Truncations are slow in PostgreSQL, and for some specific design reasons it’s
not really feasible to wrap each test in a transaction which is rolled back at
the end[^1]. We have other projects which use the same libraries but instead
run tests against an in-memory SQLite database, which works great and runs
quickly.

Depending on features specific to PostgreSQL also means you need to run a real
database server in your continuous integration setup. It’s more to install, and
you need to run it all in a virtual machine. It’s just more _stuff_. And stuff
is a liability.

For most of the more generic database work, I would prefer to abstract away the
details of the database with libraries like persistent and esqueleto. There
might be parts of the application that necessarily rely on specific database
features, but I think those parts should be extracted or in some way isolated,
perhaps even so they can be stubbed in the general case.

## Lenses

Lenses are brilliant, and for any new non-trivial Elm project, I would likely
reach for a lens library like [elm-monocle][2]. But that’s because I think Elm
projects — at least in my experience — tend towards keeping all state in one
foundational data structure which will necessarily become complex as the
project grows.

Your foundational data structure — your `Model` — is not some peripheral detail
that you interact with only occasionally. Because it’s so central, you work
with it constantly and so the mental model for manipulating that data structure
with lenses becomes rudimentary, rather than a novel curiosity.

Lenses are so worth it if you have this design and the ergonomics constraints
it brings.

But if you don’t? I don’t think it’s worth it.

Lenses in Haskell read _quite_ differently from more conventionally written
Haskell code. So much so, that while being ordinary Haskell, you could almost
consider them a domain specific language.

Working with lenses and maintaining a flow state requires familiarity, which I
don’t think you can establish if you only flirt with lenses by lightly dusting
it around the periphery of your project.

At Supercede, we haven’t gone _all-in_ on lenses. Some engineers have made the
choice to lightly dust lenses on the periphery. Those parts now cause visible
lines in the skin of programmers’ faces any time that programmer is forced to
read and understand what we’ve written.

So, I regret our lens adoption. In hindsight, it seems gratuitous. Perhaps we
indulged in whether or not some code _could_ be written in terms of lenses,
while neglecting to consider whether or not that code _should_ be written that
way.

## BEM Naming Conventions Stifled Thoughtful Design

It’s tough, balancing [_Not Invented Here_][3] syndrome with first principles
thinking. It’s not economically sensible to reinvent every wheel, nor is it
sustainable to surrender to the learned helplessness of _best practice_, which
is itself a form of [fallacious reasoning][4].

The [Block Element Modifier][5] naming convention is one such _best practice_.
I regret allowing it to creep in to our codebase because I think it allowed
some developers to try to absolve themselves of the kind of thinking required
to come up with meaningful names for things. Communicating intent through clear
writing is one of the most important parts of the job.

The use of BEM has also made us stop thinking about how the problem of CSS
selector scoping could otherwise be solved. The BEM naming convention could
certainly be the right choice in some tech stacks, but in our Yesod project we
have better tools! We generate unique identifiers for components at runtime,
with some components being comprised of smaller components. This of course
isn’t exclusive to Yesod — other ecosystems solve this problem differently. But
the point is that this problem _can_ be solved differently, and to ignore that
for the sake of _best practice_ is folly.

## We Abstracted Too Early, and It Cost Us

Haskell is a high level programming language. Almost all of the code we write
describes what we wish to achieve, and not specifically _how_ the runtime
should achieve it. So, premature optimisation is not really a problem we’ve
faced historically. At least, not in terms of algorithmic efficiency. Instead,
the premature optimisation that we have at times fallen afoul of is abstracting
too early, and abstracting the wrong things.

The worst offences here typically look like functions which are highly
configurable, but don’t really _do_ anything particularly interesting. For
example, extracting a user interface component library, so that all the buttons
in your UI look the same. But in some places, the button should be a different
colour, so you make that a configuration option. And then in some places, the
button should be a different size, so you make that a configuration option. And
then in some places, the button…

You get the idea.

A function that requires a load of configuration in order for it to understand
how it’s meant to behave is probably a code smell. Instead of one highly
configurable function, we could have written a few different functions. Or, we
could have written a few different CSS mixins and composed those together.

## Haskell’s Type System Isn’t a Substitute for Good Tests

Haskell’s type system is _brilliant_.

The type system obviates the need to write so many of the more onerous tests
that we would have had to write and maintain if we were writing our project in
something like Ruby or Python. Tests that assert the shapes of data structures,
or that functions are called with the right arguments — that kind of thing.

But ensuring that a SQL query doesn’t throw an exception at runtime? We still
need tests for that. Or documenting and automatically verifying the expected
behaviour of a path through the system? We still need tests for that. There are
many more examples to list, but this list isn’t meant to be exhaustive.

Despite being able to leverage a tool as powerful as the [Glorious] Glasgow
Haskell Compiler, I’m still seeing a huge amount of value in using automated
tests to drive out the design of the system. This shouldn’t really be
surprising either — types and tests are different things and serve different
purposes.

In parts of our project where the test suite is well written, I’m seeing that
it continues to be well written as my colleagues write more production code.
This might come from a kind of social pressure — conscientious developers
generally want to leave a project in a better state than they found it. If a
test suite is well written, it just _feels_ bad to make a change that leaves
the test suite relatively less comprehensive.

So, my regret here is in not investing enough in establishing a culture of
test-driven design. And I don’t just mean we haven’t written _enough_ tests.
There’s an art to writing test code, since its purpose isn’t purely to prevent
regressions. If we had a better cultural awareness of how to use automated
tests to drive out the design of our system, we’d almost certainly have a
better production codebase for it. We do have this culture at least partially
established in our team, but it’s harder to add this after the fact. And
because the existing test code isn’t always ideal, the path of least resistance
isn’t to write ideal test code, and the relative growth of good test code
against new production code isn’t self-perpetuating.

In other words, it takes more effort now to get to where we want to be with our
test code than if we had done this up front.

## Slow CI Kills Productivity

Programmers have a kind of adversarial relationship with continuous integration
systems. It’s a healthy conflict though — the whole point is to ensure that the
changes you make are sound.

As you work, you push changes and your CI system runs an assortment of checks
against them. You might be otherwise happy with your contribution and all of
your automated tests are passing during local development, but then the CI
system flags that you neglected to add a migration. Or that the code fails some
linting rules, or that while your code works just fine on the compiler version
or system architecture that you are running locally, it fails on other compiler
versions or system architectures that you also need to support.

So, the build on CI is failing. You make another change, push that change, and
wait again for CI to pass or fail. Another failure. Make more changes. Push
again. Wait.

This style of working is of course fine, but if CI is slow, it means your
iteration cycle is slow. And a slow iteration cycle easily distracts
programmers and kills motivation. Scaling this lack of efficiency up to a team
of people means the company isn’t shipping software as fast as it should.

In hindsight, I should have invested more effort into keeping tests fast and
build times short. The compound productivity gains from this just make economic
sense.

## Lessons Learned

Looking back, the choices we made were [usually] generally well-motivated. If I
were starting Supercede today however, I would take a firmer stand against
over-engineering, and I would be more conservative — not just in the number of
tools and libraries we depend on, but also in how much of any given language we
use. Haskell is a great language, but we don't need to use _all_ of it.

This retrospective has also reaffirmed to me how important test-driven design
is as a way to approach software development. I learned all about it a decade
ago, forgot it, and then had to relearn — painfully — why TDD matters.

Finally, I don't regret for a minute any of the time I spent learning about
[logical fallacies][6]. Software engineering as an industry is _rampant_ with
fallacious reasoning, and often it's enough to recognise fallacious reasoning
when it's being employed in order to not fall prey to it.

[^1]: Or maybe there _is_ a way to do this, and I just haven’t figured out how yet.

[0]: https://reasonablypolymorphic.com/blog/elm-is-wrong/
[1]: https://en.wikipedia.org/wiki/Write_once_read_many
[2]: https://package.elm-lang.org/packages/arturopala/elm-monocle/latest/
[3]: https://en.wikipedia.org/wiki/Not_invented_here#:~:text=Not%20invented%20here%20(NIH)%20is,against%20ideas%20from%20the%20outside.
[4]: https://simple.wikipedia.org/wiki/Appeal_to_tradition#:~:text=Appeal%20to%20tradition%20(also%20known,always%20done%20it%20this%20way%22.
[5]: https://getbem.com
[6]: https://en.wikipedia.org/wiki/List_of_fallacies
