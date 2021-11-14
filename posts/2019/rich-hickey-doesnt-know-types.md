---
title: Rich Hickey Doesn't Know Types
date: 2019-04-09
location: Sopot, Poland
excerpt:
  Sorry Rich, typing preferences are one thing, but the implication that
  parametricity is effectively useless is just a bridge too far. You can't expect
  to say something wrong on the Internet and get away with it!
tags: clojure, haskell
banner: hickey.jpg
---

Clojure inventor Rich Hickey is known for his eloquence. His talks like _Simple
Made Easy_ are often cited as a favourite when you ask seemingly any savvy
programmer.

There's something about the dulcet tones of his radio-smooth voice that brings
a little more magic to any kind of philosophising about software complexity.
Throw in some light usage of the word "complect", allow to simmer for five
minutes, and you have a seductive Nigella Lawson flavoured dish, garnished with
a twist of pretentiousness.

This is all well and good until he begins his tired _types are bad_ tirade.

Hickey doesn't understand types. If he did, he wouldn't be able to say the
things he says with any shred of intellectual integrity, and I'd rather suggest
he's displaying ignorance than dishonesty.

More specifically, I believe he doesn't understand parametricity. I base that
on one of his talks in which he says the following with much exasperation and
furrowed brow:

> `a -> a`; `[a] -> [a]`; It means nothing! It tells you *nothing*!

 — Rich Hickey, [Effective Programs][effective_programs].

I don't know if he meant to say "A to A" (`a -> a`). Assuming he did, that's an
easy concept to unpack, and an important step in understanding parametricity.

The first assumption most people make is that a function which takes an `a` and
returns an `a` could have _any_ implementation. This seems reasonable enough —
the value could _be_ anything, so perhaps the function could _do_ anything,
_right_?

No. Not right.

What if the value is a number, like `1`? Could the returned value be the same
number plus one? It would be the same _type_, right? Yeah, but then consider
that the idea of "adding one" would also have to apply to _any other type_ of
value, and there are _infinite_ types of values. What would `True + 1` be?
Exactly. It'd be nonsense.

What about if we keep the same value, but concatenate itself with another copy
of itself? Well, we can't. A Haskeller might describe this by saying not every
type has a semigroup instance. This essentially just means that not every type
of value can be concatenated. Create more examples like this to your heart's
content.

This function is also guaranteed not to have any effect on the outside world.
We _know_ it won't send your Bitcoins to China. We _know_ it won't open or
close your garage door. We _know_ it won't hang while waiting for user input.
For any of that to work, we'd need to see some hint of `IO` expressed in the
type signature.

Therefore, the _only_ thing `a -> a` could _possibly_ be, is a function that
takes a value and returns the same value unchanged. In Haskell, this is called
the `id` function (but referred to in speech as "identity").

This alone is evidence enough that Hickey's statement of "it means nothing!" is
patently and demonstrably false. As it turns out, there's an awful lot we
_know_ about a function, without even reading its implementation!

With that out of the way, we're prepared to unpack his second example of "list
of A to list of A" (`[a] -> [a]`).

First, an admission: This could _technically_ have infinite implementations,
but not for any reason that Hickey is making any allusions to. It could have
infinite implementations in the sense that you could implement it as the input
alone, or concatenated with itself, or concatenated with itself twice, or
thrice, and so on to infinity.

```
f :: [a] -> [a]
f a = a ++ a
-- or
f a = a ++ a ++ a
-- or
f a = a ++ a ++ a ++ a
-- ad infinitum
```

For practical purposes, this distinction is not useful, so it does nothing to
support Hickey's claim that the type signature `[a] -> [a]` "tells you
nothing".

A second admission: While a function of `[a] -> [a]` is considered _pure_ in
Haskell, it could potentially cause the _side effect_ of crashing your program
because of the [Halting Problem][halting_problem].

The execution of this program (theoretically, depending on input and other
circumstances) could _also_ push your CPU so hard that your machine overheats,
bursts into flames, and melts into its own little pool of glowing lava. That'd
be one hell of a side-effect.

```
f :: [a] -> [a]
f a = f a
```

Again, this distinction is not particularly useful. I include these admissions
in the interest of fairness. I want to be clear that I do not intend to
misrepresent neither Hickey nor the benefits and drawbacks of a good type
system such as the one in Haskell.

Let's retreat to the realm of the reasonable. Could this function be `reverse`?
Yeah, sure. You could recursively call this function with all the elements
except the first one, joined with a list of just the first item.

```
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]
```

Pretty standard stuff. What else could it be? Could it be "Give me the first
_n_ elements of the list?" Yeah, that works. You could also drop the first _n_
elements of the list.

It's worth noting at this point that none of the functions we _can_ implement
for this type signature actually touch the `a` inside the `[]`. Again, because
of parametricity, we're not allowed to do that. The rules about what we can do
with `a -> a` seem small and trivial, but actually this stuff is _fundamental_.
This principle stacks up with _enormously_ meaningful effect.

What about a shuffle? Can `[a] -> [a]` shuffle its elements?

Think about this for a moment. How would this work? Given referential
transparency, what would happen on subsequent calls to this function?

For it to do a shuffle, it would need a random seed. You could either pass that
in, which would make it something like

```
f :: Seed -> [a] -> [a]
```

…or you would have to do it in IO, which would make it

```
f :: [a] -> IO [a]
```

Once again, there are quite many things we can _know_ about what a function can
do just by reading its type signature.

You may begin to believe that since I have so far railed about theory, the
benefits of all this are also only theoretical. That couldn't be further from
the truth.

As Kris Jenkins said in his talk [Communicating in Types][communicating_in_types]:

> When you have a type error — and you _always_ get a type error — the question
> is whether you get it from QA, or your users, or your compiler.

The nature of business is change, and this is especially true in the startup
world where I tend to reside. If you want to make large changes across a system
in a dynamic language without breaking everything, you need to rely on human
discipline to have written all the tests.

Writing these tests manually is never going to be as quick as a compiler
writing them for you. With a good compiler these kinds of changes go from being
nearly impossible to just tedious. Encoding invariants in a type system is far
cheaper than writing tests.

Perhaps when Hickey says things like "it tells you nothing", he is just using
hyperbole for theatrical effect. If that's the case, it's effective — It
certainly got a reaction from me! However I don't think it's an effective
method for catalysing thoughtful discussion.

Type systems like Haskell's aren't a panacea, and there are _some_ things that
a language with a dynamic type system can do that are harder to achieve in
Haskell (though what this means in the context of a business is a separate
discussion).

That said, I don’t think Rich Hickey compares the two approaches on fair terms.
It isn't fair to say "type systems don’t help because if you try hard enough
you can break them. What you need to do instead is [lots of hand-waving here]
make things _simple!_"

If you listen carefully to Hickey when he talks harshly about types, you maybe
identify a number of logical fallacies.

When Rich Hickey dismisses the value of a powerful type system by saying "oh it
doesn't really work in practice", is this the _Anecdotal_ logical fallacy?
Because anecdotally, this stuff works for me and for many people I've worked
with, _in practice_. It could also perhaps be _Ambiguity_, or _No True
Scotsman_.

And the people defending his take on this? _Appeal to Authority_.

I say this being totally aware that I may be committing the _Fallacy Fallacy_,
but I’m yet to be convinced that an accumulation of design aids does not yield
a net benefit.

In any case, writing robust software is pretty tough and I'm happy to take all
the help I can get. Rejecting an entire field of study (like types!) is not
only anti-intellectual, but also does nothing to move the state of our industry
forward. I'd say that's especially harmful now in a time where software errors
can bring down a couple of Boeing 787 Max airplanes and kill hundreds of
people.

[effective_programs]: https://youtu.be/2V1FtfBDsLU?t=4020
[halting_problem]: https://en.wikipedia.org/wiki/Halting_problem
[communicating_in_types]: https://vimeo.com/302682323
