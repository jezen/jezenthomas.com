---
title: The Case Against Dynamic Typing
date: 2016-01-26
location: Gdynia, Poland
excerpt: You can have your cake, you can eat your cake, and you can automatically verify the fundamental correctness of the pâtisserie at bake-time.
tags: code, ruby, haskell, scala
---

<span class="run-in"><span class="drop">A</span>n argument for dynamic
typing</span> is that by not enforcing the types of a function's parameters, the
function becomes more generalised, and thus more flexible and reusable. I argue
that this flexibility versus automatically-verifiable correctness of the system
is not a trade-off we need to make.

This post is in part inspired by [*Why Dynamic Typing Is Useful*][bernhardt] by
Gary Bernhardt. I'll concede that his post is published 10 years prior, so I've
had plenty of time to build a counter-argument.

### Why Are Dynamic Types Useful?

To maintain sanity, programmers enforce rigid constraints over the parameters
their functions can consume. If my `process_many` function takes some `things`,
then dagnabbit; `things` better be an array! How else will I *know* that I can
send array-flavoured messages to it?

A core language feature of Ruby is *duck typing*, where essentially the exact
type of an object is disregarded in favour of the more pertinent question:
“Object, do you understand the message I’m sending you?”. It's neat way of doing
polymorphism.

It's easy to take for granted how well this maps to a normal person's perception
of the world. When you learn how to drive a car, your skills aren't exclusively
applicable to a car. Your physical output is just as effective when directed
towards a truck or a golf cart.

A code library author shouldn't need to anticipate every type of parameter that
might be thrown at a function, and a programmer who comes to consume the
function will likely be creative enough to use the function in ways the author
hadn't expected. Dynamic typing allows for all of this.

### Why Are Dynamic Types Painful?

The correctness of a dynamically-typed system is at the whim of the developer's
discipline and vigilance. And therein lies the rub. If test-cases aren't
accounted for, it's the programmer's fault. Same deal if an interface goes out
of sync.

Even people who advocate most strongly for dynamic typing program defensively.
If dynamic typing filled you with confidence, would you be littering your code
with `responds_to?` or `nil?`?

The code library author relies on writing isolated tests with various kinds of
inputs to ensure the robustness of the function. The function's consumer relies
on some sort of JavaDoc-style documentation comment which is likely to go out of
sync with the function's *actual* signature. Both rely on the rigour of humans.

If your code relies on JavaDoc-style comments, the machine isn't being used as
well as it should. Ideally, a function's signature should be verifiable at
compile time.

### What Is The Solution?

A smarter type system.

The argument for dynamic typing doesn't account for languages that provide *type
classes*. Granted, there aren't many languages that support type classes —
Haskell does, Scala does, and apparently Rust does too. In Haskell for example,
you might have a type class named ‘Traversable’, and any data type that can be
traversed over would be an instance of the ‘Traversable’ type class.

This means that instead of saying “this function takes a list”, and then having
to account for the possibility that the consumer might want to pass in a
dictionary instead, you would say “this function takes anything that can be
traversed over”. That way, you have the flexibility of dynamic types, and the
sanity of compile-time verifiable function signatures.

### If Dynamic Types Are No Good, Why Are They So Popular?

JavaScript only provides one type of number (64-bit floating point), though
people still do monetary calculations with it. Popularity is seldom a good
indicator that something is of good quality.

It's evident that those using dynamic languages in anger are shying away from
dynamics despite the supposed flexibility it provides. A few examples which come
to mind:

- Ruby 3 looks to be implementing some type of static/gradual typing system.
- Haskell is increasingly being adopted by the world's most front-running tech
  companies.
- TypeScript and Facebook's Flow type-checker are gaining traction.

[bernhardt]: http://blog.extracheese.org/2006/12/why-dynamic-typing-is-useful.html
