---
title: Io Day One
date: 2013-12-12
description: Skipping School, Hanging Out
excerpt: The first day of Io looks at syntax, messaging, and prototypal inheritance.
tags: io
---

The first day of Io study was spent looking at a few syntactical rules, and how
the language uses messaging (like Objective-C), and prototypes (like
JavaScript). The author describes Io as being “as close to object-oriented
Lisp” as we’re likely to get, which is particularly interesting to me after
having read [Beating the Averages](http://www.paulgraham.com/avg.html) by Paul
Graham.

To get started playing with the language, I installed the interpreter using
Homebrew with `brew install io`.

<div id="toc"></div>

1. [Things to find](#things-to-find)
  1. [Some Io example problems](#some-io-example-problems)
  2. [An Io community](#an-io-community)
  3. [A style guide with Io idioms](#a-style-guide-with-io-idioms)
2. [Questions to answer](#questions-to-answer)
  1. [Type system](#type-system)
  2. [Truthy and falsey values](#truthy-and-falsey-values)
  3. [Slot compatibility](#slot-compatibility)
  4. [Assignment operators](#assignment-operators)
3. [Things to do](#things-to-do)
  1. [Run an Io program from a file](#run-an-io-program-from-a-file)
  2. [Executing methods](#executing-methods)
4. [Thoughts](#thoughts)

### Things to find

The [Io website](http://iolanguage.org/) documents language clearly and
concisely, so finding what you’re looking for isn’t all that difficult.

#### Some Io example problems

It seems there were some example problems in the Io website, but they’ve since
gone missing. I managed to find an archived version, and I copied them to [this
gist](https://gist.github.com/jezen/7972975).

#### An Io community

The Io community can be found in all of the usual places:

- [#io on Freenode IRC](irc://irc.freenode.net/io)
- [Io on Twitter](https://twitter.com/iolanguage)
- [Stack Overflow](http://stackoverflow.com/questions/tagged/iolanguage)

There’s also a [mailing list](http://groups.yahoo.com/neo/groups/iolanguage/info), if you’re into that kind of thing.

#### A style guide with Io idioms

I found a brief style guide on
[Wikibooks](http://en.wikibooks.org/wiki/Io_Programming/Io_Style_Guide). I
suppose given such a spartan syntax, there isn’t much more style one could
guide.

There’s a good deal of further reading on the [io-fans
website](https://io-fans.jottit.com/), though each article is maintained by an
individual and could go out of date or disappear entirely.

### Questions to answer

#### Type system

> Evaluate 1 + 1 and then 1 + "one". Is Io strongly typed or weakly typed?
> Support your answer with code.

Io is strongly typed. Running the above through the interpreter returns `2` for
`1 + 1`, and throws an exception when adding the number to the string. To me,
this is expected behaviour. I doubt I would ever want the consecutive letters
o-n-e to be interpreted as the number `1` by a scripting language. I *might*
want the string `"1"` to be interpreted as the number `1`, but Io throws an
exception there too.

I think this is the sane way of going about business, especially when we
compare with JavaScript. The addition of the numbers `1` and `1` in JavaScript
evalute to `2`, but if one of those numbers had been a string, the `+` operator
would have performed concatenation instead of addition, *i.e.*, `1 + "1"`
evalutes to `"11"` — which is probably not what you wanted!

#### Truthy and falsey values

> Is `0` true or false? What about the empty string? Is `nil` true or false?
> Support your answer with code.

As far as I can tell, it’s not possible to explicitly cast a value to boolean.
Truthy and falsey values can be discerned by sending the `isTrue` message.

```io
Io> 0 isTrue
==> true
Io> "" isTrue
==> true
Io> nil isTrue
==> false
```

#### Slot compatibility

> How can you tell what slots a prototype supports?

If you’re just looking for a list of slots, you send the `slotNames` message.
If you’d like a map of names and values, you send `slotSummary`.

```io
Io> Car := Object clone
==>  Car_0x7ff3590b27f0:
  type             = "Car"
Io> Car model := "Boxster S"
==> Boxster S
Io> Car slotNames
==> list(model, type)
Io> Car slotSummary
==>  Car_0x7ff3590b27f0:
  model            = "Boxster S"
  type             = "Car"
```

#### Assignment operators

> What is the difference between `=` (equals), `:=` (colon equals), and `::=`
> (colon colon equals)? When would you use each one?

The
[documentation](http://iolanguage.org/scm/io/docs/IoGuide.html#Syntax-Assignment)
covers this very well already.

- `::=` Creates slot, creates setter, assigns value
- `:=` Creates slot, assigns value
- `=` Assigns value to slot if it exists, otherwise raises exception

As for *why* one might use one over the other… I have no idea. It seems as
though you would get by using `:=` for everything.

### Things to do

#### Run an Io program from a file

This works the same way as Ruby (and I suppose, most things). Simply run the
binary with the filename as the first argument, *e.g.*, `io file.io`.

While we’re on the topic of working with files, I found a Vim plugin for Io
called [vim-io](https://github.com/xhr/vim-io) which adds support for syntax
highlighting.

#### Executing methods

First you define a method by assigning `method(…)` to a slot. Executing that
method is as simple as sending the slot name to the object.

```io
Io> Car drive := method("Vroom!" print)
==> method(
    "Vroom!" print
)
Io> Car drive
Vroom!==> Vroom!
```

You can also inspect the source code of a method with `getSlot("drive") code
print`.

### Thoughts

Io is a straight-forward, minimalist language. I like the idea that there’s an
extremely small amount of syntax to remember, and that the method names I’ve
encountered so far do what you would expect them to do.

Chaining messages is interesting, though I think the lack of punctuation when
chaining is likely to cause confusion with newcomers.

A couple of things Io has in common with Ruby (and most of the languages in the
book), is that semicolons aren’t obligatory, and returns are implicit. These
are small details that I’ve really come to appreciate.

<a class="previous-post" href="/seven-languages/ruby-day-three">« Ruby: <i>Serious Change</i></a>
<a class="next-post" href="/seven-languages/io-day-two">Io: <i>The Sausage King</i> »</a>
