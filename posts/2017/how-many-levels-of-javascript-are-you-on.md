---
title: How Many Levels Of JavaScript Are You On?
date: 2017-02-01
tags: javascript
---

There's a short comic floating around the Twitterverse that pokes fun at
Functional Programming nerds. It's often on my mind because I find it absolutely
hilarious. It's a subtle, subversive, and surreal kind of comedy. I can't
exactly explain why I find it so funny, but I do.

![](/static/img/profunctoroptics.jpg)

Like so much great comedy, it illuminates some aspect of our culture that needs
to be addressed, or at least taken less seriously. Haskellers have a reputation
for indulging in the use of esoteric language; _monad, endofunctor,
zygomorphism, etc._

While Haskell nerdery is ripe for mocking, it isn't alone. JavaScript by
comparison is a seemingly bottomless pit of comedy fodder, but not quite in the
same way. The culture around Haskell is a bunch of good ideas, taken a bit too
seriously. The culture around JavaScript is that of starting with some dangerous
ideas, and piling on layer after layer of external fixes to make the language
work properly.

Come with me on a journey.

JavaScript doesn't gives us a proper standard library, so we'll add in a utility
library like Underscore.js so we can do a bunch of list processing.

Oh, it turns out all of Underscore.js' function arguments are backwards, so
using these functions the way they're intended is now kind of awkward. I guess
we can switch that out for Ramda.js or something.

Immutable data sounds far less complicated than keeping track of where state
might change. Luckily, there's a library for that too. Let's just cross our
fingers and hope that everyone on the team uses the immutable data structure
library and not the programming language by itself.

I want to express what kind of data should be flowing in and out of all my
little functions without having to write a million isolated tests. I guess
I could use something like JSDoc and add a comment to the top of each function
specifying the names and types of each parameter. I'll have to trust myself to
just _be really careful_ that the documentation and the actual type signatures
don't go out of sync.

How about those pesky colleagues of mine though? Those kids are always meddling
with my work; never bothering to update my precious documentation comments. What
about some gradual type system like Facebook's _Flow_? Ok, so it's not totally
reliable. And sure, it doesn't enforce any kind of rigour — you can explicitly
annotate any function to take _anything_ as an argument, and return an
_anything_.

Speaking of pesky colleagues: what about all those formatting errors that make
it into code review? We'll need to add linting to our build system. Wait…

A build system! Of course we'll need one of those. I think Make works well
enough, but, ah… Oh dear. It seems one of my more impressionable colleagues has
been convinced by a curly-haired San Francisco man that JavaScript is
_everything_, now and forever. So, which of those highly complex and
excrutiatingly verbose JavaScript build systems shall we choose? Hmm…

Our build system will also be good for compiling — or is it _transpiling?_ — our
JavaScript-from-the-future into a language understood by the browsers of today.
Using a language that compiles to JavaScript would be an unnecessary
complication. We should just use plain JavaScript. With added compilation.

We still haven't gotten around to picking a DOM-diffing library or a _Functional
Reactive Programming_ state store, but there's only so much that can be done in
a day.

That's enough poking fun at JavaScript for now. What I do find genuinely
interesting is that a lot of these tools are namedropped in programming
discourse, in a sort of _“my dad can beat up your dad”_ fashion.

This isn't exclusive to JavaScript, or even programmers at all. This was
standard practice for musicians back when I was one full-time. You could say
you're quite keen on Neil Peart, and I would enthusiastically recite the names
of drummers who can and do make Peart look like a shaven ape, violently flailing
in the tupperware section of a supermarket aisle.

If it weren't already clear, I also find it interesting that so many developers
would rather start with a broken thing and gradually try to improve it, than to
start with a good thing (Elm, PureScript, ClojureScript, literally anything but
JavaScript), and just enjoy the benefits of improved performance, fewer (or
a total absence of) runtime errors, a sane dependency system, a standard library
that actually helps you do your job, _etc._

Are you still using MooTools? You are like a little baby. Watch this. Spread operator.
Destructuring assignment.

How many levels of JavaScript are you on. My dude?
