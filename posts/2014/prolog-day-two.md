---
title: Prolog Day Two
date: 2014-07-07
description: Fifteen Minutes to Wapner
excerpt: The second day of Prolog focuses on lists, maths, and recursion. We also dig deep into unification, which is a core element of doing things the Prolog way. An interesting and important idea in Prolog is the way lists decompose; you split a list into its head and tail, and recursively split this way to iterate over the whole collection.
tags: prolog
---

The second day of Prolog focuses on lists, maths, and recursion. We also dig
deep into unification, which is a core element of doing things the Prolog way.
An interesting and important idea in Prolog is the way lists decompose; you
split a list into its head and tail, and recursively split this way to iterate
over the whole collection.

<div id="toc"></div>

1. [Things to find](#things-to-find)
  1. [Fibonaccis & factorials](#fibonaccis-&-factorials)
  2. [A real-world community](#a-real-world-community)
2. [Things to do](#things-to-do)
  1. [Reverse a list](#reverse-a-list)
  2. [Find the smallest list element](#find-the-smallest-list-element)
3. [Thoughts](#thoughts)

### Things to find

#### Fibonaccis & factorials

> Find some implementations of a Fibonacci series and factorials. How do they work?

The [Rosetta Code](http://rosettacode.org/wiki/Category:Prolog) project is a
boon for comparing the syntax and idioms of programming languages. It’s hard to
not be distracted by esoteric passages of code written in Befunge or
Golfscript. Given the number of language entries for Fibonacci, it seems it’s
the text-book example of a programming problem. Here’s the first RC example of a Prolog Fibonacci implementation:

~~~prolog
fib(1, 1) :- !.
fib(0, 0) :- !.
fib(N, Value) :-
  A is N - 1, fib(A, A1),
  B is N - 2, fib(B, B1),
  Value is A1 + B1.
~~~

This snippet introduces a couple of new operators — the `!` sign which
[“prevents backtracking of
clauses”](http://stackoverflow.com/a/15065841/704015), and the `is` keyword
which handles assignment. When you pass a number to the above rules, Prolog
tries to unify the input with either of the first two rules. If the values can
be unified (matched), then it returns immediately.

Magic happens in the third rule. If we pass a value of two or more, Prolog
begins exploring the third rule. Two numbers are derived from the input value
by subtracting `1` and `2` from it, and the derivatives are passed to two
recursive `fib` calls. The recursion continues until the values of the
derivatives are `1` and `0`. Prolog adds the numbers to find our answer.

I’m not sure why the `!` marks are necessary; I believe Prolog can unify
without them. When I try it however, I overflow the stack sooner without the
`!` marks. Prolog computes an answer very quickly, but it hits a stack overflow
with a miserably low input number — 27 on my machine. When I tried to inline
the subtractions, I hit an immediate stack overflow. Yikes. Prolog is touchy.

Prolog can avoid a stack overflow with tail-call optimisation. Apparently, you
would just need to position your recursive subgoal at the end of a recursive
rule, but that’s another exercise for another day.

Let’s move on to factorials. From Rosetta Code: The factorial function of
integer *n* is the product of sequence *n*, *n*-1, *n*-2 *etc*. For example,
the factorial of `7` would be `7*6*5*4*3*2*1 = 5040`.

~~~prolog
fact(X, 1) :- X<2.
fact(X, F) :- Y is X-1, fact(Y,Z), F is Z*X.
~~~

This snippet is similar to the Fibonacci example. The first rule is there to
prevent Prolog from counting down past `1`. The second rule recursively calls
itself with `X-1` until Prolog can unify with the first rule.

#### A real-world community

> Find a real-world community using Prolog. What problems are they solving with it today?

I found [this
article](http://www.drdobbs.com/parallel/the-practical-application-of-prolog/184405220)
which proves that Prolog is pretty much everywhere in the world, and thanks to
Boeing, even out of this world. Prolog is mostly used in academia and research,
but I did find a [long list of
applications](http://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/prolog/doc/pl_1000/pl1000v1.gz).
Apparently, Prolog also heavily influenced Erlang.

### Things to do

#### Reverse a list

Based on what I learned from the Fibonacci rules, I came up with this:

~~~prolog
rev([], []).
rev([Head|Tail], Answer) :-
  rev(Tail, ReverseTail), append(ReverseTail, [Head], Answer).
~~~

This works by splitting a list into its head and tail, and then appending the
head onto the reverse of the tail. We reverse the tail by recursively calling
the rules with a gradually dimishing tail. Once the tail become an empty list,
Prolog unifies with the first rule and the recursion stops. I wanted to put the
recursive call at the end of the list of subgoals, but strangely enough I ended
up with a longer stack trace.

#### Find the smallest list element

After spending a good deal of time thinking about this problem, I gave up and
Googled an answer. The answer I found *kind of* makes sense to me, but I
wouldn’t be able to explain it to you if you were six years old, which means I
don’t understand it well enough.

~~~prolog
smaller(X, Y, X) :- (X =< Y).
smaller(X, Y, Y) :- (Y < X).

smallest([Head|[]], Head).
smallest([Head|Tail], Answer) :-
  smallest(Tail, What),
  smaller(What, Head, Answer).
~~~

The final Prolog challenge for this chapter is to sort a list, but I’ve already
found this language challenging enough so I’m skipping that before I give up on
Prolog completely.

### Thoughts

Prolog is easily the most confusing challenge I’ve faced in my programming
career so far. I’m convinced it’s an incredibly powerful tool, but I can’t
easily transition from imperative to declarative thinking. Any knowledge I have
of more mainstream languages doesn’t mean shit in Prolog.

I feel that I’m not able to break problems into small steps or components. In
something like JavaScript, you can step through the flow of logic with
strategically placed `console.log()` calls and understand what the machine is
doing every step of the way. Not only am I not sure how to achieve the same in
Prolog, I don’t think you’re even *supposed* to think in this fashion because
it’s imperative and not declarative.

I admit Prolog has defeated me, but I will finish what I can, and come back to
it another day.

<a class="previous-post" href="/seven-languages/prolog-day-one">« Prolog: <i>An Excellent Driver</i></a>
<a class="next-post" href="/seven-languages/prolog-day-three">Prolog: <i>Blowing Up Vegas</i> »</a>
