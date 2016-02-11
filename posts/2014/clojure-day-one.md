---
title: Clojure Day One
date: 2014-07-23
description: Training Luke
excerpt: Clojure is my first Lisp, so I’m bound to see some fresh ideas here even though the first Lisp appeared 55 years ago. Lisp introduced many programming ideas which today we take for granted, such as conditionals, higher-order functions, recursion, and garbage-collection.
tags: clojure
---

Clojure is my first Lisp, so I’m bound to see some fresh ideas here even though
the first Lisp appeared 55 years ago. Lisp introduced many programming ideas
which today we take for granted, such as conditionals, higher-order functions,
recursion, and garbage-collection.

<a href="http://xkcd.com/297/" class="image-link" target="_blank">
  <img src="http://imgs.xkcd.com/comics/lisp_cycles.png"
       title="I've just received word that the Emperor has dissolved the MIT computer science program permanently.">
</a>

Clojure is a Lisp that runs on the JVM which makes it a serious contender for
large enterprise applications. At my day job, I work with a bunch of incredibly
smart developers and they choose to run most of our application code on the
JVM. A bonus is that if I learn and enjoy Clojure, I could write some for my
employer.

<div id="toc"></div>

1. [Things to find](#things-to-find)
  1. [The formal definition of a Clojure function](#the-formal-definition-of-a-clojure-function)
  2. [A script for quickly invoking the repl in your environment](#a-script-for-quickly-invoking-the-repl-in-your-environment)
2. [Things to do](#things-to-do)
  1. [Check a string’s length](#check-a-strings-length)
  2. [Check a collection’s type](#check-a-collections-type)
3. [Thoughts](#thoughts)

### Things to find

#### The formal definition of a Clojure function

From the [Clojure docs](http://clojure.org/special_forms#fn):

> Fns are first-class objects that implement the IFn interface. The IFn
> interface defines an invoke() function that is overloaded with arity ranging
> from 0-20. A single fn object can implement one or more invoke methods, and
> thus be overloaded on arity.

#### A script for quickly invoking the repl in your environment

Once again, the magnificent Tim Pope saves the day with his Vim plugin: [vim-fireplace](https://github.com/tpope/vim-fireplace).

### Things to do

#### Check a string’s length

> Implement a function called `(big st n)` that returns true if a string `st`
> is longer than `n` characters.

This is relatively straightforward. Line-by-line, we define a function, write
its documentation, list its parameters, then write the function body. An
important thing to remember is that Clojure uses prefix notation as opposed to
infix notation.

~~~clojure
(defn big
  "Return length of `st` compared to `n` as boolean"
  [st, n]
  (> (count st) n))

; Usage:
(doc big)
(big "force" 5)
~~~

#### Check a collection’s type

> Write a function called `(collection-type col)` that returns `:list`, `:map`,
> or `:vector` based on the type of collection `col`.

This is also relatively straightforward. I’m not exactly sure why I couldn’t
have used the `case` method here, but using an equality operator seems to have
worked just fine. I’m slightly suspicious that comparing the class of our
collection to something like `clojure.lang.PersistentList` is brittle and I
should dig through the API for something that resembles `instanceof`.

~~~clojure
(defn collection-type
  "Return the type of collection `col` as a keyword."
  [col]
  (cond
    (= clojure.lang.PersistentList (class col)) :list
    (= clojure.lang.PersistentMap (class col)) :map
    (= clojure.lang.PersistentVector (class col)) :vector))

; Usage:
(doc collection-type)
(collection-type [:foo :bar])
~~~

### Thoughts

Today’s challenges were probably the easiest in the book so far. I like the use
of prefix notation, and I *love* that a function’s documentation is a part of
the language, and not some JavaDoc-style convention.

<a class="previous-post" href="/seven-languages/erlang-day-three">« Erlang: <i>The Red Pill</i></a>
<a class="next-post" href="/seven-languages/clojure-day-two">Clojure: <i>Yoda and the Force</i> »</a>
