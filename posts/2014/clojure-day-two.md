---
title: Clojure Day Two
date: 2014-08-01
description: Yoda and the Force
excerpt: A look at Clojure’s macros and lazy-evalutation, and how the language interacts with the JVM via types and interfaces.
tags: clojure
---

The second day of Clojure looks at lazy evaluation, macros, and writing classes
to interfaces. Macros are a core concern of all lisps, so understanding this
idea means I would be slightly less disoriented when reading something in
another dialect like Common Lisp or Scheme.

<div id="toc"></div>

1. [Things to find](#things-to-find)
  1. [Some Clojure macros](#some-clojure-macros)
  2. [A lazy sequence](#a-lazy-sequence)
2. [Things to do](#things-to-do)
  1. [The unless condition](#the-unless-condition)
  2. [A type with defrecord](#a-type-with-defrecord)
3. [Thoughts](#thoughts)

### Things to find

#### Some Clojure macros

> Find the implementation of some of the commonly used macros in the Clojure language.

A whole bunch of core Clojure constructs are in fact macros. We can view the
implementation of a macro by *expanding* it. Here are a couple of examples I
found in [the documentation](http://clojure.org/macros):

~~~clojure
user=> (macroexpand '(when (pos? a) (println "positive") (/ b a)))
(if (pos? a) (do (println "positive") (/ b a)))

user=> (macroexpand '(-> {} (assoc :a 1) (assoc :b 2)))
(assoc (clojure.core/-> {} (assoc :a 1)) :b 2)')
~~~

#### A lazy sequence

> Find an example of defining your own lazy sequence.

Clojure’s lazy evaluation is quite clever in that it allows you to define some
infinite sequence, and rather than hang forever while it computes to infinity,
you can just take the parts from the sequence that you need. I found a fairly
contrived yet simple to understand example on [Stack
Overflow](http://stackoverflow.com/a/4993105/704015):

~~~clojure
(defn ints-from [n]
  (cons n (lazy-seq (ints-from (inc n)))))

(take 10 (ints-from 7))
=> (7 8 9 10 11 12 13 14 15 16)
~~~

### Things to do

#### The unless condition

> Implement an `unless` with an `else` condition using macros.

With macros, we’re able to add our own control structures to Clojure. Defining
a macro is similar to defining a function; the difference is that a normal
function will evaluate all of its arguments, whereas with a macro we can delay
execution.

Here’s my implementation of an `unless` control structure. I wouldn’t write
this in production code, but it makes for a clear example.

~~~clojure
(defmacro unless [test body]
  (list 'if (list 'not test)
  body
  '(println "Chocolate Chip Wookiee")))

; usage
user=> (unless false (println "Do or do not. There is no try."))
Do or do not. There is no try.
nil
user=> (unless true (println "It’s a trap!"))
Chocolate Chip Wookiee
nil
~~~

An interesting extension of this would be to declare an optional third
parameter that takes another `body`, so the user can choose what they’d like to
do if the `else` branch of the conditional is reached.

#### A type with defrecord

> Write a type using `defrecord` that implements a protocol.

Records and protocols are Clojure’s equivalent of Java’s classes and
interfaces. Thinking in terms of classes and interfaces seems like a very
OO-style thing to do, so I’m unsure where this fits into a functional language
like Clojure. I think Rich Hickey spent four years lying in a hammock thinking
about how to design Clojure. I think I’m going to need four years hacking with
Clojure to understand it.

~~~clojure
(defprotocol Manoeuvre
  (accelerate [this]))

(defrecord Car [brand model]
  Manoeuvre
  (accelerate [_] (print "The " brand model " has accelerated.")))

; Usage
(def boxsterS (->Car "Porsche" "Boxster S"))

(accelerate boxsterS)
~~~

### Thoughts

As I mentioned, Clojure is complicated and I think it’s not a language that can
be skim-read easily. Given that it’s a functional language *and* it’s on the
JVM, I’d say its a language that’s worth the initial time investment. I’ll make
a mental note to write my *Next Big Idea*™ in Clojure.

<a class="previous-post" href="/seven-languages/clojure-day-one">« Clojure: <i>Training Luke</i></a>
<a class="next-post" href="/seven-languages/clojure-day-three">Clojure: <i>An Eye for Evil</i> »</a>
