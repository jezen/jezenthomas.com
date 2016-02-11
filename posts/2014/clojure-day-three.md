---
title: Clojure Day Three
date: 2014-08-05
description: An Eye for Evil
excerpt: A walk through Clojure’s concurrency structures, and an implementation of a classic Edsger Djikstra programming problem.
tags: clojure
---

The final day of Clojure introduces the language’s concurrency structures.
Clojure uses *software transactional memory*; a mechanism not so far removed
from modern database transactions. I’m not sure whether this is better or worse
than the actors found in Erlang or Io. As I understand it, actors are
*theoritically* better but programming is often about making practical
compromises. I’ll have to work with concurrency more to develop a taste for
different approaches.

The first problem — balancing an account — was relatively straightforward and
helped me to understand the concept of transactions. The *Sleeping Barber*
problem however was seriously challenging. I’m not embarrassed to say it took
me several hours of staring at the screen to come up with a solution, and I had
to go away and think about the problem a few times and even considered giving
up. I’m glad I didn’t.

<div id="toc"></div>

1. [Balancing an account](#balancing-an-account)
2. [The Sleeping Barber](#the-sleeping-barber)
3. [Thoughts](#thoughts)

### Balancing an account

> Use references to create a vector of accounts in memory. Create debit and
> credit functions to change the balance of an account.

In this example I create two references to amounts you might find in your
personal and business bank accounts. I add those references to the vector
`accounts`. The credit and debit functions are essentially the same; they both
take a reference to an account, and the amount that it should be altered by.

~~~clojure
(ns accounts.core
  (:gen-class))

(defn credit [account amount]
  (dosync
    (alter account + amount)))

(defn debit [account amount]
  (dosync
    (alter account - amount)))

(def personal (ref 3000))
(def business (ref 50000))

(def accounts [personal business])

; usage
(credit (first accounts) 1200)
(debit (last accounts) 5000)
~~~

The interesting thing here is the form of the `alter` method. This function
needs to be used like `(alter reference function optional-arguments)` or else
Clojure throws some slightly cryptic errors at you. We’re saying “Ok, `alter`
method, take my `account` reference and apply the `+` function to it with
`amount` as an argument.”

Since accounts are always a reference, any function that mutates the referenced
data must be wrapped in a transaction like `dosync`. This transaction basically
negates the possibility of data being corrupted by some ugly race condition.

### The Sleeping Barber

> Write a multithreaded program to determine how many haircuts a barber can
> give in ten seconds.

This is a problem devised in 1965 by Edsger Djikstra. It has the following characteristics:

- A barber shop takes customers.
- Customers arrive at random intervals, from ten to thirty milliseconds.
- The barber shop has three chairs in the waiting room.
- The barber shop has one barber and one barber chair.
- When the barber’s chair is empty, a customer sits in the chair, wakes up the
  barber, and gets a haircut.
- If the chairs are occupied, all new customers will turn away.
- Haircuts take twenty milliseconds.
- After a customer receives a haircut, he gets up and leaves.

In this problem we need to use threads to spin up some concurrent processes. We
do this with the `monitor-waiting-room` and `send-customers-from-street`
functions by wrapping the function bodies in futures. Without the futures, the
program would hit the first `while` loop and block, looping infinitely.

An interesting thing about collections in Clojure is that lists push and pop
from the front, whereas vectors push and pop from behind. In order to model the
concept of a waiting room where the first customer who comes to the store is
also the first customer served, we need to use a PersistentList. If you
experiment with a PersistentList in a Clojure REPL and you don’t get the syntax
100% correct, Clojure starts [messing with your
brain](http://stackoverflow.com/questions/25126368/seemingly-magical-behaviour-in-my-clojure-repl/25126712?noredirect=1#comment39109008_25126712).

Most of the code is heavily commented, so I’ll let my work speak for itself.

~~~clojure
(ns barber.core
  (:gen-class))

(def chair (ref []))
(def lost-customers (ref []))
(def shop-is-open (ref false))
(def serviced-customers (ref []))
(def waiting-customers (ref clojure.lang.PersistentQueue/EMPTY))

; If there are less than three customers waiting for a haircut, we
; can welcome them in and sit them down. If there are already three
; customers waiting, the barber has lost some business.
(defn greet-customer [customer]
  (dosync
    (if (< (count @waiting-customers) 3)
      (alter waiting-customers conj customer)
      (alter lost-customers conj customer))))

; Take a customer and cut his hair for 20 milliseconds. Once we’re
; done, we add him to our list of satisfied customers.
(defn cut-hair [customer]
  (Thread/sleep 20)
  (dosync (alter serviced-customers conj customer)))

; While the shop is open, continuously check the waiting room for
; waiting customers. If we find one, remove him from the waiting
; room and send him to have his hair cut.
(defn monitor-waiting-room []
  (future
    (while @shop-is-open
      (if (not (empty? @waiting-customers))
        (do
          (cut-hair (first @waiting-customers))
          (dosync (alter waiting-customers pop)))))))

; Send potential customers into the barber’s shop from the street
; at random intervals between 10ms and 30ms. The rand-int function
; is non-inclusive, so we have to specify an amount one higher than
; the maximum of our desired range.
(defn send-customers-from-street []
  (future
    (while @shop-is-open
      (do
        (Thread/sleep (+ 10 (rand-int 21)))
        (greet-customer :customer)))))

; Open the barber’s shop for 10 seconds, begin sending people into
; the store from the street, and ask the barber to keep an eye on
; the waiting room.
(defn open-for-business []
  (dosync (ref-set shop-is-open true))
  (monitor-waiting-room)
  (send-customers-from-street)
  (Thread/sleep (* 10 1000))
  (dosync (ref-set shop-is-open false)))

(open-for-business)
(println "serviced customers: " (count @serviced-customers))
(println "lost customers: " (count @lost-customers))
~~~

I hope my implementation is correct (it does work, and the output is within the
expected range), but I suspect I haven’t followed the specification closely
enough. In Dijkstra’s scenario, the barber is supposed to be asleep whenever
he’s not cutting hair. In my implementation, the barber is constantly
monitoring the waiting room. Let’s just pretend the door to the barber’s shop
has a very loud doorbell.

### Thoughts

Clojure feels flexible, powerful, and serious. As much as programmers love
arguing about programming languages, I don’t remember ever seeing someone
saying bad things about Clojure; it seems to be universally celebrated. Since
immutability is a core idea, I think Clojure would be a good candidate for a
language to base a career on if/when OOP is rendered inadequate, though it
looks as though there’s a whole mountain of knowledge to climb. Maybe I’ll ask
Rich Hickey if I can borrow his hammock.

<a class="previous-post" href="/seven-languages/clojure-day-two">« Clojure: <i>Yoda and the Force</i></a>
<a class="next-post" href="/seven-languages/haskell-day-one">Haskell: <i>Logical</i> »</a>
