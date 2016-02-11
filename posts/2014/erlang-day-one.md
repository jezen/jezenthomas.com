---
title: Erlang Day One
date: 2014-07-12
description: Appearing Human
excerpt: Erlang! What an exciting language. Erlang is the first proper functional language I’m looking at. In this introduction to the language, we look at Erlang’s syntax and its similarities with Prolog. We also look at some more recursion, functions, and pattern-matching.
tags: erlang
---

Erlang! What an exciting language. Erlang is the first proper functional
language I’m looking at (Scala doesn’t count). In this introduction to the
language, the book looks at Erlang’s syntax and its similarities with Prolog.
We also look at some more recursion, functions (kind of important in a
functional language), and pattern-matching (like you would do in Scala).

<div id="toc"></div>

1. [Erlang documentation](#erlang-documentation)
2. [Count the words in a string](#count-the-words-in-a-string)
3. [Count to ten](#count-to-ten)
4. [Pattern matching](#pattern-matching)
5. [Thoughts](#thoughts)

### Erlang documentation

Here comes my best Google-fu.

- [The official Erlang site](http://www.erlang.org/)
- [Official documentation for Erlang’s function library](http://www.erlang.org/doc/man_index.html)
- [Documentation for Erlang’s OTP library](http://www.erlang.org/doc/pdf/otp-system-documentation.pdf)

### Count the words in a string

> Write a function that uses recursion to return the number of words in a string.

There may be a more elegant solution to this, but I’m quite pleased with what I
came up with. I would have had a much harder time thinking of this had I not
already spent so much time struggling with Prolog.

~~~erlang
-module(day_one).
-export([words_in_string/1]).

words_in_string([]) -> 1;
words_in_string([32|Rest]) -> 1 + words_in_string(Rest);
words_in_string([_|Rest]) -> words_in_string(Rest).
~~~

The important thing to remember is that a string is actually a list of integers
in Erlang. The magic number `32` is the ASCII code for a space character, so my
script starts with `1` and adds `1` for any spaces it finds.

The `[_|_]` form splits lists into its head and tail, just like in Prolog. We
recursively call the function with the gradually diminishing string (which is
really a list) to iterate over the whole string.

### Count to ten

> Write a function that uses recursion to count to ten.

I’m quite happy with my solution to this too. I again use pattern matching to
check the input, and the recursion stops on the base case. I added the
comparison guard to stop Erlang counting into Infinity if you do something
silly like `count_to_10_from(11)` or `count_to_10_from(1.0)`.

~~~erlang
-module(day_one).
-export([count_to_10_from/1]).

count_to_10_from(10) -> 10;
count_to_10_from(N) when N < 10 -> io:write(N), count_to_10_from(N+1).
~~~

### Pattern matching

> Write a function that uses matching to selectively print “success” or “error:
> message” given input of the form `{error, Message}` or success.

Pattern matching makes this problem trivial. The `success` atom prints a static
string, and when the tuple is matched, a string is concatenated with the error
message. When throwing an error, a literal string will have to be passed in
place of the `Message` variable, otherwise it’d be unbound and Erlang will
throw an exception.

~~~erlang
-module(print_status).
-export([print_status/1]).

print_status(success) -> "success";
print_status({error, Message}) -> string:concat("error: ", Message).
~~~

It’s important to note than `success` and `error` are *atoms* (named
constants), and `Message` is a variable. Erlang is just like Prolog in that
atoms begin with a lowercase letter, and variables begin with an uppercase
letter.

### Thoughts

My introduction to Erlang was enjoyable, especially after having such a hard
time with Prolog. I’m beginning to find it easier to think about recursion too.
I found the three coding challenges suspiciously easily — I’m hoping functional
programming just comes naturally, and that it’s not a case of this first
chapter being unrealistically simple.

<a class="previous-post" href="/seven-languages/prolog-day-three">« Prolog: <i>Blowing Up Vegas</i></a>
<a class="next-post" href="/seven-languages/erlang-day-two">Erlang: <i>Changing Forms</i> »</a>
