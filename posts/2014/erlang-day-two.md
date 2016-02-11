---
title: Erlang Day Two
date: 2014-07-14
description: Changing Forms
excerpt: Today we’re diving deeper into the functional programming paradigm with Erlang, looking at the language’s control structures, lists, and a little more on functions.
tags: erlang
---

Today we’re diving deeper into the functional programming paradigm with Erlang,
looking at the language’s control structures, lists, and a little more on
functions.

<div id="toc"></div>

1. [Matching with tuples](#matching-with-tuples)
2. [Manipulating a list](#manipulating-a-list)
3. [Parsing Tic-Tac-Toe](#parsing-tic-tac-toe)
4. [Thoughts](#thoughts)


### Matching with tuples

> Consider a list of keyword-value tuples, such as `[{erlang, "a functional
> language"}, {ruby, "an OO language"}]`. Write a function that accepts the
> list and a keyword and returns the associated value for the keyword.

~~~erlang
-module(day_two).
-export([description_for_term/2]).

description_for_term(Languages, Term) ->
  {_, Description} = lists:keyfind(Term, 1, Languages),
  Description.
~~~

I’m surprised at how little code I had to write to make this work. I initially
thought of iterating over the list and manually comparing `Term` to the first
element of each tuple, but after some digging around I found the
[`keyfind`](http://erlang.org/doc/man/lists.html#keyfind-3) method in the lists
module, which does the work of searching through the list for the tuple I want.

I then use the idea of unification that I learned from Prolog to bind the
second element of my tuple to the variable `Description`, which I return at the
end.

### Manipulating a list

> Consider a shopping list that looks like `[{item, quantity, price}, …]`.
> Write a list comprehension that builds a list of `items` of the form `[{item,
> total_price}, …]`, where `total_price` is `quantity` times `price`.

This problem was a nice opportunity to exercise Erlang’s list comprehension
construct. I could have done this with `lists:map(function, list)`, but I think
the idiomatic way is with this bracket syntax.

~~~erlang
-module(day_two).
-export([invoice_for_shopping_list/1]).

invoice_for_shopping_list(Shopping_list) ->
  [{Item, Quantity * Price} || {Item, Quantity, Price} <- Shopping_list].
~~~

The syntax may look slightly cryptic at first, but I think it’s quite elegant
and easy to understand. On the first line (ignoring the module and export
declarations) I declare a function that takes one parameter, which is the
shopping list.

Now reading the second line from right to left: For each item in the list
`Shopping_list`, bind the elements of the tuple to `Item`, `Quantity`, and
`Price`. The tuple returned has the `Item` unaltered, and then the product of
whatever was bound to `Quantity` and `Price`.

### Parsing Tic-Tac-Toe

I’m feeling confident and I’m so far quite enjoying Erlang, so I’ll have a go
at the bonus problem from this chapter.

> Bonus problem: Write a program that reads a Tic-Tac-Toe board presented as a
> list or a tuple of size nine. Return the winner (X or O) if a winner has been
> determined, `draw` if there are no more possible moves, or `no_winner` if no
> player has won yet.

~~~erlang
t3_state(Game) ->

  case Game of
    [x,x,x|_] -> x;
    [_,_,_,x,x,x|_] -> x;
    [_,_,_,_,_,_,x,x,x] -> x;
    [x,_,_,x,_,_,x,_,_] -> x;
    [_,x,_,_,x,_,_,x,_] -> x;
    [_,_,x,_,_,x,_,_,x] -> x;
    [x,_,_,_,x,_,_,_,x] -> x;
    [_,_,x,_,x,_,x,_,_] -> x;
    [o,o,o|_] -> o;
    [_,_,_,o,o,o|_] -> o;
    [_,_,_,_,_,_,o,o,o] -> o;
    [o,_,_,o,_,_,o,_,_] -> o;
    [_,o,_,_,o,_,_,o,_] -> o;
    [_,_,o,_,_,o,_,_,o] -> o;
    [o,_,_,_,o,_,_,_,o] -> o;
    [_,_,o,_,o,_,o,_,_] -> o;
    [A,B,C,D,E,F,G,H,I] when A =/= e, B =/= e, C =/= e,
                             D =/= e, E =/= e, F =/= e,
                             G =/= e, H =/= e, I =/= e -> draw;
    _ -> no_winner
  end.
~~~

This is probably the corniest code I have ever written. The whole thing is one
big pattern match, where I test every possible win condition. I’m certain
there’s a less verbose way of expressing this function, but I haven’t thought
of it yet.

On the one hand, it’s interesting that such a visual approach can be taken to
solving a programming problem. On the other hand, I feel that I should refactor
this to remove the duplication. If I do decide to revisit this challenge at
some point (there are many rainy days in Sweden, so this is likely), I think I
would create a function that checks that each group of three contiguous cells
have the same marker, and return that marker. I’d also like to write a function
that *rotates* the game board, so that instead of checking three rows, three
columns and two diagonals, I would check three rows and a diagonal, then rotate
and repeat.

One thing that makes me slightly unhappy is that I couldn’t use a list
comprehension in the penultimate guard — I had to compare every cell on the
game board with the atom `e` (which in this context I’m using as an *empty*
value) and string the comparisons together with commas which are a bit like
logical `AND`s in Erlang and Prolog.

If the game board has three contiguous atoms, we return the winner. If we don’t
find three contiguous atoms, we fall through to the penultimate clause and
check for any empty cells. If no cells are empty, the game is a draw. If there
are empty cells, we fall through to the catch-all pattern and return
`no_winner`.

### Thoughts

I’m still thoroughly enjoying Erlang, despite its few warts. I’m not sure if
it’s Erlang itself or the functional programming paradigm in general that I’m
enjoying more since as I understand it, Erlang shines brightest when dealing
with concurrency and telecommunications stuff, none of which I’ve covered yet.

<a class="previous-post" href="/seven-languages/erlang-day-one">« Erlang: <i>Appearing Human</i></a>
<a class="next-post" href="/seven-languages/erlang-day-three">Erlang: <i>The Red Pill</i> »</a>
