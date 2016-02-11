---
title: Prolog Day Three
date: 2014-07-11
description: Blowing Up Vegas
excerpt: The final chapter on Prolog looks at constraint programming — namely, describing the rules for the Sudoku and Eight Queens puzzles and having Prolog solve them for you. I think these are the types of programming problems where Prolog would be the Right Tool for the Job.
tags: prolog
---

The final chapter on Prolog looks at constraint programming — namely,
describing the rules for the Sudoku and Eight Queens puzzles and having Prolog
solve them for you. I think these are the types of programming problems where
Prolog would be the *Right Tool for the Job™*.

<div id="toc"></div>

1. [Input & output](#input-&-output)
2. [Conditionals](#conditionals)
3. [Solve Sudoku](#solve-sudoku)
3. [Thoughts](#thoughts)

### Input & output

> Prolog has some input/output features as well. Find print predicates that
> print out variables.

I found the `write/1` predicate and gave it a test-drive.

~~~prolog
| ?- write("Hello!").
[72,101,108,108,111,33]

yes
~~~

Well that was weird! It gave me a list of integers. If I change the style of
quotes I use however…

~~~prolog
| ?- write('Hello!').
Hello!

yes
~~~

… it works as expected. I’m not exactly sure why Prolog differentiates between
the two, or what that would be useful for.

I found [a paper](http://alumni.cs.ucr.edu/~vladimir/cs171/prolog_2.pdf) by
Vladimir Vacic and Christos Koufogiannakis that covers the rest of the I/O
predicates.

### Conditionals

> Find a way to use the print predicates to print only successful solutions.
> How do they work?

I’m not sure if I’m understanding the question correctly, but I found that
Prolog supports conditional statements with an arrow syntax. Here’s an example
where I compare two numbers and print the result conditionally.

~~~prolog
greater(X, Y) :-
  X > Y
    ->
      write('It’s greater.')
    ;
      write('Nope.').
~~~

This is essentially a one-liner, but I divided it up so the syntax is clearer.
Prolog is *not* indent-sensitive.

### Solve Sudoku

> Modify the Sudoku solver to work on 9×9 puzzles.

The book provides a Prolog script that solves 4×4 Sudoku puzzles. The script
contains surprisingly little logic; the bulk of the code outlines structures to
represent each row, column, and square in the game. Here’s the script, modified
to work on 9×9 puzzles.

~~~prolog
valid([]).
valid([Head|Tail]) :-
  fd_all_different(Head),
  valid(Tail).

sudoku(Puzzle, Solution) :-
  Solution = Puzzle,

  Puzzle = [S11, S12, S13, S14, S15, S16, S17, S18, S19,
            S21, S22, S23, S24, S25, S26, S27, S28, S29,
            S31, S32, S33, S34, S35, S36, S37, S38, S39,
            S41, S42, S43, S44, S45, S46, S47, S48, S49,
            S51, S52, S53, S54, S55, S56, S57, S58, S59,
            S61, S62, S63, S64, S65, S66, S67, S68, S69,
            S71, S72, S73, S74, S75, S76, S77, S78, S79,
            S81, S82, S83, S84, S85, S86, S87, S88, S89,
            S91, S92, S93, S94, S95, S96, S97, S98, S99],

  fd_domain(Solution, 1, 9),

  Row1 = [S11, S12, S13, S14, S15, S16, S17, S18, S19],
  Row2 = [S21, S22, S23, S24, S25, S26, S27, S28, S29],
  Row3 = [S31, S32, S33, S34, S35, S36, S37, S38, S39],
  Row4 = [S41, S42, S43, S44, S45, S46, S47, S48, S49],
  Row5 = [S51, S52, S53, S54, S55, S56, S57, S58, S59],
  Row6 = [S61, S62, S63, S64, S65, S66, S67, S68, S69],
  Row7 = [S71, S72, S73, S74, S75, S76, S77, S78, S79],
  Row8 = [S81, S82, S83, S84, S85, S86, S87, S88, S89],
  Row9 = [S91, S92, S93, S94, S95, S96, S97, S98, S99],

  Col1 = [S11, S21, S31, S41, S51, S61, S71, S81, S91],
  Col2 = [S12, S22, S32, S42, S52, S62, S72, S82, S92],
  Col3 = [S13, S23, S33, S43, S53, S63, S73, S83, S93],
  Col4 = [S14, S24, S34, S44, S54, S64, S74, S84, S94],
  Col5 = [S15, S25, S35, S45, S55, S65, S75, S85, S95],
  Col6 = [S16, S26, S36, S46, S56, S66, S76, S86, S96],
  Col7 = [S17, S27, S37, S47, S57, S67, S77, S87, S97],
  Col8 = [S18, S28, S38, S48, S58, S68, S78, S88, S98],
  Col9 = [S19, S29, S39, S49, S59, S69, S79, S89, S99],

  Sqr1 = [S11, S12, S13, S21, S22, S23, S31, S32, S33],
  Sqr2 = [S14, S15, S16, S24, S25, S26, S34, S35, S36],
  Sqr3 = [S17, S18, S19, S27, S28, S29, S37, S38, S39],
  Sqr4 = [S41, S42, S43, S51, S52, S53, S61, S62, S63],
  Sqr5 = [S44, S45, S46, S54, S55, S56, S64, S65, S66],
  Sqr6 = [S47, S48, S49, S57, S58, S59, S67, S68, S69],
  Sqr7 = [S71, S72, S73, S81, S82, S83, S91, S92, S93],
  Sqr8 = [S74, S75, S76, S84, S85, S86, S94, S95, S96],
  Sqr9 = [S77, S78, S79, S87, S88, S89, S97, S98, S99],

  valid([Row1, Row2, Row3, Row4, Row5, Row6, Row7, Row8, Row9,
         Col1, Col2, Col3, Col4, Row5, Row6, Row7, Row8, Row9,
         Sqr1, Sqr2, Sqr3, Sqr4, Sqr5, Sqr6, Sqr7, Sqr8, Sqr9]).
~~~

This challenge wasn’t particularly exciting, as I really only extended the script I found in the book. I did however start searching for a way to not have to write out all of those lists manually, and after a couple of hours of chatting to a seasoned Prolog developer on IRC I was pointed towards a less verbose solution:

~~~prolog
:- use_module(library(clpfd)).

sudoku(Rows) :-
        length(Rows, 9), maplist(length_(9), Rows),
        append(Rows, Vs), Vs ins 1..9,
        maplist(all_distinct, Rows),
        transpose(Rows, Columns), maplist(all_distinct, Columns),
        Rows = [A,B,C,D,E,F,G,H,I],
        blocks(A, B, C), blocks(D, E, F), blocks(G, H, I).

length_(L, Ls) :- length(Ls, L).

blocks([], [], []).
blocks([A,B,C|Bs1], [D,E,F|Bs2], [G,H,I|Bs3]) :-
        all_distinct([A,B,C,D,E,F,G,H,I]),
        blocks(Bs1, Bs2, Bs3).

problem(1, [[_,_,_,_,_,_,_,_,_],
            [_,_,_,_,_,3,_,8,5],
            [_,_,1,_,2,_,_,_,_],
            [_,_,_,5,_,7,_,_,_],
            [_,_,4,_,_,_,1,_,_],
            [_,9,_,_,_,_,_,_,_],
            [5,_,_,_,_,_,_,7,3],
            [_,_,2,_,1,_,_,_,_],
            [_,_,_,_,4,_,_,_,9]]).
~~~

### Thoughts

I’m absolutely convinced that Prolog is a powerful tool and it’s worth coming
back to. Since it has such little relation to the rest of the [imperative and
functional] languages in *Seven Languages in Seven Weeks*, I don’t feel just a
few chapters is enough to catalyse declarative thought. It’s certainly whet my
appetite though.

<a class="previous-post" href="/seven-languages/prolog-day-two">« Prolog: <i>Fifteen Minutes to Wapner</i></a>
<a class="next-post" href="/seven-languages/erlang-day-one">Erlang: <i>Appearing Human</i> »</a>
