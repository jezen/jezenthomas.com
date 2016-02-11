---
title: Io Day Two
date: 2014-01-22
description: The Sausage King
tags: io
---

In the second day of Io language studies, we looked at control structures and
how to define custom operators. We also had a deeper look at how message
sending works, and how to reflect on an object’s prototype chain.

<div id="toc"></div>

1. [Things to do](#things-to-do)
  1. [The Fibonacci sequence](#the-fibonacci-sequence)
  2. [Divide by zero](#divide-by-zero)
  3. [List comprehension](#list-comprehension)
  4. [Extending objects](#extending-objects)
  5. [Matrix factory](#matrix-factory)
  6. [Reading and writing files](#reading-and-writing-files)
  7. [Higher or lower](#higher-or-lower)
2. [Thoughts](#thoughts)

### Things to do

#### The Fibonacci sequence

> A Fibonacci sequence starts with two 1s. Each subsequent number is the sum of
> the two numbers that came before. Write a program to find the n<sup>th</sup>
> Fibonacci number.

I’m absolutely not a maths guy, but I’ve tackled the Fibonacci sequence before
on [Project Euler](http://projecteuler.net/) and I learned it has a close
relationship with the Golden Ratio. Any number in the sequence can be solved
using Phi, which to 15 decimal places is `1.618033988749895`.

The question originally calls for solutions using loops and recursion, so I may
be cheating slightly here. All that needs to be done is to create a method that
accepts an integer, and applies the formula <code>f<sub>n</sub> =
Phi<sup>n</sup> / 5<sup>½</sup></code>.

```io
Fib := Object clone
Fib Phi := 1.618033988749895
Fib get := method(n, ((Phi ** n) / (5 ** .5)) round)
```

#### Divide by zero

> How would you change `/` to return `0` if the denominator is zero?

Overriding default behaviour in Io is surprisingly easy; you can just assign
values and methods to operators like any other variable. In this case, we want
to retain the original behaviour of the division operator, so we store it in an
intermediate variable before assigning our custom implementation.

```io
Number oldDivision := Number getSlot("/")
Number / = method(n, if(n == 0, 0, self oldDivision(n)))
```

#### List comprehension

> Write a program to add up all of the numbers in a two-dimensional array.

My first thought was to use a nested loop (loopception?), but a less obvious
and more elegant solution is to iron out the lists and apply the addition with
a couple of chained messages.

```io
Io> list(1,2,list(3,4,5),6,7,list(8,9)) flatten reduce(+)
==> 45
```

#### Extending objects

> Add a slot called `myAverage` to a list that computes the average of all the
> numbers in a list. What happens if there are no numbers in a list?

This is an odd task; sending the `slotSummary` message to `List` shows that we
already have the message `average`, which works exactly as you would expect it
to.

One potential issue with Io’s core `average` method however is that it throws
an exception if it encounters anything that isn’t a number. Our implementation
will only look for numbers and average those. If no numbers are found, it
returns 0.

```io
List myAverage := method(
  numbers := self select(isKindOf(Number))
  if (numbers size == 0, return 0)
  numbers sum / numbers size
)
```

#### Matrix factory

> Write a prototype for a two-dimensional list. The `dim(x, y)` method should
> allocate a list of `y` lists that are `x` elements long. The `set(x, y,
> value)` method should set a value, and `get(x, y)` should return that value.

Again, sending `slotSummary` to `List` gives us a good starting place. We can
use the `setSize(n)` method to fill a list with `(n) nil` elements. My approach
is to create a list `x` elements long, and `map` that list with more lists.

The `get` and `set` methods are handled with `at()` and `atPut()`. I decrement
the indexes because in most applications I can think of, *e.g.*, Chess and
Battleships, matrices aren’t zero-based.

```io
Matrix := List clone
Matrix dim := method(x, y, self setSize(x) mapInPlace(list() setSize(y))
Matrix get := method(x, y, self at(x-1) at(y-1))
Matrix set := method(x, y, value, at(x-1) atPut(y-1, value))
```

#### Reading and writing files

> Write the matrix to a file, and read a matrix from a file.

Working with files in Io is straight-forward and similar to Ruby (and perhaps
any other language). Since my Matrix object’s prototype is the List object, I
already have a bunch of helper methods for iterating over my matrix. I used
simple concatenation to write my matrix as a CSV, and the `split` method to
convert the data back to lists.

```io
Matrix save := method(path,
  file := File with(path) openForUpdating
  self foreach(line,
    file write(line first)
    line rest foreach(value, file write("," .. value))
    file write("\n")
  )
  file close
)

Matrix load := method(path,
  file := File with(path) openForReading
  file foreachLine(line, self append(line split(",")))
  file close
)

// Usage
myMatrix save("matrix.csv")
newMatrix := Matrix clone
newMatrix load("matrix.csv")
```

#### Higher or lower

> Write a program that gives you ten tries to guess a random number from 1 –
> 100. Give a hint of “hotter” or “colder” after the first guess.

This last challenge is not quite as difficult; it’s essentially an Io
adaptation of the guessing game implemented in Ruby from the previous chapter.

```io
"Enter a number…" println
target := (Random value(99) + 1) floor
10 repeat(
  guess := File standardInput readLine asNumber
  if (guess == target, break, "Try again." println)
  if (hasSlot("previousGuess"),
    if ((guess - target) abs < (previousGuess - target) abs,
      "You’re getting warmer…" println,
      "You’re getting colder…" println
    )
  )
  previousGuess := guess
)

if (guess == target,
  "You win!" println,
  "Better luck next time…" println
)
```

### Thoughts

After this truly extensive set of exercises, I have a more comprehensive
understanding of how Io works. I also have a more comprehensive sense of
frustration with Io, since the syntax is confusing and the documentation is
sparse. I found that the best documentation is probably the core library
referece. As of yet, I’m not sure if Io does anything better or more easily
than Ruby does, so I'm unlikely to actually use Io for anything once I’ve
finished this chapter.

<a class="previous-post" href="/seven-languages/io-day-one">« Io: <i>Skipping School, Hanging Out</i></a>
<a class="next-post" href="/seven-languages/io-day-three">Io: <i>The Parade and Other Strange Places</i> »</a>
