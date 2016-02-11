---
title: Scala Day Two
date: 2014-04-21
description: Clipping Bushes and Other New Tricks
excerpt: Exploring Scala’s functional side with foldLeft, and a look at multiple inheritance with traits.
tags: scala
---

The second day of ~~Java for Hipsters~~ Scala looked at the functional side of
the language and how we can we can use a robust collection API to write
cleaner, more concise code.

<div id="toc"></div>

1. [Things to find](#things-to-find)
  1. [A discussion on how to use Scala files](#a-discussion-on-how-to-use-scala-files)
  2. [What makes a closure different from a code block](#what-makes-a-closure-different-from-a-code-block)
2. [Things to do](#things-to-do)
  1. [List comprehension](#list-comprehension)
  2. [Traits and maps](#traits-and-maps)
  3. [Loading Scala from a file](#loading-scala-from-a-file)
3. [Thoughts](#thoughts)

### Things to find

#### A discussion on how to use Scala files

A quick search pulls up [this blog
post](http://www.naildrivin5.com/blog/2010/01/26/reading-a-file-in-scala-ruby-java.html)
by David Bryant Copeland, and what he has to say about working with files in
Scala is anything but reassuring. There are some interesting answers in [this
Stack Overflow
question](http://stackoverflow.com/questions/1284423/read-entire-file-in-scala),
and typically there is some discussion around performance when handling large
files (as there always is in programmer circles). Daniel Sobral’s answer seems
the cleanest syntactically, so it’s the one I’m likely to use.

~~~scala
val source = scala.io.Source.fromFile("file.txt")
val lines = source.mkString
source.close()
~~~

#### What makes a closure different from a code block

In Scala, a code block is a function that sequentially evaluates expressions
and returns the value of the last expression; nothing strange there. A closure
is slightly more interesting because it has at least one indirect dependency.
As I understand it, both types of functions can be assigned to variables and
passed around as parameters to other functions, just like JavaScript.

### Things to do

#### List comprehension

> Use `foldLeft` to compute the total size of a list of strings.

Scala’s `foldLeft` is similar to the `reduce` method from Ruby or Io. It takes
a start value, which in our case is `0`. We can then perform some function with
the accumulator and the current list item. We take the current list item’s
string length, add it to the accumulated value, and continue through the list.

~~~scala
val wordList = List("rum", "sugar", "ice", "lime", "mint", "soda")
wordList.foldLeft(0)((total, current) => total + current.length)
// 23
~~~

#### Traits and maps

> Write a `Censor` trait with a method that will replace the curse words
> *Shoot* and *Darn* with *Pucky* and *Beans* alternatives. Use a map to store
> the curse words and their alternatives.

Scala uses ‘traits’ to handle multiple inheritance, and I think it’s a neat
idea. I haven’t relied upon multiple inheritance in practice, but traits
apparently mitigate the [“Deadly Diamond of
Death”](http://en.wikipedia.org/wiki/Multiple_inheritance#The_diamond_problem)
problem. To use it, we need to mix it into a class. Instances of a class that
extend a trait can then use properties and methods from that trait.

~~~scala
trait Censor {
  val curseWords = Map(
    "Shoot" -> "Pucky",
    "Darn" -> "Beans"
  )
  def clean(dirty: String): String = {
    curseWords.foldLeft(dirty)((content, curses) =>
      content.replaceAll(curses._1, curses._2))
  }
}

class Story(content: String) extends Censor {
  def getClean = clean(content)
}

val story = new Story("I stubbed my toe! Shoot! Darn!")

println(story.getClean)
~~~

#### Loading Scala from a file

> Load the curse words and alternatives from a file.

To start working through this problem, I created a CSV file with the curse
words and their alternatives. We can open the file with the code we found
earlier. Since we’re doing `mkString`, we have to split on a new line
character, which gives us an array of the lines in the file.

~~~scala
// Open the file
val source = scala.io.Source.fromFile("cursewords.csv")
val lines = source.mkString
source.close()

// Replaces `curseWords` from our earlier Censor trait
val curseWords = lines.split("\n")
                      .map(line => {
                        val terms = line.split(",")
                        (terms(0), terms(1))
                      })
                      .toMap
~~~

We use the `map` method to perform some action with each item in the array; in
this case, we’re splitting on the comma and returning a tuple. Once we have a
list of tuples, it’s trivial to convert that to a map.

### Thoughts

This second day of Scala study was less daunting than the first. I was
expecting to dodge a curve ball with the functional side of Scala, but aside
from syntax it’s not too different from Ruby or Io.

What I’ve been noticing is that the somewhat flexible syntax often leads to
one-liners, albeit hard-to-read, 100+ character long one-liners. From reading a
bunch of Stack Overflow answers, it feels almost idiomatic for Scala to be
written as “heiroglyphic syntax soup”, which is either acceptable and takes
getting used to, or not acceptable.

I think I’ll try to really make a point of formatting my Scala code neatly. I
wouldn’t want to [lose any dedicated
developers](http://stackoverflow.com/questions/1722726/is-the-scala-2-8-collections-library-a-case-of-the-longest-suicide-note-in-hist).

<a class="previous-post" href="/seven-languages/scala-day-one">« Scala: <i>The Castle on the Hill</i></a>
<a class="next-post" href="/seven-languages/scala-day-three">Scala: <i>Cutting Through the Fluff</i> »</a>
