---
title: Io Day Three
date: 2014-01-27
description: The Parade and Other Strange Places
tags: io
---

In the third and final day of Io, we looked at writing Domain Specific
Language, and also how Io handles concurrency with coroutines, actors, and
futures. I predict this will come in handy when I eventually look at Scala.

<div id="toc"></div>

1. [Things to do](#things-to-do)
  1. [Add indentation support to XML](#add-indentation-support-to-xml)
  2. [Add array-literal support](#add-array-literal-support)
  3. [Add object-literal support](#add-object-literal-support)
2. [Thoughts](#thoughts)

### Things to do

#### Add indentation support to XML

> Enhance the XML program to add spaces to show the indentation structure.

First, we’ll take a look at the code we already have to work with.

```io
Builder := Object clone
Builder forward := method(
  writeln("<", call message name, ">")
  call message arguments foreach(
    arg,
    content := self doMessage(arg);
    if (content type == "Sequence", writeln(content))
  )
  writeln("</", call message name, ">")
)
```

I solved this problem by keeping track of the current indentation level in an
external counter. I then pass this “depth count” to a method that creates an
appropriately sized string of spaces, and I print those spaces before each tag,
or before the tag contents. Here follows the complete script:

```io
Builder := Object clone
Builder depth := 0
Builder indent := method(
  depth,
  indentation := ""
  depth repeat(indentation = indentation .. "  ")
  return indentation
)

Builder forward := method(
  writeln(indent(depth), "<", call message name, ">")
  depth = depth + 1
  call message arguments foreach(
    arg,
    content := self doMessage(arg);
    if (content type == "Sequence",
      writeln(indent(depth), content)
    )
  )
  depth = depth - 1
  writeln(indent(depth), "</", call message name, ">")
)
```

#### Add array-literal support

> Create a list syntax that uses brackets.

My first approach was to try adding `[` to the `OperatorTable`, but Io kept
throwing exceptions — `Object does not respond to 'squareBrackets'`. *Aha!* Io
already understands that brackets have some sort of special meaning. While
searching for some documentation on `squareBrackets` (which I couldn’t find in
the official core reference), I found a one-liner implementation for adding
array-literal support.

```io
List squareBrackets := method(call message arguments)
```

#### Add object-literal support

> Enhance the XML program to handle attributes: if the first argument is a map
> (use the curly brackets syntax), add attributes to the XML program.

This has been the most frustrating challenge in the book so far. Not because
the problem itself is difficult, but because no matter how I formed my logic
the same exception was thrown.

```io
Exception: Sequence does not respond to ':'
```

Searching for this exception message turns up a few results, but only from
people battling with this very same problem in this very same book. Apparently,
the `OperatorTable` can’t be modified and used in the same file, unless you
cast your code that uses any custom operators to a string and then evaluate it.

Most of my solution is continued from what I had earlier, mixed with samples
from the book. Some simple string interpolation helps us print out our
attributes in the correct format.

```io
OperatorTable addAssignOperator(":", "parseAttribute")

parseAttribute := method(
  attribute := call evalArgAt(0) asMutable removePrefix("\"") removeSuffix("\"")
  value := call evalArgAt(1)
  " #{attribute}=\"#{value}\"" interpolate
)

curlyBrackets := method(
  call message arguments map(
    arg, 
    self doMessage(arg)
  ) join
)

Builder forward := method(
  args := call message arguments
  name := call message name
  attrs := ""
  if (args first name == "curlyBrackets",
    attrs = doMessage(args removeFirst)
  )
  writeln("<", name, attrs, ">")
  args foreach(
    arg,
    content := self doMessage(arg);
    if (content type == "Sequence",
      writeln(indent(depth), content)
    )
  )
  writeln("</", name, ">")
)

doString(
  """
  Builder ul(
    li({"style": "color: #f00"}, "Foo")
    li("Bar")
  )
  """
)
```

### Thoughts

Given my frustration with the third challenge and the lack of documentation
surrounding it, I’m turned off from writing anything serious with Io. It might
be good for writing small CLI scripts, but I’m not sure why I would choose this
language over Ruby.

Io is praised for how it handles concurrency, but I don’t have much experience
with concurrency anyway so I can’t appreciate what Io offers. Perhaps after
working through the next few chapters I might change my mind.

Studying Io has been a challenge, and after making a whole bunch of U-turns in
my problem solving I realised that if anything I’ve at least been made to think
slightly differently. And that’s still *progress*.

<a class="previous-post" href="/seven-languages/io-day-two">« Io: <i>The Sausage King</i></a>
<a class="next-post" href="/seven-languages/scala-day-one">Scala: <i>The Castle on the Hill</i> »</a>
