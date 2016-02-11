---
title: Scala Day Three
date: 2014-04-22
description: Cutting Through the Fluff
excerpt: Using Scala’s actors for concurrency, querying and dissecting XML, and a quick look at pattern matching.
tags: scala
---

The third day of Scala studies gives me a luke-warm feeling. Since *Seven
Languages in Seven Weeks* was written, Scala’s actors have been deprecated in
favour of using [*Akka*](http://akka.io/), a concurrency library.

Scala treats XML as a first-class construct, but I find it hard to care about
that. Given my JavaScript background, I generally avoid XML where possible in
favour of JSON. XML was popular and useful in the dinosaur age of the Internet,
but since JSON rose to ubiquity I think XML (and by effect, Scala) seems dated.

<div id="toc"></div>

1. [XML parsing and concurrency](#xml-parsing-and-concurrency)
2. [Thoughts](#thoughts)

### XML parsing and concurrency

> Take the sizer application and add a message to count the number of links on
> the page.

The first issue I faced was a problem with encoding. Java/Scala seems to
struggle when reading an input stream with an unexpected encoding. I’m not
exactly sure, but I think Scala was expecting a UTF-8 stream because my Scala
file has that encoding. For half of the URLs I tried, the compiler threw this
exception:

~~~scala
scala.actors.Actor$$anon$1@79613135: caught java.nio.charset.MalformedInputException: Input length = 1
java.nio.charset.MalformedInputException: Input length = 1
        at java.nio.charset.CoderResult.throwException(CoderResult.java:277)
        at sun.nio.cs.StreamDecoder.implRead(StreamDecoder.java:338)
        at sun.nio.cs.StreamDecoder.read(StreamDecoder.java:177)
        at java.io.InputStreamReader.read(InputStreamReader.java:184)
        at java.io.BufferedReader.fill(BufferedReader.java:154)
        at java.io.BufferedReader.read(BufferedReader.java:175)
        at scala.io.BufferedSource$$anonfun$iter$1$$anonfun$apply$mcI$sp$1.apply$mcI$sp(BufferedSource.scala:38)
        ...etc
~~~

The `fromURL` method takes an optional encoding parameter which we can use to
mitigate this issue.

~~~scala
object PageLoader {
  def getPageSize(url: String) = Source.fromURL(url, "ISO-8859-1").mkString.length
}
~~~

Finding the number of anchor tags on a given page would have been an excellent
opportunity to make use of Scala’s XML parser, but sadly it’s not robust enough
to handle malformed XML (like real HTML). There are some libraries that
apparently do a better job of parsing HTML, but I didn’t manage to make any of
them work with Scala. At this point, I’ll fall back to using a regular
expression.

For some reason, my parser seems to think there is only one anchor on the
Google homepage, so something is not working but I don’t feel it’s worth
investigating at this point. What we have is close enough. Here’s the method
for finding anchor tags in a HTML string:

~~~scala
def getPageLinks(url: String) = """<a .+<\/a>""".r.findAllIn(getPage(url)).length
~~~

In the end, our final chunk of code looks like this:

~~~scala
import scala.io._
import scala.actors._
import Actor._

object PageLoader {
  def getPage(url: String) = Source.fromURL(url, "ISO-8859-1").mkString
  def getPageSize(url: String) = getPage(url).length
  def getPageLinks(url: String) = """<a .+<\/a>""".r.findAllIn(getPage(url)).length
}

val urls = List("http://www.amazon.com",
                "https://twitter.com",
                "https://google.com",
                "http://cnn.com")

def timeMethod(method: () => Unit) = {
  val start = System.nanoTime
  method()
  val end = System.nanoTime
  println("Method took " + (end - start)/1000000000.0 + " seconds.")
}

def getPageSize() = {
  val caller = self

  for(url <- urls) {
    actor {
      caller ! (url, PageLoader.getPageSize(url))
    }
  }

  for(i <- 1 to urls.size) {
    receive {
      case (url, size) =>
        println("Size for " + url + ": " + size)
    }
  }
}

def getLinkCount() = {
  val caller = self

  for(url <- urls) {
    actor {
      caller ! (url, PageLoader.getPageLinks(url))
    }
  }

  for(i <- 1 to urls.size) {
    receive {
      case (url, size) =>
        println("Number of links on " + url + ": " + size)
    }
  }
}

println("Concurrent run:")
timeMethod { getPageSize }
timeMethod { getLinkCount}
~~~


### Thoughts

I still have mixed feelings about Scala. Some of the ideas feel elegant, and I
think it’s worth incorporating these ideas into my work in other languages. One
of Scala’s supposed strengths is its extensibility; everything is available in
libraries. I’m sure this is a good thing for a number of reasons, but it
doesn’t feel user-friendly to me. The language feels possibly too big, with too
many idioms. I guess I’ll have to learn to love it.

<a class="previous-post" href="/seven-languages/scala-day-two">« Scala: <i>Clipping Bushes and Other New Tricks</i></a>
<a class="next-post" href="/seven-languages/prolog-day-one">Prolog: <i>An Excellent Driver</i> »</a>
