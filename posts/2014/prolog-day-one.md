---
title: Prolog Day One
date: 2014-04-28
description: An Excellent Driver
excerpt: Defining knowledge bases and making simple queries with Prolog&#58; Programmation in Logique.
tags: prolog
---

Beginning Prolog is quite exciting for me. It’s the first declarative
programming language I’ve looked at, and the syntax is far removed from
anything I’ve worked on previously. I like the idea that you don’t describe the
solution to a problem, rather you describe the problem in [pure
logic](https://xkcd.com/1112/).

Setting up my environment was quick and painless. The compiler can be installed
with Homebrew by doing `brew install gnu-prolog`, and we can start the
interpreter by doing `gprolog`.

Prolog files have a `.pl` extension, and I noticed that Vim interprets this
extension as a Perl file. A simple workaround is to begin a Prolog file with a
comment, denoted with `%`. Magic, right?

<div id="toc"></div>

1. [Things to find](#things-to-find)
  1. [Some free Prolog tutorials](#some-free-prolog-tutorials)
  2. [A support forum](#a-support-forum)
  3. [An online Prolog reference](#an-online-prolog-reference)
2. [Things to do](#things-to-do)
  1. [Create a knowledge base](#create-a-knowledge-base)
  2. [Query the knowledge base](#query-the-knowledge-base)
  3. [The anonymous variable](#the-anonymous-variable)
4. [Thoughts](#thoughts)

### Things to find

#### Some free Prolog tutorials

Finding tutorials for Prolog is easy. Most of them seem to exist in university
publications, which I imagine is probably a good thing. Documentation for
technology used by more serious circles has always been more comprehensive in
my experience. Compare that to documentation written by ‘Web Designers’, which
sometimes amounts to “Use jQuery! It’s amazing and does all the things!”.

Here’s a few I found:

 - [Prolog Tutorial at Cal Poly Pomona](https://www.csupomona.edu/~jrfisher/www/prolog_tutorial/contents.html)
 - [Tutorials and exercises from University of London](http://www.doc.gold.ac.uk/~mas02gw/prolog_tutorial/prologpages/)
 - [More tutorials from Dublin City University](http://www.computing.dcu.ie/~jhayes/Logic/)

I also managed to find a collection of [Ninety-Nine Prolog Problems](http://www.ic.unicamp.br/~meidanis/courses/mc336/problemas-prolog/).

![Jay-Z, famed rapper and software developer.]({{ site.url }}/img/jayzprolog.jpg)

#### A support forum

There are ~17k topics in [this Prolog Google
Group](https://groups.google.com/forum/#!forum/comp.lang.prolog), there is a
discussion forum on [PrologCentral.com](http://www.prologcentral.com/forums/),
and there is also a [Prolog mailing
list](http://www.swi-prolog.org/Mailinglist.html).

#### An online Prolog reference

The version of Prolog I’m using is Gnu Prolog 1.4.4, and the reference for it
[can be found here](http://www.gprolog.org/manual/gprolog.html).

### Things to do
#### Create a knowledge base

> Make a simple knowledge base. Represent some of your favourite books and
> authors.

Rather than books and authors, I’ve gone for a music theme with albums and
recording artists. The interesting thing about writing facts in Prolog is that
it reads almost like the English language.

One of my favourite artists is Björk, but it looks as though Prolog doesn’t
support accented characters like `ö` out of the box so the wonderful Ms
Guðmundsdóttir gets excluded.

~~~prolog
% albums.pl

recorded(nine_inch_nails, year_zero).
recorded(nine_inch_nails, hesitation_marks).
recorded(wu_tang_clan, the_w).
recorded(ludovico_einaudi, islands).
~~~

#### Query the knowledge base

> Find all books in your knowledge base written by one author.

To query our knowledge base of recording artists and albums, we start the
interpreter and enter `['albums'].` at the prompt. If you forget to mark the
end of the statement with a full-stop (`.`), nothing will happen and the
interpreter won’t tell you anything, which is quite frustrating.

The knowledge base quickly compiles, and awaits our query. If we ask
`recorded(nine_inch_nails, Album).`, Prolog returns us the first result,
`year_zero`. Hit `a` to view all results.

#### The anonymous variable

> Make a knowledge base representing musicians and instruments. Also represent
> musicians and their genre of music. Find all musicians who play the guitar.

I could create two sets of facts — one for the relationship between musicians
and their instruments, and another for the same musicians and the musical genre
they’re most commonly associated with — but the queries would be essentially
the same as what we saw in the previous problem. This time, we’ll write the
facts with a third predicate.

~~~prolog
% musicians.pl

plays(vinnie_colaiuta, drums, jazz).
plays(michael_landau, guitar, jazz).
plays(danny_carey, drums, metal).
plays(adam_jones, guitar, metal).
~~~

To find all the musicians who play the guitar, I first tried `plays(Who,
guitar).` but the interpreter simply threw an error. I went in search of some
sort of wildcard for Prolog, and dominikh on the ##prolog IRC channel
recommended the *anonymous variable*. With this special symbol, we can run the
query `plays(Who, guitar, _).` and surely enough, Michael Landau and Adam Jones
are found.

### Thoughts

So far, the concept seems intuitive enough to me. I don’t think today’s
exercises are representative of the true power of Prolog, but it was a nice,
gentle introduction.

I still have many questions echoing around in my head, all along the lines of
“But what if I want to do this…”, and “How would I do that…”. The documentation
I’ve read on Prolog so far has mostly been quite dry and academic, and I’d
appreciate a more human approach.

<a class="previous-post" href="/seven-languages/scala-day-three">« Scala: <i>Cutting Through the Fluff</i></a>
<a class="next-post" href="/seven-languages/prolog-day-two">Prolog: <i>Fifteen Minutes to Wapner</i> »</a>
