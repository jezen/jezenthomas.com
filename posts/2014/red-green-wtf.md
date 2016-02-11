---
title: Red, Green, WTF?
date: 2014-05-08
description: The Problem with Most TDD Literature
excerpt: In order to reason about any particular concept, the concept needs to be analysed at a reasonable level of abstraction. Here’s what we’re getting wrong when introducing test-driven JavaScript development.
tags: javascript, tdd
---

Test-Driven Development is gradually working its way into the front-end
development ecosystem, but I don’t feel the adoption rate is as high as it
probably should be. There are a couple of reasons for this; or rather, the two
reasons I can think of are sides of the same coin.

I’ve been consuming my fair share of TDD-flavoured literature lately, all
written and recorded by the revered and outspoken thought-leaders in this
space. It’s funny; I could swear they all copy each other’s prose. I’m
particularly amused by the phrase “reason about”, as in: “…blah blah blah, I
find this approach makes the concept in question easier to *reason about*”.

The interesting thing here is that in order to *reason about* any particular
concept, the concept needs to be analysed at a *reasonable* level of
abstraction. Most of the TDD discussion tends to fall into one of two levels of
abstraction:

1. An overly-simplified *Hello, World!* syntax example, or:
2. Dry academia involving diagrams of boxes with arrows pointing to other boxes

A typical ‘Hello, World!’ example of using a test framework like
[Jasmine](https://github.com/pivotal/jasmine) will look something like this:

~~~javascript
describe('Hello World', function() {
  it('should return hello world', function() {
    expect(helloWorld()).toEqual('Hello World');
  });
});
~~~

What does any of that mean? From the outset, these five lines are confounding.
In the first line, we say we are describing a ‘Hello World’. What is a ‘Hello
World’? It doesn’t even sound like a noun! In this case, the fact that the
example is nonsensical actually makes the concept *more difficult* to reason
about.

Empathise with your fellow developer. They’re thinking: “Well, I have this web
form, and some JavaScript that validates the input fields; I wonder how I might
test that my JavaScript does what I intend.” In which parallel universe does
writing a function that returns the string ‘Hello World’ have any use to our
friend writing form validation functionality?

If you’re explaining TDD, starting with an unrealistically simple example
steepens the learning curve.

The academic types sometimes go too far the *other* way. An article on Test
Doubles opens with an explanation of stunt doubles and their role in the film
industry. What follows is an analogy between stunt doubles and Test Doubles.
You could argue for or against that connection, but whilst you’re pondering the
difference you’re *not* thinking about how this applies to your codebase.

I’m convinced the two are equally discouraging to a TDD beginner. Being forced
to draw parallels between your codebase and example code with no realistic
use-case is an unnecessary added complexity when you’re trying to understand
how and *why* to write unit tests for that code.
