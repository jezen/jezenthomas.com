---
title: JavaScript Is Not Expressive
date: 2016-05-28
excerpt: Some fuel to stoke the flames of the language wars.
tags: javascript, code
---

Rant Mode: Engaged.

The word “expressive” is often bandied about when describing the features of a
language. In the same way that every new CSS framework that hits the front page
of Hacker News is “light-weight”, “modular”, and “modern”, everyone's favourite
language will be at some point described by them as *expressive*.

Whether or not a language is expressive is a relative measure. To be expressive
is to effectively convey thought or feeling. There are some languages that are
so expressive that you immediately understand the author's intent without
needing to know the syntax. Take the following lines of Ruby, for example:

```ruby
# "How long ago?"
10.years.ago

# "I expect my list of car brands to include the best one"
expect(car_brands).to include "Porsche"
```

I feel confident offering up those lines of Ruby for debate because they are
indeed idiomatic. I want to also argue that Haskell is an expressive language
(and it most certainly is), but I concede I would be cherry-picking from the
nice parts and there is indeed some syntax to learn first.

The current darlings of the JavaScript world are React and Redux. Both of these
libraries are built on solid functional programming ideas which is most
appreciated in the insanity that is browser programming. Perhaps because this is
relatively new territory, it seems any tutorial or open-source example you find
of a Redux project follows a few of the same conventions. The convention that
irks me the most is the practice of defining a constant who's value is the
stringified version of the name of the constant. If that sounds awkward, it's
because it is. Here's how that looks:

```javascript
export const ADD_TODO = 'ADD_TODO'
export const DELETE_TODO = 'DELETE_TODO'
export const EDIT_TODO = 'EDIT_TODO'
export const COMPLETE_TODO = 'COMPLETE_TODO'
export const COMPLETE_ALL = 'COMPLETE_ALL'
export const CLEAR_COMPLETED = 'CLEAR_COMPLETED'
```

What thought, feeling, or idea is the preceding snippet of code trying to
convey? We can clearly see that we have six different things here, that in a
better language might be expressed as a list of six things.

All of the noise is a result of having to work around failings of the language.
We're telling the language how to do its job, whereas we should only be telling
the language what we want. How might that work in a more expressive language?
Well, take Haskell, for example:

```haskell
data Action = AddTodo
            | DeleteTodo
            | EditTodo
            | CompleteTodo
            | CompleteAll
            | ClearCompleted
```

Here we can see that the language allows us to boil down our code to just the
ideas we're trying to convey. It also doesn't hurt that GHC will check that all
our action names match up, but that's a different topic.

Why does Redux have the crazy `export const…` convention anyway? Am I the only
one who is uncomfortable with this? Turns out, [I am not][so]. The
justifications offered up essentially come down to "all action names in one
place", and "please protect me from typos". If the problem really is one of
organisation and you want all your action names in one place, then why not an
array of strings?

If the use of `const` is to convey intent, couldn't that intent also be
communicated with documentation? Or tests? If the argument against documentation
and/or tests is that eventually they will become untrue, then perhaps it's time
to drop JavaScript in favour of some language which actually provides the
correctness guarantees you seemingly crave?

I've heard some of the more prominent JavaScript advocates advise against the
use of `const` because although the intent its supposed to convey is “this value
shall not change”, it actually has nothing to do with immutability. Although
`const` will prevent a developer from assigning some other value, the original
value can still be mutated.

Perhaps JavaScript is expressive in the sense that it's effective in conveying
contempt for other programmers.

One argument to prove that JavaScript is *expressive* is that the language
allows you to achieve so much with so few keywords. For example, [the `function`
keyword is a function, as well as a method, a class, and a lambda][bob]. But
should it be? I would argue that this is another example of JavaScript
developers shoe-horning features onto a language that doesn't really support
them. In the same way that you *could* build an e-commerce site with WordPress:
it's possible, but there are less painful ways you could live your life.

Upon searching for other people's thoughts on whether or not JavaScript is
expressive, I stumbled on this line by Douglas Crockford:

> Most of the people writing in JavaScript are not programmers. They lack the
> training and discipline to write good programs. JavaScript has so much
> expressive power that they are able to do useful things in it, anyway.

I call bullshit on that. Amateurs are not able to do useful things because
JavaScript is *expressive*. They get things done because it's the *lingua
franca* of the Internet and there are an abundance of snippets available online
for them to cargo-cult. And that cargo-culting culture is exactly what permeates
through tutorials and examples and leaves everyone [maintaining convoluted
reimplementations of build systems we had decades ago][twmbbs].

I'm not exactly sure where I'm going with this. To try and wrap this up in a
constructive manner, I'll say that if you think JavaScript is expressive and
your prior experience is only with PHP or Java, then please continue exploring.
There's a world of great stuff out there which is just so much better.

I will say that JavaScript is many things, but it is not *expressive*.

[so]: http://stackoverflow.com/questions/34965856/what-is-the-point-of-the-constants-in-redux
[bob]: http://stackoverflow.com/a/2427532/704015
[twmbbs]: https://jezenthomas.com/the-worlds-most-boring-build-system/

