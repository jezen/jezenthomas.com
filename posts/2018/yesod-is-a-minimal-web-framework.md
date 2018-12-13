---
title: Yesod is a Minimal Web Framework
date: 2018-12-13
location: London, UK
tags: haskell
---

Software developers have a natural affinity for minimalism. This manifests
itself in a number of ways. It might be removing unnecessary work to make some
code path more performant. It might be an effort to adhere to the Single
Responsibility Principle. It might be elegance. It might be a Divide and
Conquer strategy to reduce the programmer's cognitive load.

It's no surprise then that programmers would be naturally drawn to libraries
and frameworks that are billed as _minimal_. But simply stripping things away
is not the essence of minimalism.

Minimalism is about being deliberate in your choice of things to keep ownership
of. It's about consciously choosing what you would like to control, and what
you would like to delegate away.

In the Haskell world, we have a decent selection of web frameworks. Yesod is
typically recognised as one of the more _batteries included_ frameworks, and
this becomes a reason for many developers to avoid it. The sentiment is usually
"Oh, I'm just throwing together a simple JSON API. I don't need any of the
other stuff it provides."

I think this sentiment is short-sighted. There are a number of foundational
components to any web application that you will need regardless of how complex
you anticipate your project will be. A great example is logging. Every web
application needs decent logging, and unless logging is an interesting part of
the software you're writing — _spoiler alert: it probably isn't_ — there's no
real value in starting without that component and adding it afterwards.

Yesod also provides templating, application state, routing, and potentially
database access if you choose to scaffold your project that way. It's always
easier to start with some reasonable defaults for these mundane aspects of your
system and later strip away what you don't need, than it is to start with
nothing and gradually piece together your grand architecture.
