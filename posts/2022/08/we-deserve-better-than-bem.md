---
title: We Deserve Better Than BEM
date: 2022-08-24
location: Odessa, Ukraine
excerpt:
  A simple approach to composing web page components using a compiler and a
  monadic action. It's easier than BEM, and it works better too.
image: conductor.jpg
tags: haskell
---

The past decade has seen a number of CSS methodologies come and go in web
development. The approach that seems to have become most widely adopted is
called Block Element Modifier (BEM).

BEM aims to make user interface code more manageable by ignoring the
_cascading_ part of Cascading Style Sheets, keeping the specificity of
selectors low, and avoiding naming collisions. It does this by imposing a
naming convention which programmers should rigidly adhere to.

Applied BEM looks something like this:

```html
<header class="header">
  <img class="header__logo header__logo--success" src="/slava_ukraini.jpg">
</header>

<style>
  .header {
    background: linear-gradient(-180deg, royalblue 50%, yellow 50%);
  }

  .header__logo--success {
    position: absolute;
  }
</style>
```

The _block_ in this case is `header`. We delimit the block and the _element_
with a couple of underscores, and appending two hyphens and some string denotes
a modifier.

In practice, not only does this turn into a mess of punctuation, but this
approach is also rather fragile.

There's nothing but human discipline to stop you from accidentally mistyping a
selector, or inadvertently introducing a naming collision. When the visual
design of your software changes and some markup is removed, there is nothing to
indicate that the associated styles are now dead code and can safely be
removed.

Fundamentally, I don't believe that relying on human discipline is a sensible
way to scale a software project. We deserve better. Computers are perfectly
capable of managing the relationships between components of a software system,
whether that's some markup and its associated styles and scripts, or classes
and functions and the data types that flow between.

The tools are available to us. We just need to use them.

At Supercede we're leveraging a technique which I think scales better than BEM.
The technique is facilitated by Yesod which is the Haskell web framework that
we use, but there's no reason why this technique couldn't be recreated in other
frameworks.

Approaching the previous `header` component in Yesod would look like this[^1]:

```haskell
header :: Widget
header = do
  theId <- newIdent
  [whamlet|
    <header id="#{theId}">
      <img class="logo success" src="/slava_ukraini.jpg">
  |]
  toWidget [cassius|
    ##{theId}
      background: linear-gradient(-180deg, royalblue 50%, yellow 50%)
      .logo.success
        position: absolute
  |]
```

The interesting part here is the use of `newIdent`. This is a monadic action
which will bind `theId` to an identifier which is guaranteed to not collide
with any other identifiers on the page which are generated the same way.

This works by maintaining a counter in some request-specific internal state. At
runtime when a request comes in and the application begins building up the page
to serve to the user, every run of the `newIdent` action asks the state for the
current count. The count is then incremented and stored back in the state, and
the new count is used to generate a unique identifier.

Composing a couple of widgets together illustrates this effect.

```haskell
myWebPage :: Widget
myWebPage = do
  theId <- newIdent
  [whamlet|
    <div id="#{theId}">
      ^{header}
      ^{footer}
  |]
  toWidget [cassius|
    ##{theId}
      margin: auto
  |]

  where

  header :: Widget
  header = do
    theId <- newIdent
    [whamlet|
      <header id="#{theId}">Some header text…
    |]
    toWidget [cassius|
      ##{theId}
        background: royalblue
    |]

  footer :: Widget
  footer = do
    theId <- newIdent
    [whamlet|
      <footer id="#{theId}">&copy; Copyright Acme Inc. 2022
    |]
    toWidget [cassius|
      ##{theId}
        background: yellow
    |]
```

This Haskell code generates markup and styles which look like this:

```html
<div id="hident3">
  <header id="hident1">Some header text…</header>
  <footer id="hident2">&copy; Copyright Acme Inc. 2022</footer>
</div>

<style>
  #hident1 { background: royalblue; }
  #hident2 { background: yellow; }
  #hident3 { margin: auto; }
</style>
```

So, correct use of the framework ensures our IDs are unique even when they're
all composed together, and the compiler ensures we don't mistype the names that
bind the HTML elements with their associated CSS and JavaScript. The compiler
also helps us clean up dead code along the way.

Ordinarily you might feel uncomfortable using a name as nondescriptive as
`theId`, but in practice we tend to only use one unique identifier per widget.
Each binding is scoped to its respective widget, and the name
doesn't appear verbatim in the generated HTML, CSS, or JavaScript.

This approach also allows us to improve the _locality_ of the code. Associated
markup, styles, and scripts can all be written in adjacent code which makes the
writing process less cumbersome.

Because naming collisions are prevented by the application, we're free to embrace
specificity in CSS and just use IDs everywhere instead of limiting ourselves
to classes. Relying on IDs in web development is usually avoided because
they're hard to manage manually at scale, but this pain goes away if you can
delegate that management to a sufficiently sophisticated mechanism.

Widget composition is one of the unsung heroes of the Yesod framework. Given
that the framework is written in Haskell, I think people are often more
interested in talking about exotic-sounding ideas like _zygohistomorphic
prepromorphisms_, but the truth is that more basic concepts make up the vast
majority of web development work, so those are the areas we should be focusing
on. The composition of widgets and their associated styles and scripts is
exactly one of those areas.

I've found that the more responsibility I delegate away from humans and into
the compiler, the fewer mistakes I tend to see in production. The mechanisms
I've described here aren't exactly new either — they've existed in Yesod for
more than a decade. If your web framework of choice doesn't have something like
this, you should be asking yourself an important question: _Why not?_

[^1]: Actually, it's more typical to use [syntactical conveniences][0] that Hamlet provides. The verbose version is used here so there is less unfamiliar syntax for the reader.

[0]: https://www.yesodweb.com/book/shakespearean-templates#shakespearean-templates_attributes
