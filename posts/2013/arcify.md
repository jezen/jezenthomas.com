---
title: Arcify
date: 2013-08-08
description: You spin me right ’round, baby
image: arcify
excerpt: Animating the border-radius property with CSS3 for fun and profit.
tags: css
---

I borrowed the idea from a design that I came across on
[simpledesktops.com](http://simpledesktops.com/browse/desktops/2012/feb/14/arcify/),
by this cool hacker guy [Adam Bachman](https://twitter.com/abachman). It
consists of a number of different coloured arcs of varying lengths. I decided
to try and bring it to life with some animation.

Each arc is just a list-item in an unordered list. The part of the element
you’re seeing is the border. I curved the borders with `border-radius`, and
then just removed a few sides of the borders by making their colour
transparent. The animation is just random degrees of rotation in keyframes,
with an infinite transition time.

<p data-height="340" data-theme-id="477" data-slug-hash="CAHsk" data-default-tab="result" data-user="jezen" class="codepen">See the Pen <a href="http://codepen.io/jezen/pen/CAHsk/">Arcify</a> by Jezen Thomas (<a href="http://codepen.io/jezen">@jezen</a>) on <a href="http://codepen.io">CodePen</a>.</p>
<script async src="//s.codepen.io/assets/embed/ei.js"></script>
