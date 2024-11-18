---
title: Arcify
date: 2013-08-08
location: Göteborg, Sweden
description: You spin me right ’round, baby
image: arcify
excerpt: Animating the border-radius property with CSS3 for fun and profit.
tags: css
---

I borrowed the idea from a design that I came across on [simpledesktops.com][0].
It consists of a number of different coloured arcs of varying lengths. I decided
to try and bring it to life with some animation.

Each arc is just a list-item in an unordered list. The part of the element
you’re seeing is the border. I curved the borders with `border-radius`, and
then just removed a few sides of the borders by making their colour
transparent. The animation is just random degrees of rotation in keyframes,
with an infinite transition time.

<div id="arcify">
  <ul>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
  </ul>
</div>

[0]: http://simpledesktops.com/browse/desktops/2012/feb/14/arcify/
