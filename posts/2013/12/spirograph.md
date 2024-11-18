---
title: Spirograph
date: 2013-12-09
location: Göteborg, Sweden
description: If you look long enough, it turns into a squirrel
excerpt: A visualisation experiment in Sass using loops and animation.
tags: css
---

I wanted to create a visualisation by generating style rules with a loop in
Sass. Making art like this is pretty simple; you draw a line, and adjust it
slightly at regular intervals with the loop. The animation is slightly off-sync
relative to the position of each respective line, which creates the effect of
the shape evolving through many interesting patterns.

<div id="spirograph">
  <ul>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
    <li></li>
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

The key to doing this in a reasonable way is to use pre-processors like Haml
and Sass to generate the markup and styles with some simple loops.

The Haml code generates an unordered list with 90 list items inside.

```haml
%ul
  -(1..90).each do
    %li
```

The Sass code uses an only slightly more complicated loop with some variable substitution and basic arithmetic to displace each list item by a regular interval.

```sass
@for $i from 1 through 90
  li:nth-child(#{$i})
    transform: rotate($i*4+deg)
    animation: rot-#{$i} 3000s infinite

  @keyframes rot-#{$i}
    100%
      transform: rotate($i*360deg)
```
