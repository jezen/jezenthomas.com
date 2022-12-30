---
title: Working In The Dark
date: 2016-03-06
location: Gdynia, Poland
excerpt: It's not uncommon to work unsociable hours, be that because of deadlines, midnight inspiration, or just skipping a few timezones. But that doesn't mean your eyes have to suffer. Here's how I protect mine.
tags: productivity
---

<span class="run-in"><span class="drop">I</span> often find myself</span>
working on my computer in dark environments. Sometimes I'll be writing code
late into the evening or typing up an article on a long-haul flight. The
blinding difference in luminosity created by the computer screen is an
excellent way of killing your eyesight. It's also a great way to piss everyone
off around you when they're trying to get some sleep.

To mitigate this problem, I use two tools: [Flux][flux] and [iTerm][iterm].

## Flux

Flux is a tool for controlling the warmth of the light emanating from your
computer display. I believe that unless you're a graphic designer and need to
see accurate colours, everyone should be using this tool.

The primary utility of Flux is to make the light from your screen less blue so
it doesn't screw with your natural sleep patterns. In total darkness though,
this alone is not enough to curb screen brightness and protect your eyes.
Instead, I use a neat little feature of Flux called 'Darkroom'. The Darkroom
feature emulates a traditional photograph development room. Compare the
following photos…

![Without Flux, the screen is too bright and kills my eyes](/static/img/without_flux.jpg)

The same shot with Darkroom enabled…

![Enabling Darkroom means you can safely view light-background content at night](/static/img/darkroom.jpg)

## iTerm

The Darkroom feature takes care of viewing anything with a light background,
but what about anything with a dark background? My terminal background is black
and the window is full-screen, so viewing it with Darkroom enabled means I see
a screen-full of bright red.

Luckily, making the terminal play nicely with Flux's Darkroom is simple enough
with a couple of settings. After enabling Darkroom, I open the preferences pane
for iTerm, and navigate to Profiles > Colors. At the bottom of the pane, there
is a dropdown menu labelled ‘Load Presets...’. Selecting ‘Light Background’
from the menu takes care of most of that colour problem.

There are a couple of further minor tweaks that I make for working in Vim (which
is where I spend most of my time).

```
set bg=light
set nocursorline
```

And just like that, no more sore eyes!

[flux]: http://justgetflux.com
[iTerm]: https://iterm2.com/
