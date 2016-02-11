---
title: A Better Embedded Map
date: 2014-05-22
description: My simple solution to a common usability problem.
excerpt: Combining page scrolling with map zooming is an annoying and tricky UX problem. Here’s a simple idea that could be a good workaround for both touch and cursor devices.
tags: javascript, ux
---

So here’s the problem: you’re scrolling a web page with a mouse or trackpad,
and your cursor lands in the middle of an embedded Google Map. Suddenly your
scroll has been hi-jacked and instead of navigating the page, you’re zooming in
and out of whichever city the map happens to have been focused on.

It’s a similar story on a touch device; you encounter an embedded map tall
enough to cover the entire height of your device and short of refreshing the
page or closing the browser, you’re stuck in map-panning mode and there’s no
way to scroll away from it.

A common solution is to completely disable gesture-based zooming and panning,
but it’s at a fairly obvious cost. Compared with the map behaviour we have all
become accustomed to, using buttons to navigate your way around a map is rather
awkward.

I propose all embedded maps come with a focus toggle. You press some button,
and the map starts listening to mouse-scrolling events and touch-device
gestures. Press the button again to revert to a static map. On devices with
smaller screens, I think it’s totally reasonable to expand the map to fit the
screen, as long as a clearly labelled ‘dismiss’ button is visible.
