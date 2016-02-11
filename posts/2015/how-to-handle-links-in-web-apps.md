---
title: How To Handle Links in Web Apps
date: 2015-07-19
excerpt: A simple fix for Apple’s broken link behaviour in iOS home-screen web applications.
tags: javascript, rails
---

If you’ve tried developing a home-screen/mobile web application for iOS, you may
have run into an awkward issue where tapping any link in the application will
switch the active application over to Safari and open the link there instead.

This issue is at least five years old — and yes, Apple, it is a *bug*, not a
feature.

To fix this, we need to hijack the default link behaviour and instead change the
page location with JavaScript. There are however a few minor caveats:

- Links that are intended to be opened in a new tab/window should be ignored
- If the user explicitly wants to open the link in a new tab, *e.g.*, by holding
  the ⌘ key, we should respect that
- We should ignore a link if the behaviour is already hijacked somewhere else,
  *e.g.*, the way Ruby on Rails uses a combination of links and forms to emulate
  `DELETE` requests

With that said, here is the simple snippet of CoffeeScript I use in my Rails app
to handle all anchor elements:

~~~coffeescript
$ ->

  # This prevents links from opening in Safari when the site is being run as a
  # home-screen web-app on iOS
  openLink = (event) ->
    # Some libraries like $.chosen use an anchor element to build components.
    # If you click a $.chosen dropdown, it won’t have an href and we’d end up
    # redirecting the user to `undefined`
    return unless $(@).attr("href")
    return if event.altKey or event.ctrlKey or event.metaKey or event.shiftKey
    return if $(@).attr("target") is "_blank"
    # In Rails apps, an anchor might have a data-method attribute if it should
    # be handled by jquery-ujs. Instead of acting as a normal anchor,
    # jquery-ujs will append a form to the page and submit it.
    return if $(@).data("method")
    event.preventDefault()
    window.location = $(@).attr("href")

  $(document).on "click", "a", openLink
~~~

*N.B.* This is a good example of how to correctly use comments in code. The
comments don’t describe *what* the code does; the code itself is easy enough to
understand. Instead, the comments describe *why* we have to jump through these
hoops, which isn’t immediately obvious.

Depending on your application, you may need to add more conditional rules to the
above function.

Oh, and Apple: Fix your fucking software already.
