---
title: Rails Assets are AWOL in Production
date: 2014-11-02
excerpt: Having to write code specific to a framework is not cool.
tags: operations, rails
---

I’m working with Oddjob’s deployment workflow, and one of the first hurdles I
ran into was that assets (CSS, JavaScript, images) weren’t found in production.
I’m surprised that a framework who’s primary selling point is convention over
configuration and just working out of the box, *doesn’t* just work out of the
box in production.

After searching around, I discovered a simple fix for serving the CSS and
JavaScript assets is to include the
[12factor](https://rubygems.org/gems/rails_12factor) gem. I don’t know exactly
what it does behind the scenes and I feel uneasy pulling more code (and
implicit complexity) into the project to fix something that should never have
been broken.

Fixing the images were a slightly different issue. I noticed that in
production, Rails’ ‘Asset Pipeline’ adds cache-busters to all of the images,
which is the right thing to do. It doesn’t use query strings — which was once
The Right Way™ but is now known to be The Old Way™ — so it’s following best
practice there too.

The problem is, all the image paths have to be rewritten in some compile step
to match the new cache-busting filenames. Rails is not smart enough to
recognise a relative asset path in a stylesheet and rewrite it. Instead, you
have to change your native, *portable* style rules into a Rails-specific
bastardisation.

~~~sass
// Normal style rule. Works everywhere.
.normal_element
  background: url(unicorn.png)

// Rails-specific. Also won’t work without quotes.
.rails_element
  background: asset_url('unicorn.png')
~~~

This is a minor pain practically, and a wisdom-tooth philosophically. It’s one
example of Rails tightly coupling your otherwise agnostic code to the
framework.
