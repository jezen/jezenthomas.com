---
title: Don't Skip HTML Validation
date: 2025-05-20
location: Varna, Bulgaria
excerpt:
  We obsess over complex testing strategies but often forget to check if our
  HTML is even valid. Here’s why that matters—and how I built a tool to fix it.
tags: code
image: dont-skip-html-validation/cover.jpg
---

You’re probably already using a few tools to automatically check your
software — like linters, automated tests, code formatters, or a type-checking
compiler.

These are all great, and we use them at Supercede too. The more checks we can
run, the better. And the earlier in the development process, the better.

What I've noticed over the years though, is that we tend to focus on
more advanced, often esoteric forms of analysis. With training and
practice, we can express our application's logical constraints in the type
system, or verify properties of functions with property-based testing.

Yet with all this sophistication, we often forget to check whether the HTML
we're generating is even valid.

Producing invalid HTML has bitten us in the past. Fixing the HTML is usually
easy, but until it's fixed you might have a broken page. If you're lucky, it'll
be broken in an obvious way. Sometimes it might be broken in a subtle way,
which is harder to debug (which means it costs your team more money).

We've had online HTML validators like [Nu HTML Checker][0] for ages. You
can paste your HTML in there and immediately validate your HTML for free.

This does get tedious though. You (or your colleagues) won't remember to do
this every time you update a page. And having a human do this manually just
doesn't scale.

I'm writing a web application which validates pages across entire websites,
automatically. It uses both a crawl-based and a push-based approach, which
means it can validate pages that are behind a login screen.

Here's a screenshot of how it looks currently.

![A dashboard for a website showing aggregated validation statistics.][1]

I'm planning to launch this as a public tool soon.

Interested in trying it? Drop your email below and I'll let you know when it's
ready.

<form class="html-validator-lead" action="https://formspree.io/f/xldbvpeg" method="POST">
  <label for="email-input">Your email</label>
  <div class="input-row">
  <input id="email-input" type="email" name="email" required>
  <button type="submit">Send</button>
  </div>
  <input type="hidden" name="signup" value="Show me your HTML validator">
</form>

[0]: https://validator.w3.org/nu/
[1]: /static/img/dont-skip-html-validation/screen.png
