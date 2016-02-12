---
title: Long Live The Realign
date: 2016-02-11
excerpt: This blog has migrated from Jekyll to Hakyll, and from GitHub Pages to Surge. Here's a look at the motivations for the switch, and the hacker toys running under the hood.
tags: haskell
---

<span class="run-in"><span class="drop">A</span>nother year</span>, another site
~~redesign~~ realign. This time around I've used a number of new tools and
services for both practical and philosophical reasons. All of the tools I'm
using are relatively self-contained, so you can pick and choose pieces to add to
your own site without worrying about becoming married to some particular
workflow.

1. [Motivation]
    1. [Breaking Changes]
    2. [A Software Monopoly]
    3. [Growth & Development]
2. [Design]
3. [Technology]
    1. [From Jekyll To Hakyll]
    2. [Keeping Secrets]
4. [Publishing]
    1. [Deployment Quick And Simple]
    2. [Performance For Free]

## Motivation

There were a few motivations for this latest version; I promise I wasn't just
being fickle. Possibly the biggest driver of the change were things breaking in
the new blogging system powering GitHub Pages.

### Breaking Changes

GitHub's recent major version bump of Jekyll — the static site generator that
builds all the sites for GitHub Pages — left me feeling jaded after the
[behaviour of permalinks changed][github]. One of my articles were sitting on
the front page of Hacker News, and was also receiving some traffic from Reddit
and a couple other sites, when suddenly: *all my links turned to 404s!*

In GitHub's/Jekyll's defence, the new permalink behaviour is technically more
correct than the previous behaviour. My permalink scheme omitted trailing
slashes, and Jekyll would automatically add them in. Google, Hacker News, and
all the others would link to my site with the trailing slash, and after the
version bump, those external links are no longer valid. There are several other
breaking changes in Jekyll 3.x — like fenced code blocks no longer rendering
correctly — but I found the broken URLs most egregious.

### A Software Monopoly

From a more philosophical perspective, I'm growing disenchanted with GitHub as a
company. In much the same way as I'm not cool with relying on the world's
largest advertising service hosting my email, I'm suspicious of GitHub — a
company that pushes transparency as a software ethic yet is anything but
transparent as an organisation. This is only reinforced by recent news that
they're terminating early hires who helped build the company from the ground up,
and abandoning the flattened "meritocracy" in favour of a more traditional
hierarchical structure.

### Growth & Development

Finally, a constructive and practical reason: I want to surround myself with the
Haskell ecosystem. I find that the immersion method works for programming
languages just as well as it does for natural languages. If I'm ever going to
achieve fluency, I need to stop flirting with the language and just dive in
face-first.

## Design

The design goals for this version of the site are not so different from the
previous version; I want people to come here and read what I have to say rather
than admire pretty colours and animations. The content is front and center, and
I'm optimising for legibility as best I can. In terms of typography, I borrowed
ideas from the New York Times, modern Swiss design, [Mechanical
Elephant][mechelephant], [Sudophilosophical][sudophilosophical], and [Jon Tan's
old website][jontan].

The previous site design wasn't *bad*, but it wasn't ideal either. I liked the
typography, but the entire layout felt narrow and cramped in places. Rather than
redesigning totally from scratch, I took the best of what I had, and realigned
the rest.

> ["Good Designers Redesign, Great Designers Realign."][cameronmoll]

There were practical constraints I had to work within; perhaps the most
prominent of which is the need to strike a balance between paragraph
line-lengths being short enough to be read easily, while allowing pre-formatted
code blocks to display up to 80 columns without creating a horizontal
scroll-bar. A few readers also had difficulty finding the RSS subscription links
in the previous version when they were hidden away down in the footer.

In an effort to push this new version out the door quickly, I'm yet to create
responsive style rules, though this will appear soon enough. Sorry, mobile
telephone readers!

## Technology

In much the same way that my design choices were more conservative this time
around, I also opted to use fewer technologies. Yes, I'm using Haskell, and this
may be a case of using a cannon to kill a mosquito, but in the eloquent words of
[Steve Losh][stevelosh]:

> “Processor time is cheap and my time isn’t.”

### From Jekyll To Hakyll

As I mentioned earlier, I switched from the Ruby-based Jekyll to the
Haskell-based Hakyll for statically generating all of the pages of the site. I
largely cargo-culted together my Hakyll configuration, though save for
pagination I have just about all the features I could want already.

Hakyll was chosen because I want to force myself upon Haskell, and all of the
tooling that exists around it — which for some reason is often ignored in the
language tutorials. To create my development environment and manage all of my
dependencies, I use Nix in almost exactly the same way as I described in my
article about [deploying a Haskell web service][yesodtutorial].

### Keeping Secrets

Although the site is mostly [open-source][opensource], I'm not comfortable with
allowing the public to access my unfinished drafts. Previously, I solved this by
just not committing drafts to the repository, though this is a weak approach for
obvious reasons.

This time around, I'm using a Git extension called [git-crypt][gitcrypt] to
store encrypted versions of my unfinished work. By forcing myself to encrypt
files I store with version control, I'm also in some way encouraging myself to
start encrypting my email too. I used the excellent [PGP and You][pgpandyou]
tutorial to get up and running with GPG.

## Publishing

My publish needs are simple: It must be fast, and it must be free. Otherwise
what good is the Internet.

### Deployment Quick And Simple

As I alluded to earlier, I don't like the monopoly on software projects that
GitHub has established, and I want to gradually rely on their services less and
less. My friend [Radosław Karpowicz][vulnsec] suggested I try a service called
[Surge][surge] which provides dead-simple deployments of static sites to a CDN
for free, and I am so far quite pleased with the hassle-free experience.

### Performance For Free

Even with a static site, I worry that my pages won't be served quickly if the
site is hammered with requests — like for example, if my site ends up on the
front page of Hacker News again. I used the [Load Tester Tool][loadtester] from
*Richard's Toolbox* to simulate a flurry of requests over a duration of one
minute, and sadly the response time averaged around half a minute. Switching my
DNS to CloudFlare magically brought the average response time down to 170ms, and
they give you use of an SSL certificate *for free!*. Can't do better than free.

[github]: https://github.com/jekyll/jekyll/issues/4440
[mechelephant]: http://mechanical-elephant.com/
[sudophilosophical]: http://sudophilosophical.com/
[jontan]: http://v1.jontangerine.com/about/
[cameronmoll]: http://alistapart.com/article/redesignrealign
[yesodtutorial]: https://jezenthomas.com/deploying-a-haskell-web-service-with-nix/
[opensource]: https://github.com/jezen/jezenthomas.com
[vulnsec]: https://vulnsec.com/
[surge]: https://surge.sh/
[loadtester]: http://loadtestertool.com/
[gitcrypt]: https://github.com/AGWA/git-crypt
[stevelosh]: http://stevelosh.com/projects/stevelosh-com/#implementation
[pgpandyou]: https://robots.thoughtbot.com/pgp-and-you
