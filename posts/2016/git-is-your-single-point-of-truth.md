---
title: Git Is Your Single Point Of Truth
date: 2016-08-19
excerpt: Not all project management tools are created equal, and most of them are fundamentally flawed with regards to one of the most important aspects of a software development project. Here's one that isn't.
tags: git
---

Software startups today use a plethora of project management tools, and not all
of them are created equal. Regardless of which one has the most bells and
whistles, the shiniest marketing site, or employs the most rabid of sales
representatives and evangelists, there is one value that I hold in higher
regard than any other: longevity.

In any STEM discipline, a core part of an engineer's daily work is to run sanity
checks. As per that old adage, “A good programmer is one who looks both ways
before crossing a one-way street.” Sanity checks are rendered somewhat less sane
when a single point of truth can't be established.

There is one tool — and one tool only — that I believe lasts the lifetime of a
software product, and that is its version control system. Long after your team's
current pointy-haired project manager moves to greener pastures and the next
person fills their shoes, decisions in your engineering team will still be
encoded into its revision history.

Middle-management types love to shake things up by throwing away a team's
project management workflow and replacing it with some alternative they favour.
When that happens you lose your designated single point of truth. You lose
context behind important decisions when the cards they're referenced from
inevitably get “lost”. Losing the context behind why a particular change
happened in a system over the course of its history is an excellent way of
losing confidence in that system. And not far beyond losing confidence, is
losing sanity.

If I were running a software company, I'd be inclined to do most project
management through GitHub. The _Issues_ system is probably good enough for 99%
of software projects out there, and by its very nature it is the tool that most
intimately integrates with your single point of truth — Git.

GitHub Issues is the weapon of choice for most of the world's largest and most
successful open-source software projects, many of those projects accepting
pull-requests from thousands of contributors. If you think the needs of your
engineering team are somehow more sophisticated, it may be time for a reality
check.
