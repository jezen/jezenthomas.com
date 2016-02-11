---
title: Code Review Done Right
date: 2015-11-24
excerpt: Code review is an oddly bureacratic process for a team of software developers to follow. What follows are my non-technical thoughts on conducting constructive, humane reviews of code.
tags: code, humans
---

<span class="run-in"><span class="drop">T</span>he common approach</span> to
code review is in my opinion a sad one — you may not contribute until you have
received the stamp of approval from a project senior. It's an oddly bureacratic
process for a team of software developers to follow.

It's the software development version of *guilty until proven innocent*.

Although the approach is prevalent, I don't believe it's the best one, or even a
good one. What follows are my non-technical thoughts on conducting constructive,
*humane* reviews of code.

## Sharing

Code review ought to be about sharing. It's an effective way to show colleagues
how you approach a problem. Initiating code review is a powerful catalyst for
dialogue. When you submit code for review, you provide all developers involved
with common ground to stand on and the context needed to engage in meaningful
discussion.

If you're not explicitly sharing an idea, you might be asking for alternatives.
Good code review involves both sharing and learning, for both the committer and
the reviewer. This is the value proposition of the code review. Everybody wins
here.

## Nitpicking

Nobody likes having their work nitpicked, and nobody who isn't a total jackass
enjoys nitpicking work.

If a significant portion of comments in your code review process are of the more
mundane variety, *e.g.*, “you forgot to add a semicolon here”, it tells you that
there is something missing in the team's development workflow. Humans make
mistakes, and our development process should allow for that. If the one thing
standing between a commit and a production deploy is human discipline, you have
broken software.

There's another problem with nitpicky comments; it's distracting. If a reviewer
feels the need to double-check the amount of whitespace used, their attention is
drawn away from more interesting insights, and possibly more serious problems.

This isn't to suggest strict adherence to a style-guide isn't important — it is.
But we have tools that can catch these issues and we ought to use them.
Cognitive power is too precious to waste on the mundane.

## Encouragement

Code review involves review, not military inspection. Not all feedback needs to
be critical. We are after all a team of humans, and it feels good to be told
“Ah, I like the way you did that!” from time to time. This isn't to suggest
anyone should promote a culture of circle-jerking, but as a general rule people
do enough good work to deserve an animated GIF of a dancing princess or
something at least once per day.

<center>
![It doesn't necessarily *have* to be a dancing
princess](/img/dancing-princess.gif)
</center>

## Language

Even if people consciously understand that they are not their code, and that
criticisms of their work are not personal, it's still so hard to not be
sensitive towards critique. I'm not sure I do enough to accommodate people's
feelings in the review process, but I do try to differentiate the language I use
when I give criticism versus when I give encouragement.

When giving criticism, nothing is the committer's fault. If there is a problem
in a commit, it's *everyone's* problem in *our* codebase, and it's *everyone's*
responsibility to rectify.

When giving encouragement, it is the committer who receives direct praise. If
*you* rose above and beyond the call, then *you* deserve the kudos.
