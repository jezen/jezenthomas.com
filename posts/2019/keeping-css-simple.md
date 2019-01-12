---
title: Keeping CSS Simple
date: 2019-01-12
location: Kaliningrad, Russia
tags: css
banner: dominoes.jpg
---

In a previous day job, I suggested removing all instances of [Less][0]. Not
because it’s bad, or because [Sass][1] is _“better”_ in some way.

I suggested removing it and _not_ replacing it with Sass, instead keeping with
plain CSS because I felt the team had not shown the discipline to not abuse the
extra power a pre-processor gives you.

The codebase was a tangle of nested rules which results in overly complex and
specific selectors. When specifics of those need to change, the team doesn’t
back out of the existing specificity, but instead overwrite it with more. Add
to this a liberal helping of mixins — because “code reuse”, apparently — and
the resulting CSS is applied, disabled, reapplied, over and over.

My suggestion to not use a CSS pre-processor was met with much flapping and
exasperation.

> But everyone uses pre-processors! That’s just the way it’s done!

You can chalk up the above to both the [Bandwagon][2] and [Appeal to
Authority][3] logical fallacies. These fallacies are particularly widespread in
software development. To [paraphrase][4] Alan Kay and then take him slightly
out of context, _software development today is more popular culture than
engineering_.

Any flavour of CSS pre-processor gives you objectively _more_ power when
writing web page styles. But do you need that power? Or is that extra power
just rope with which to hang yourself? I think the benefits of pre-processors
don’t outweigh the drawbacks, especially in the hands of the enthusiastic but
inexperienced.

Plain CSS gets you plenty far enough.

[0]: http://lesscss.org/
[1]: https://sass-lang.com/
[2]: https://yourlogicalfallacyis.com/bandwagon
[3]: https://yourlogicalfallacyis.com/appeal-to-authority
[4]: http://www.drdobbs.com/architecture-and-design/interview-with-alan-kay/240003442
