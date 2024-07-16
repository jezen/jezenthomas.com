---
title: Is it c? Or is it с?
date: 2024-07-16
location: Kraków, Poland
excerpt: Sometimes errors like these hide in plain sight.
tags: haskell, work
image: is-it-c.jpg
---

Someone working on the team at Supercede asked  the following question
regarding some perplexing GHCi output.

> I must have missed something obvious because I've been staring myself blind
> at this for the past few minutes. Isn't it saying one thing is not in scope
> and then immediately suggesting that very same thing as a replacement?

```
ghci> :t API.Handler.V20201001.Types.tscaExcluded

<interactive>:1:1-40: error:
    Not in scope: 'API.Handler.V20201001.Types.tscaExcluded'
    Perhaps you meant one of these:
      'API.Handler.V20201001.Types.tsсaExcluded' (imported from API.Handler.V20201001.Types),
```

Can you spot the error?

The problem could have been related to some surprising behaviour in GHCi when
references are held to old values after their names are shadowed. Or perhaps it
was something related to the build cache since we use incremental compilation.
But those would have been guesses two and three.

Based on experience — and some luck — here was my first guess.

> You know, we had an interesting issue once where a c was substituted for a с.
> See the difference?

Lo and behold…

> Emacs says one is a LATIN SMALL LETTER C and the other is a CYRILLIC SMALL
> LETTER ES but I would never have guessed. And would you believe – this is
> indeed what has happened. Wow!

Perhaps this deserves a new linting rule.
