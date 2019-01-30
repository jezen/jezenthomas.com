---
title: You Think CSS-in-JS is Bad?
date: 2019-01-30
location: Gdynia, Poland
tags: css, javascript
banner: bsod.jpg
---

Microsoft have announced they will finally terminate Internet Explorer 10 next
year.

Good riddance.

However, I am nostalgic for the somewhat masochistic pleasure in finding hacky
solutions for old IE versions.

Earlier in my career I figured out that IE7 can be tricked into mimicking the
behaviour of the `:before` and `:after` CSS pseudo-elements. You think
CSS-in-JS is bad? Check this out:

```css
.thing {
  *zoom: expression(
    this.runtimeStyle.zoom = '1',
    this.insertBefore(
      document.createElement('i'),
      this.firstChild
      ).className = 'ie-before'
    );
}

.thing .ie-before {
  *zoom: expression(
    this.runtimeStyle.zoom = '1',
    this.innerHTML = '|'
    );
}
```

I don't remember why _zoom_ was relevant, but I do remember why the declaration
was prefixed with an asterisk. Any rule prefixed with an asterisk was ignored
as invalid by any browser other than IE7. This was a browser bug that many
people relied upon at the time. The same bug also existed in IE6, but that
browser's special character was an underscore instead.

Needless to say, it was a horrific state of affairs.

Aren't you glad that all you have to worry about these days is which
over-engineered JavaScript build system you should use?
