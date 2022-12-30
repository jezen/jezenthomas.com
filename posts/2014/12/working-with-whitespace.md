---
title: Working with Whitespace
date: 2014-12-21
location: Göteborg, Sweden
excerpt: A couple of tips for weeding out trailing whitespace and unnecessary blank lines in Vim.
tags: vim
---

Any thought-leadering piece, any conference talk, any tweet from an outspoken
nerd; if there’s one common theme — one opinion for which there is no
counterargument — it’s that there’s no excuse for careless, messy whitespace in
a codebase. Here’s how I handle whitespace with my weapon of choice, Vim.

## Always show whitespace

Here are a couple of lines that should be in everyone’s `.vimrc`. The first
displays invisible characters, and the second defines which symbols should be
used to represent the different kinds of invisible characters. I mostly borrowed
the second line from Steve Losh’s [vimrc][1].

~~~
set list
set lcs=tab:▸\ ,extends:❯,precedes:❮,nbsp:.,trail:·,eol:¬
~~~

## Cleaning trailing whitespace

This is another gem I found in Steve Losh’s vimrc. I don’t care to learn exactly
how it works just yet. At the moment, I have it mapped to `<leader>w`, but it
might be a good idea to automatically run the command anytime you write to a
file. I think it really depends on how you feel this will affect your version
control workflow.

~~~
nnoremap <leader>w mz:%s/\s\+$//<cr>:let @/=''<cr>`z
~~~

## Normalising blank lines

Unless you’re drawing ASCII art, I don’t think there’s ever a good reason to
have contiguous blank lines. Any group of rules or logic in a codebase should be
separated by one single blank line, not more. Before I took the nosedive into
Vim and Unix, I would clean these blank lines manually or force myself to ignore
the disorder. Neither of those are any fun.

Vim provides us with *filters*, which pipe the contents of a buffer to some
external Unix command and read the results of the command back into the buffer,
replacing the original text. If you pipe a file to `cat -s`, it gives it back to
you with the blank lines normalised. To do this straight from Vim:

~~~
:%!cat -s
~~~

The percent symbol references the entire buffer (which means filters also work
with ranges), and the bang tells Vim to drop to the shell.

[1]: https://bitbucket.org/sjl/dotfiles/src/603bb1ae9da27c6e08ab115df1cb5d8f6a1442c3/vim/vimrc?at=default 
