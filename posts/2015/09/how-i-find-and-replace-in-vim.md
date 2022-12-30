---
title: How I Find and Replace in Vim
date: 2015-09-02
location: Gdynia, Poland
excerpt: A simple and effective way to find and replace across an entire project with plain Vim, without plugins.
tags: vim
---

<span class="run-in"><span class="drop">M</span>y current task</span> in my day
job involves an obnoxious amount of shotgun surgery. The task is to sandbox a
JavaScript library which extends native objects, pollutes the global namespace,
and doesn't play nicely with other libraries we hope to use.

Sandboxing the library is essentially a case of finding all references to the
objects and functions the library exports, and attaching them to some named object
instead of to the `window`, though this is easier said than done.

There are several ways to find and replace in Vim. Most guides I read suggest to
use some plugin or some snippet of Vimscript which you should stuff into your
`.vimrc`.

My approach to finding and replacing across the project is with stock Vim,
paired with an external grep-like tool — in my case `ack`.

It seems as though most people reach for plugins by default, and that's a shame
because the tools we have are often powerful enough on their own. Vim is
certainly powerful enough by default to perform substitutions across an entire
project, with or without confirmation.

Here's the method I'm using:

~~~vim
:args `ack -l '\bClass\b' --ignore-dir=compiled`
:argdo %s/\<Class\>/MooTools.Class/gc | update
~~~

The first line populates Vim's argument list with the result of the external
`ack` command. I test out my `ack` directly in the shell before using it in Vim
so I know I'm roughly getting the list of files I want. The `-l` flag in `ack`
tells the tool to just return me the file names. I'm using the `\b`
word-boundary to refine my search results so I'm not overwhelmed with noise.

The second line uses `argdo` to perform some `Ex` command across every file in
the argument list. In this case, that `Ex` command is a buffer-wide
substitution, with manual confirmation for each replacement. It's interesting to
note that I'm also using word-boundaries to refine my search results, but Vim's
word-boundaries (`\<` and `\>`) look different from the ones in `ack`.

The substitute command is passed the `g` and `c` flags. The `c` flag is
interesting here; it stands for 'confirmation', and will ask me to confirm or
deny substitutions with the `y` and `n` keys.

The pipe character in the context of an `Ex` command is not the same as piping
data through the shell; it's more like a semicolon in C-like languages and
allows you to perform separate commands in one move.

I don't think the commands all together are as interesting as the components
they are composed of. Understanding how each of the pieces work in isolation is
key to developing proficiency in Vim and in Unix in general.
