---
title: Shell Script Static Analysis in Vim
date: 2015-05-17
excerpt: How to configure Vim to provide immediate feedback on the quality of your shell scripts.
tags: code, vim, bash
---

<span class="run-in"><span class="drop">I</span>’m a big fan</span> of static
analysis for a few reasons:

1. Enforcing a consistent style helps other developers quickly scan and
  understand the code I write.
2. Some static analysis tools guide the programmer towards an approach that is
  more idiomatic of the language, which is a great way to learn.
3. I’m only human and my clumsiness often causes bugs which at best slow me down,
  and otherwise potentially ruin my day. Code linting prevents this.

I also enjoy solving small problems with shell scripts, though the shell is a
relatively unforgiving environment; you’re not given all the safety and sugar
that you would have in Ruby, for example. Having some static analysis as part of
my Bash script development workflow would make for an excellent first line of
defence.

I have total confidence that any self-respecting IDE has some sort of static
analysis built in for shell scripting, but I’m a Vim guy so that just wouldn’t
be my cup of tea. All of this naturally pushes me in the direction of marrying
Vim with [ShellCheck][shellcheck], which (as I’m sure you’ve guessed)
automatically detects problems with shell scripts and commands.

The first step is to install ShellCheck. If you’re running OSX like me, you can
pull down a ready-to-go ShellCheck with [Homebrew][homebrew].

~~~sh
brew update && brew install shellcheck
~~~

The next step is to teach Vim to use ShellCheck as an external compiler. We can
set this with `makeprg`. We’ll also want to ask ShellCheck to use gcc-style
output, as Vim understands this by default. This could be added under an
`autocmd` in your `.vimrc`, but instead I’m going to use Vim’s filetype plugin
to store this configuration in its own file.

~~~vim
" ~/.vim/ftplugin/sh.vim
set makeprg=shellcheck\ -f\ gcc\ %
au BufWritePost * :silent make | redraw!
~~~

The second line of that snippet runs the `:make` command every time Vim writes
to the shell script. We need to do the silent/redraw dance to suppress the “Hit
ENTER to continue” prompt.

Finally, I’d prefer Vim to automatically open the quickfix window if ShellCheck
found any problems with my shell script, so I add the following couple of lines
to my `~/.vimrc`.

~~~vim
au QuickFixCmdPost [^l]* nested cwindow
au QuickFixCmdPost    l* nested lwindow
~~~

Now, whenever I write to a shell script, Vim gives me immediate feedback so I
can catch problems early. It’s worth noting also that ShellCheck reads in the
shebang so you’ll receive information specific to the shell you choose.

[shellcheck]: http://www.shellcheck.net/ "Official ShellCheck site"
[homebrew]: http://brew.sh/ "Official Homebrew site"
