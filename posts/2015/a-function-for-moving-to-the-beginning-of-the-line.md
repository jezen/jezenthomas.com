---
title: Moving to the Beginning of the Line
date: 2015-05-26
location: Gdynia, Poland
excerpt: A nice little Vim function that moves the cursor to the first non-whitespace character on the current line, or the first column if it was already on the first character.
tags: vim
---

When I started using Vim, one of the first custom mappings I added to my
`.vimrc` was moving to the beginning of the line with `H` and moving to the end
of the line with `L`.

~~~vim
" Jump to beginning of line
noremap H 0
" Jump to end of line
noremap L $
~~~

This mapping may be slightly awkward because it’s rare you want to move to the
first column of the current line, and in most cases you actually would rather
move the cursor to the first non-whitespace character of the line.

We could change this mapping to move to the first non-whitespace character of
the current line by using `^` instead of `0`, but I don’t want to lose the
ability to move to the first column of the line. To remedy this, I wrote a
short function that moves the cursor to the first non-whitespace character of
the current line, or moves the cursor to the first column if it was already on
the first non-whitespace character.

~~~vim
" Jump to first character or column
noremap <silent> H :call FirstCharOrFirstCol()<cr>

function! FirstCharOrFirstCol()
  let current_col = virtcol('.')
  normal ^
  let first_char = virtcol('.')
  if current_col <= first_char
    normal 0
  endif
endfunction
~~~

The function above works by first checking the column that the cursor is
currently on and storing it in `current_col`. We then move the cursor to the
first non-whitespace character and check the column again. If the current
column number is greater than that of the first non-whitespace character, then
we move the cursor to the first non-whitespace character. Otherwise, we move
the cursor to the first column.
