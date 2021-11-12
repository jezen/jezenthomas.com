---
title: Using Git to Manage Todos
date: 2015-10-19
location: Gdynia, Poland
excerpt: The TODO comments in your codebase will linger and rot, but this small bash script will coerce your team into cleaning up after themselves.
tags: git, bash
---

<span class="run-in"><span class="drop">A</span> common convention</span> when
writing software is to add `TODO` or `FIXME` comments directly to the codebase.
These comments clearly mark bits of logic that are yet to be implemented or are
in need of review.

One problem with this approach is that it's easy to ignore these comments, and
under the human pressures of deadlines and multi-tasking, the comments are
forgotten about and are left to linger and rot. The number of `TODO` comments in
the codebase grows over time, gradually becoming more intimidating, more
unwieldy, and less useful.

In order to coerce my colleagues (and myself) into taking better care of our
heaving pile of `TODO` comments, I added a *post-commit hook* that runs `grep`
across the directories we care about.

~~~sh
#!/bin/bash

set -e

grep -InR 'TODO' ./* \
  --exclude-dir=node_modules \
  --exclude-dir=public \
  --exclude-dir=vendor \
  --exclude-dir=compiled \
  --exclude-dir=git-hooks

exit 0
~~~

The output gives me the following bits of information:

- The file path
- The line number where the TODO was found
- The TODO message

This is a good start; at least everyone on the team will now see a list of all
TODO items every time they push some commits. The problem now though, is that
all of the TODO items are mixed together and it's difficult to know who is
responsible for which item. We could introduce some convention where everyone
adds their name to their TODO, but realistically people just wouldn't adhere to
that.

What's missing from my ideal output is the author of each TODO item. We can use
`git log` to find the author of a commit, and using the `-L` flag the command
will take a range of lines and a file path, both of which we find in our `grep`
output. The approach then, is to iterate over our `grep` output, pull the file
path and line number out of each search match, plug those into `git log` to find
the author, and glue the pieces back together to build the output we need.

~~~sh
#!/bin/bash

set -e

main() {
  while IFS= read -r todo; do
    printf "%s\n" "$(file_path):$(line_number) $(line_author) $(message)"
  done < <(todo_list)
}

todo_list() {
  grep -InR 'TODO' ./* \
    --exclude-dir=node_modules \
    --exclude-dir=public \
    --exclude-dir=vendor \
    --exclude-dir=compiled \
    --exclude-dir=git-hooks
}

line_author() {
  LINE=$(line_number "$todo")
  FILE=$(file_path "$todo")
  tput setaf 6
  printf "%s" "$(git log --pretty=format:"%cN" -s -L "$LINE","$LINE":"$FILE" | head -n 1)"
  tput sgr0
}

file_path() {
  printf "%s" "$todo" | cut -d':' -f 1
}

line_number() {
  printf "%s" "$todo" | cut -d':' -f 2
}

message() {
  printf "%s" "$todo" | cut -d':' -f 3 | xargs
}

main

exit 0
~~~

I think this approach is probably *good enough*. Allowing a list of TODO items
to grow like weeds is a human problem that can't exactly be solved elegantly
with technology alone, but at least the problem is now far more visible to all
of the developers on my team so they have chance at doing something about it.

A few non-obvious technical considerations:

- I'm using `printf` instead of `echo` because it's more portable, and it also
  allows you to be explicit about where newline characters fall.
- The `tput` commands in the `line_author` function change the colour of the
  output. This makes the commit author's name stand out so they don't miss it.
- The `xargs` command tacked on to the end of the `message` function is there to
  strip leading and trailing whitespace. It's probably not the best way to do
  that.

