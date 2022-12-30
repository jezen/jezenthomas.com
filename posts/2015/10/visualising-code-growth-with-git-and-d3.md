---
title: Visualising Code Growth with Git and D3.js
date: 2015-10-25
location: Warsaw, Poland
excerpt: Combine Git, Bash, and JavaScript to see a 10,000 foot view of the growth of your codebase over time.
tags: git, javascript, bash
---

*Update:* I gave a lightning talk at the Falsy Values developer conference in
Warsaw, Poland where I discussed this topic. You can now watch the video from
the conference here. Original article is found below.

<iframe width="560" height="315" src="https://www.youtube.com/embed/c9CoBR6_OkI?rel=0" frameborder="0" allowfullscreen></iframe>

<span class="run-in"><span class="drop">I</span> recently watched a
screencast</span> that discussed the idea of generating statistics about a
project using Git. The gist of it is to iterate over the revision history and
extract some metric at each change.

In the screencast, the single extracted metric was the number of lines of code
in the codebase. While this is interesting, it's difficult to see patterns in
the numbers when they're plainly printed on the screen.

The best way to see patterns in numbers is to plot them on a graph, so I
extended the script from the screencast to not only show the number of lines of
code, but also the date and time each commit was made.

~~~bash
#!/bin/bash

# Based on 'Statistics Over Git Repositories' from Gary Bernhardt's 'Destroy
# All Software' screencast series

set -e

revisions() {
  git rev-list --reverse HEAD
}

commit_date() {
  git show --format="%cD" "$rev" | head -n 1
}

number_of_lines() {
  git ls-tree -r "$rev" |
  awk '{print $3}' |
  xargs git show |
  wc -l
}

for rev in $(revisions); do
  printf "%s\t%s\n" "$(number_of_lines | xargs)" "$(commit_date)"
done
~~~

The bash script above outputs a set of tab-separated values; one row per commit.
The idea would be to run the script from inside a Git repository and redirect
the output to some file.

I tested this against the Underscore.js repository which contains a couple
thousand commits spanning the past six years. Plugging the resulting data into a
trivial chunk of D3.js code produces this beautiful little graph.

<p data-height="355" data-theme-id="477" data-slug-hash="bb8940bf7c651cb3a4110c091b8afc4d" data-default-tab="result" data-user="jezen" class='codepen'>See the Pen <a href='http://codepen.io/jezen/pen/bb8940bf7c651cb3a4110c091b8afc4d/'>Underscore.js LOC Stats</a> by Jezen Thomas (<a href='http://codepen.io/jezen'>@jezen</a>) on <a href='http://codepen.io'>CodePen</a>.</p>
<script async src="//assets.codepen.io/assets/embed/ei.js"></script>

I think viewing a codebase from this perspective provides a strong starting
point for asking important questions about a project, *e.g.*:

- Why did the size of the codebase more than double half-way through 2012?
- What code exists in that plateau towards the end of 2013? Does the plateau
  suggest a portion of the code should be extracted to another library?
- What caused the brief valley towards the end of 2011? Are some developers
  changing too much in one commit?
- What's the general trend? Will the project continue to grow in complexity?
  Will technical debt accumulate?
- Does the growth of production code correlate with the growth of test code?

The results of reflecting on a codebase this way might not always be flattering,
but if there are obvious issues, at least you'll know about them, and you'll
have a chance to effect change.
