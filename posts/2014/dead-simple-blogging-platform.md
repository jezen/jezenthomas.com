---
title: Dead-Simple Blogging Platform
date: 2014-06-27
description: How to run a blog with a few simple tools
excerpt: How I use bits of Make and Bash to provide a friction-free blogging workflow.
tags: unix, jekyll, ruby
---

After hacking on several Wordpress projects, I’m tired of Wordpress. The same
can be said of the Grunt.js task-runner. Both systems are simply too complex
for the task of displaying words on an Internet page. I now use neither of
these tools.

**TL;DR**: The source for this site is [available on
Github](https://github.com/jezen/jezen.github.io/tree/source).

After spending over a year living in the command line with Vim and unix tools,
I’ve come to appreciate small, simple, composable components that do [one thing
well](http://onethingwell.org/).

I use only a few small and simple tools to power this blog, and it’s the most
pleasant development and publishing experience I’ve had. In terms of languages,
the development stack is Ruby and a bit of Bash. I use Make as well, but it’s
not really a separate language and I use it mostly as a wrapper around my shell
scripts.

The static site generator I use is [Jekyll](http://jekyllrb.com/). This article
is not a Jekyll tutorial, so I won’t go into any detail on how to use it. I
write the styles in Sass with the indent syntax, because it’s more similar to
Ruby idioms than SCSS or plain CSS. There is minimal JavaScript (and no jQuery)
here, though I plan to strip back on that even further.

### Hosting & Deployment

I think FTP is lame and old-hat, but I’m also slightly too lazy to setup any
kind of git hook that deploys the site whenever I push some changes.
Fortunately, Github Pages does all this stuff automatically, and they’re very
fast and stable (I’ve even had a few months with absolute zero downtime), *and*
they’re free. Can’t get a better price than free.

For this to work, I created a `jezen.github.io` repository and a branch called
`source`. As the name implies, the `source` branch is for the source code and
content I actually work with. The `master` branch is only for generated files.
There are a few steps to making this play nicely with Jekyll:

 1. Clone the repository to some directory. Let’s call it `blog`.
 2. Create a directory inside `blog` which we use as our build target. Let’s call it `dist`.
 3. Clone the repository into the `dist` directory. *Yes*, we have the repository inside the repository.
 4. In the `blog` directory, checkout the `source` branch. The `dist` directory should always be on `master`.
 5. Create a `.gitignore` file in the `blog` directory, and add `dist` to the file.
 6. Add `destination: dist` to your `_config.yml` file, so Jekyll knows where to output generated files.

The final trick is to create an empty file in the `blog` directory called
`.nojekyll`. This file tells Github not to try building the site. Github won’t
be able to build the site if you use any custom Jekyll plugins. It’s easy
enough to generate the site on your local machine anyway, so why not. Add
`include: ['.nojekyll']` to your `_config.yml` file so the `.nojekyll` file is
always included in the generated site.

Deploying to Github is as simple as pushing all the stuff in the `dist`
directory. I have a shell script called `deploy` to handle this, which looks
like this:

~~~bash
#!/bin/bash

cd dist
git add --all .
git commit -m build
git push
cd ..
~~~

Don’t forget to do `chmod u+x deploy` so the file is actually executable.

### Development workflow

Jekyll comes with an *ad hoc* server, which we can fire up with `jekyll serve`.
When I’m working, I like to keep a copy of the site open in a browser so I can
quickly review changes. I use a couple of flags to tell Jekyll that I want to
display draft posts, and I also want the server to rebuild the site whenever a
file is changed. In the end, the command is `jekyll serve --drafts --watch`.

I do much the same thing with Sass. I use the command `sass --watch
_sass/main.sass:main.css` to watch the `main.sass` file and compile it into
`main.css` whenever a change is made.

Since I generally only ever use one terminal instance, I make use of some Unix
job control to move these two ‘watch’ tasks out of my way. I have a shell
script called `serve` that sets these tasks up for me, which looks like this:

~~~bash
#!/bin/bash

(jekyll serve --drafts --watch) &
(sass --watch _sass/main.sass:main.css)
~~~

The parantheses means these jobs run in *subshells*, so they don’t get in each
other’s way.

I mentioned earlier that I use Make. I like Make because it’s a nice wrapper
for my shell scripts, and I can also write short scripts in it directly. It
seems to be a convention to have a default task in a Makefile that builds
everything, but I prefer to use the first task for documentation so I can
quickly remind myself how I should manage the site. Let’s take a look at my
Makefile:

~~~make
SHELL := /bin/bash

.PHONY: default build serve deploy

default:
	@printf "\n Howdy! Use the following commands to manage this site:\n"
	@printf " - build \t Build the site for production"
	@printf " - serve \t Run an ad hoc WEBrick server"
	@printf " - deploy \t Push the site live to GitHub \n\n"

build:
	jekyll build
	sass --style compact _sass/main.sass dist/style.css
	awk 'NF > 0' dist/style.css > tmp && mv tmp dist/style.css

serve:
	./serve

deploy: build
	./deploy
~~~

Starting from the top, I tell Make which shell to use. If you don’t do this,
Make will use `sh` by default, which is likely to give some odd surprises at
some point.

The `.PHONY` line is needed because all of the task names in the file are just
generic names, and don’t reference any real file in the project. If you don’t
do this, and you have a file with the same name as one of your tasks, Make will
get confused.

My first task is called `default`, and it’s run if you do `make` from the shell
without any arguments. This pattern is used for many other Unix tools, and I’m
quite fond of it. Be sure to use `printf` and not `echo`, because of the two,
only the former is portable.

The `build` task builds the site and the styles, and then does some poor-man’s
file compression of the output css with `awk`. That last step is probably a bit
pointless, but it was a nice exercise. The `serve` and `deploy` tasks simply
run their respective shell scripts. You’ll notice the `deploy` task *depends*
on the `build` task, so the latest version of the site is always deployed to
Github.

When I want to work on the site, I could just run `make serve`, but then the
two tasks would print their output to the terminal. I’d rather the output would
get out of my way, so I start the task in the background and send its output to
a nohup.out file. To do this, run `nohup make serve &`.

There’s not much more to it, and nor should there be.
