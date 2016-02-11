---
title: Tips for Working with Bundler
date: 2015-01-07
excerpt: Store your dependencies with the rest of your Ruby project.
tags: ruby
---

Bundler makes managing dependencies in a Ruby project quite easy, but there’s
one thing I think Bundler still gets fundamentally wrong, and that is that by
default it stores gems somewhere outside of your project’s root directory.

I think as a general rule, a project’s dependencies should always be stored with
the rest of the project. Luckily we can teach Bundler to do this.

## Getting started

When you start working with a project and you want to install its dependencies,
you should *always specify the installation path*. You only need to do this
once, as Bundler saves the path to `.bundle/config`.

~~~
bundle install --path vendor/bundle
~~~

## Working with Binstubs

When you specify your own gem installation path, you’ll notice that you don’t
have access to commands like `rspec` or `cucumber`. You can access any command
provided by a bundled gem by prefixing the command with `bundle exec`, but I
find this too cumbersome. If I say `rspec`, then my machine should just do what
I tell it to.

Bundler can create binaries on the fly with the `binstubs` command, so to create
my `rspec` binary, I would do:

~~~
bundle binstubs rspec
~~~

This will create a binary in the `bin` directory. You’ll need to add that
directory to your path before you can use anything from it. Since this `bin`
directory is project-specific, it doesn’t make sense to do this system-wide.

You can use [autoenv][1] to automatically update your path when you change into
a project directory.

~~~
brew install autoenv
touch .env && echo 'export PATH="./bin:$PATH"' > .env
~~~

## Working with Git/Mercurial

Your `.bundle/config` is specific to your workflow, so you shouldn’t force it on
others. Whether or not you want to add your gems to your repository is up to you. At
Vaamo, we version control our `node_modules` directory because if `npm` doesn’t
respond for whatever reason, our deployments fail. I haven’t had this issue with
RubyGems yet, so I’m happy to exclude gems from the repository.

Add the following few lines to your `.gitignore` or `.hgignore`.

~~~
.bundle
vendor/bundle
~~~

## Working with Ruby on Rails

Rails has its own way (surprise, surprise) of working with binaries. Rails will
complain if you do `bundler binstubs rails`, so don’t do that. Instead, Rails
provides a Rake task for creating binaries (if you don’t already have them).

## Working with Jekyll

Jekyll tries to convert everything you have in your project root, but you won’t
want to try and copy a bunch of gems to the static site Jekyll outputs. Add the
following to `_config.yml`:

~~~
exclude: ["vendor"]
~~~

[1]: https://github.com/kennethreitz/autoenv
