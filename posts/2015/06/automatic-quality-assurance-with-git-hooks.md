---
title: Automatic Quality Assurance with Git Hooks
date: 2015-06-22
location: Gdynia, Poland
excerpt: How to use a pre-commit hook in Git to make sure every commit passes all tests and style checks.
tags: git, bash, ruby
---

<span class="run-in"><span class="drop">F</span>or some months now</span> I’ve
been trying to make sure every commit in my Rails project passes all tests and
style checks. This helps to guard against code quality taking a nose-dive that
usually is inevitable. Git (and Mercurial) allows us to run scripts at selected
points of our version-control workflow.

At a high level, this means we can write a script that makes a number of checks
against our codebase, and have Git automatically run this script any time we try
making a commit.

I have a bash script in my Rails app that does the following:

- Stash unstaged changes so checks are only run against staged changes
- Run style checks with Rubocop
- Check for security vulnerabilities with Brakeman
- Run RSpec tests
- Run Cucumber tests
- Update application version number

If at any point any of those checks should fail, we should pop the stash and
abort the commit.

## Stashing Unstaged Changes

In my script I check if there are any staged changes, and stash anything that
isn’t staged. This is because I only want to run checks against changes I’m
including in the commit. If there are no changes staged for the commit, the
script exits early without stashing anything. Don’t worry about the `highlight`
function for now; it’s just a wrapped version of `printf` with some colour, and
you’ll see it later.

~~~sh
if git diff --quiet --cached HEAD; then
  highlight "No changes to test; exiting"
  exit 0
fi

if [ "$(git status -s | wc -l)" != 0 ]; then
  highlight "Stashing unstaged changes"
fi

git stash save --keep-index --include-untracked
~~~

## Quality Control

For a git-hook to succeed, the script should exit with an error code of `0`.
After each check I add the exit code of that check to a counter, and exit the
commit hook with that counter as the error code. If none of the checks fail, the
exit code will be `0` and Git will allow the commit. Otherwise, the commit is
aborted, and I have to fix the mistakes in my codebase.

The call to `trap` is a bit like `Kernel#at_exit` that you find in Ruby; it
listens for an `EXIT` event and runs some function before exiting. In my case
I’ve asked it to run a function called `pop_stash`, which will revert my working
directory to the state it was in before stashing unstaged changes.

My checks are specific to Ruby/Rails projects, but there are most likely
equivalents for whichever tech stack you’re using. I’m using the
[brakeman gem][brakeman] to protect me from creating obvious security
vulnerabilities, and I think every project ought to have something like this
running regularly.

~~~sh
declare -i ERRORS=0
trap pop_stash EXIT

highlight "Checking for style/syntax errors"
rubocop --rails --fail-fast
ERRORS+=$?

highlight "Checking for security vulnerabilities"
brakeman -q -z
ERRORS+=$?

highlight "Running RSpec tests"
rspec --fail-fast
ERRORS+=$?

highlight "Running Cucumber tests"
cucumber --format progress
ERRORS+=$?

exit $ERRORS
~~~

## Automatic Application Versioning

I like the idea of seeing my application version printed somewhere in my app. If
all of my quality control checks pass, I write a datestamp to a file in my
application. I went with a datestamp instead of major/minor numbers or a commit
hash because it’s the simplest thing that works, and also it [makes the most
sense][versions]. I use Git to then add the bumped version to the staging area.

~~~sh
date +%Y%m%d%H%M > config/version
git add config/version
~~~

If you want to use this version number in your Rails app, you can add the
following in `config/application.rb`.

~~~ruby
module MyApp
  class Application < Rails::Application
    config.version = File.read("config/version")
  end
end
~~~

## The Whole Script

I was jumping around the script before so I could explain its components in
finer detail, but this is an imperative script so the order in which each line
is executed is important. Here’s the complete script for you to ~~copy and
paste~~ reference.

~~~sh
#!/bin/bash

set -e

highlight() {
  tput setaf 6
  printf "%s\n" "$1"
  tput sgr0
}

pop_stash() {
  highlight "Reapplying unstaged changes"
  git stash pop
}

declare -i ERRORS=0

if git diff --quiet --cached HEAD; then
  highlight "No changes to test; exiting"
  exit 0
fi

if [ "$(git status -s | wc -l)" != 0 ]; then
  highlight "Stashing unstaged changes"
fi

git stash save --keep-index --include-untracked

trap pop_stash EXIT

highlight "Checking for style/syntax errors"
rubocop --rails --fail-fast
ERRORS+=$?

highlight "Checking for security vulnerabilities"
brakeman -q -z
ERRORS+=$?

highlight "Running RSpec tests"
rspec --fail-fast
ERRORS+=$?

highlight "Running Cucumber tests"
cucumber --format progress
ERRORS+=$?

date +%Y%m%d%H%M > config/version
git add config/version

exit $ERRORS
~~~

[brakeman]: https://github.com/presidentbeef/brakeman
[versions]: http://blog.codinghorror.com/whats-in-a-version-number-anyway/
