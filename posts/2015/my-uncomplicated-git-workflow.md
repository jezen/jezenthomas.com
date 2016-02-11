---
title: My Uncomplicated Git Workflow
date: 2015-11-12
excerpt: Stop screwing around with version control ceremony and learn to love Git from the command line.
tags: git, unix
---

<span class="run-in"><span class="drop">R</span>ecently I have been
surprised</span> with how often I see developers struggle with their version
control system. Given that using version control is such a central part of any
serious software endeavour, struggling with it must be a great way to kill
developer productivity and potentially lose work.

I don't find it valuable to spend brain cycles context switching between
thinking about what my production code should look like and trying to remember
which Git command and with which flags is appropriate in any given context. For
this reason, my Git workflow is about as simple as I can make it.

My workflow exists exclusively in the terminal, and leans heavily on Git aliases
and a Ruby gem (don't laugh) called *git-smart*. The gem seems not to be
actively maintained any more, but it still works regardless. I don't use
git-flow, and I advise against using it because it is needlessly complex.

Reasons for only using Git in the terminal:

- I want to protect my arms, wrists, hands, and fingers by not having to shuffle
  a cursor around the screen with a mouse or trackpad. Typing only.
- My terminal is easy on the eyes with its dark theme so I suffer from fewer
  headaches when working.
- I am unable to fathom how anyone would find a GUI application like SourceTree
  less intimidating than Git's command-line interface.

## Pulling changes

A typical working session for me begins with pulling the latest changes from the
remote repository. I use the `git smart-pull` command provided by the git-smart
gem for this, and I have the command aliased in my global `.gitconfig` so I only
need to type `git sp`. The git-smart gem will first run `git fetch origin`, and
then figure out the best way to pull changes from remote, rebasing if necessary.
It's also nice that it tells you which commands it's running and when, so you
can learn more about Git while it makes decisions for you.

## Reviewing changes

After I have spent time working on a task, I will check the status of the
working directory with the standard `git status` command, which I have aliased
to `git st`. I use the standard output because I can scan it more quickly with
my eyes. In most cases, I already know what the status of the working directory
will be before checking the status, but I run the command anyway as a simple
sanity check. I am usually able to scan the status in a fraction of a second.

Assuming checking the status didn't give me any surprises, I inspect the work I
did in greater detail with `git diff`. I don't alias this command, because I
find it comfortable enough to type at speed. I do make an effort to keep my
commits small, so I am usually able to scan the diff in about a second.

## Staging changes

Now I am ready to begin staging my changes. In the cases when I want to stage
*everything* — and this is the most common case — I use the `git add --all`
command, which I have aliased to `git aa`. If on the other hand I feel my work
should be split into multiple commits, I'll stage hunks of changes with `git add
-p`. I use this command often, but not often enough to justify creating an alias
for it. Using the `-p` flag when staging puts you in an interactive patch mode
that asks you which hunks to stage. Answer `y` for yes, `n` for no, and `q` to
quit patching. Sometimes the hunk it presents to you contains more code than you
wish to stage, and at that point you can answer `s` for split.

Sometimes I feel the need to check once again which changes are staged, but `git
diff` alone won't work here. For staged changes, you need to run `git diff
--cached`, which I have aliased to `git dc`.

## Committing changes

When I'm ready to commit my changes, I run `git commit`, which I have aliased to
`git ci`. I almost *never* use the `-m` flag when committing. Instead, Git opens
up my text editor so I can comfortably write a proper commit message there.

If I forget to add some changes to the commit and/or I want to change the commit
message, I use `git ci --amend`. If I want to undo the commit entirely, I use
`git reset HEAD~1`.

If there are no other commits to make, I will run `git sp` again to ensure I
have the latest changes locally, and then I run `git push` to share my work with
my team.

## Working with branches

I'm generally not a fan of the pull-request model of collaboration, and I don't
like the idea that a team of developers can't trust one another enough to commit
directly to master. The pull-request model usually involves a synchronous
code-review step, where the PR can not be merged into master until it has been
approved by other members of the team. It's unfortunate that we have tools that
cater to an asynchronous workflow, and yet use them synchronously anyway. The PR
model can be a necessary evil however if a project doesn't have any tests.

When I do need to use branches, I make liberal use of `git checkout`, which I
have aliased to `git co`. If I want to create a new branch, I use `git co -b
<branchname>`. If I want to switch back to the previous branch I was on (I
sometimes shuffle quickly between a feature branch and master), I use `git co
-`. The checkout command is also handy for clearing away unstaged changes that I
don't care for anymore, with `git co -- .`.

If I need to merge from a feature branch back into master, I use the `git
smart-merge` command from git-smart. I don't use this often enough to bother
creating an alias. This is another one of those commands that decides the best
strategy for merging given some context, and it's always just done the right
thing for me.

## The view from 10,000 feet

From time to time, I like to briefly check how the project has been progressing,
and again git-smart pulls through for us here providing the `git smart-log`
command, which I have aliased to `git sl`.

## Getting distracted

Sometimes I'll be half-way through working on a task and I will have to
context-switch to another task. I don't want to commit my work in an unfinished
state, but I do want a clean working directory. In this case, I use `git stash`.
When I want to retrieve my unfinished work, I use `git stash apply`, followed by
`git stash drop` assuming nothing went wrong when applying the latest stash.
This is safer than using `git stash pop` directly.

## Problems in the wild

There is a caveat to using git-smart, and that is it doesn't play nicely with
Git submodules. What happens is, a submodule that is yet to be updated will make
the working directory appear dirty, when in reality it isn't. When git-smart
sees a dirty working directory, it'll stash your changes before pulling, and
then pop them after pulling, which will pop the wrong stash which could
potentially cause problems. In practice, this shouldn't be an issue because
submodules should be avoided anyway.

## Conclusion

Learning to harness the power of Git properly is a key factor in communicating
with colleagues effectively, and it also makes projects far easier to maintain.

Uninstall SourceTree. Abandon git-flow.
