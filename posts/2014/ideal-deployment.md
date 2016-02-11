---
title: Ideal Deployment
date: 2014-10-31
excerpt: Thinking about what I need to continuously deploy software, without all the hipster complexity.
tags: operations
---

I’m faced with the task of deploying Oddjob, and I haven’t yet found
documentation online that isn’t some variation of “Oh, deployment is easy, just
use Dokku/Docker/EC2/Heroku/Ansible/Vagrant/Capistrano/Puppet/Chef/etc… etc…
etc…”.

Aside from that fact that most programming advice that begins with “Oh, you
*just*…” is unhelpful, I’ve seen very little advice given on *why* and *when*
to use a particular deployment strategy, as opposed to just picking a random
brand and rattling off some installation instructions.

Oddjob is an important app that will at some point support businesses with
their daily administrative duties, so it’s super important that the deployment
process is done right. And I don’t think the right solution will come around
without first comprehensively defining the problem. In this post I’ll mull over
what I think I need in my deployment process.

## Minimal-shared multitenancy

I’d like to have separate instances of the app; one for each company who uses
Oddjob. At least, I *think* that’s what I want. I’m 99% sure I at least want
separate databases. I’m convinced that sharing a database among companies will
be a minefield technically, and potentially also legally.

Perhaps it can be a single app instance that will switch database connections
depending on the subdomain a request points to. Perhaps it should be a whole
bunch of Rails apps that are isolated with virtual machines. I won’t pretend I
know what’s best here, so I’d love for an experienced operations person to
describe the ups and downs of each.

I need some metrics so I can better understand how much memory a server will
realistically need to cope with every-day use. This along with a bunch of other
metrics should be sent to something like Grafana, but this is perhaps a
slightly different though related topic.

## Single-step deployment

Deploying my app should only ever be a command away. Whether it’s
`./scripts/deploy.sh` or `git push dokku master` doesn’t matter. It should just
be easy to deploy. I would probably want database migrations to run
automatically too. This should work across instances too, so I don’t have to
run this command once for every instance of the application.

## Easy access to the console

There will be times when I need to SSH into the production server and run stuff
in the Rails console. I don’t think “Oh, you *just* run `docker exec -it --some
--other --flags bundle exec RAILS_ENV=derp command bollocks`” is acceptable for
me, or for anyone. I just want this stuff to work so I can focus on making a
great product for my customers.

## Easy app creation

There should not be a need to keep a laundry-list of commands to run when I
want to set up a new app instance for a new customer. If Acme Inc wants to run
on Oddjob, I should be able to run one command that sets up an environment
(possibly in a VM), clones the app, creates a database, connects the database
to the app, runs migrations, etc. This new app should be included as one of the
targets for the single-step deployment I described previously.

## Help?

If I at some point figure all this stuff out, I promise I’ll write about it in
excruciating detail, only to save the next poor fool from similar heartache. If
you, my dear reader, already know all this stuff, please [talk to
me](https://twitter.com/jezenthomas). I’ll even give you money (not bags of it
though; I’m not loaded). This stuff shouldn’t still be this hard in 2014.
