---
title: Bleep. Bloop. I Am Approving Your Transaction.
date: 2019-10-05
location: Gdynia, Poland
tags: business
banner: robot.jpg
---

I used to work at a company that behind the scenes had to use a user interface
in a novel way for part of their system to work.

This company's web-based user interface would allow its users to initiate bank
transactions. Nothing would appear out of the ordinary from the user's end.
However, the company had to _approve_ the transactions in real time,
synchronously.

The approval was software-based, but in a closed system. The approval mechanism
ran in a third-party iOS application, and we couldn't just run it in a
simulator.

The company could do all sorts of programmatic anti-money laundering and fraud
prevention verification on the user, the recipient, the transaction, _etc._, but
we had to actually _physically press_ a button on the screen of a smart
telephone to approve the transaction. There was no programmatic way around
this.

Some programmers at the company came up with a robotic arm attached to a
Raspberry Pi that could be controlled programmatically, which would run 24/7
and do the telephone screen pressing for us. If I remember correctly, it was
also attached to a small bell, so we received a low-tech kinetic notification
in the office for every transaction approval.

I always thought that was pretty clever.
