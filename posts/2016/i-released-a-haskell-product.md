---
title: I Released A Haskell Product!
date: 2016-07-10
excerpt: The story of building and launching my first Software-as-a-Service product, asimpleform.com.
tags: haskell
---

<span class="run-in"><span class="drop">I</span> love simplicity</span>, and I
love static website generators. This blog you are reading now is a static
website.  Marketing sites for companies I have worked with have been static
websites. There is one drawback of static websites though: because you don't
have a backend system to receive form data, you can't have a contact form.

A few weeks ago, I quietly released [asimpleform.com][asimpleform] to remedy
that. If you have a static website and you want a contact form, you follow three
simple steps:

1. Sign up to my service
2. Create a form endpoint
3. Point your form to the unique URL that my service gives you

Whenever someone uses your contact form, the data is immediately emailed to you.

The service is currently free and always will be for basic functionality. There
are other services that meet the same functional need, but they charge through
the nose for it. For example, Thoughtbot's ‘FormKeep’ service doesn't have a
plan cheaper than $29 per month. I'm not sure about you, but asking $348 per
year for a simple form endpoint strikes me as a bit of a rip-off.

There will always be a basic free tier, which should be enough for most people.
My intention is to add more advanced features for power users as the service
grows. The service accepts both normal form submissions and JSON requests, so if
you want a more AJAX-y experience for your user then that's fine. In the future
I may add analytics, storage, and custom redirects so your user never sees a
asimpleform.com branded page.

Not only is this my first *Software as a Service* product, but it's also my first
*Haskell* product. I chose to build the service with Haskell mostly as a learning
experiment. I'm using the Yesod framework, and I'm deploying to an Amazon EC2
instance with NixOS and NixOps. As I worked through the project, I documented
everything I did and why. This will form the basis of the book I am now writing,
[Haskell, But Quickly][haskellbutquickly].

What I've found when working with Haskell relative to Ruby is I have a much
higher level of confidence in the system even without writing any tests. One of
the problems I have with a dynamic language like Ruby is that I can only be
confident about catching errors that I explicitly wrote test cases for. To be
slightly meta about it, what I don't know when testing in Ruby is all the things
I don't know. I don't know all the potential edge-cases. With Haskell's GHC
compiler, this problem goes away. In most cases, the compiler shows me the flaws
in my logic.

The other issue I have with building systems in Ruby is that I find documenting
all the object contracts with TDD to be fatiguing. A large proportion of my
isolated tests for any given Ruby project just assert that the correct method
was called the correct number of times, with the correct arguments in the
correct order. All of these tests can just cease to exist because of Haskell's
type system. If my types don't line up, the system doesn't compile.

Building with Haskell has been a super-positive experience, and I'm glad I took
the time to invest in it. Immersing myself in a real project has forced me to
learn the language beyond simple arithmetic exercises that most Haskell
tutorials are full of. Some Haskell tutors argue that the language should be
studied more slowly and that you shouldn't attempt a project early on in case
your lack of knowledge overwhelms you and you burn out. I plainly disagree with
this. In my case, having a powerful tool like Haskell and not being able to do
anything with it makes me want to give up computers and read a book instead.

Give [asimpleform.com][asimpleform] a try. If you have any feedback or if for
any reason it doesn't work, please write to me at [jezen@jezenthomas.com][email]
or [shoot me a tweet instead](https://twitter.com/jezenthomas).

[asimpleform]: https://asimpleform.com/
[haskellbutquickly]: https://haskellbutquickly.com/
[email]: mailto:jezen@jezenthomas.com
