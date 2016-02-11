---
title: Erlang Day Three
date: 2014-07-19
description: The Red Pill
excerpt: A look at Erlang’s strength in concurrency, message-passing and spawning, linking and monitoring processes.
tags: erlang
---

The final day of Erlang looks at Erlang’s core strength: concurrency. Erlang
allows you to spawn processes that send and receive messages, both
synchronously and asynchronously. Erlang’s processes can also be linked
together — you can have a process monitoring another process and respawning it
when it dies. This is a core idea in Erlang, and it’s why the language is so
fault-tolerant.

The challenges for this day were interesting; but I only really got into the
first one. The second challenge was to figure out how to make a process respawn
itself when it dies, and after many hours researching and chatting to seasoned
Erlang programmers, I’ve learned that this just isn’t a thing you do in Erlang.

<div id="toc"></div>

1. [Things to find](#things-to-find)
  1. [An OTP supervisor](#an-otp-supervisor)
  2. [How to build an OTP server](#how-to-build-an-otp-server)
2. [Things to do](#things-to-do)
  1. [Restart a process](#restart-a-process)
3. [Thoughts](#thoughts)

### Things to find
#### An OTP supervisor

> Find an OTP service that will restart a process if it dies.

It looks as though you would use the `supervisor` behaviour with the
`one_for_one` restart strategy from OTP for this. You can find some
documentation and examples for this
[here](http://www.erlang.org/doc/design_principles/sup_princ.html#id68617).

The name OTP (Open Telecom Platform) is a bit misleading since it’s not about
telecommunications. OTP is a bunch of libraries, conventions and tools to help
write more robust Erlang applications, so even if you’re not writing Erlang for
telecommunication apps you’ll probably still want to use OTP.

#### How to build an OTP server

In a very nice book called [Learn You Some Erlang for Great
Good!](http://learnyousomeerlang.com/) (free to read online), author Fred
Hebert provides an example of [a basic OTP
server](http://learnyousomeerlang.com/what-is-otp#the-basic-server).

### Things to do
#### Restart a process

> Monitor the `translate_service` and restart it should it die.

Without getting into too much detail, we can see this code sets up an infinite
loop and uses pattern matching to receive two different Spanish strings. If we
send the service anything other than “casa” or “blanca”, the service tells us
it doesn’t understand.

When a pattern is matched, the script sends the English string to the process
ID that was unified with the `From` variable. Erlang uses the `!` operator for
message sending, so you can see where Scala borrowed some of its design ideas
from.

~~~erlang
-module(translate_service).
-export([loop/0, translate/2, monitor/0]).

loop() ->
  receive
    {From, "casa"} -> From ! "house", loop();
    {From, "blanca"} -> From ! "white", loop();
    {From, _} -> exit("Goodbye, Mr. Anderson.")
  end.

translate(To, Word) ->
  To ! {self(), Word},
  receive
    Translation -> Translation
  end.

monitor() ->
  process_flag(trap_exit, true),
  receive
    new ->
      io:format("Creating and monitoring translation process.~n"),
      register(translator, spawn_link(fun loop/0)),
      monitor();

    {'EXIT', From, Reason} ->
      io:format("The translate_service ~p "
                "has died with reason ~p",
                [From, Reason]),
      io:format("Restarting process…"),
      self() ! new,
      monitor()
  end.
~~~

It’s not immediately obvious, but the process is definitely killed when we send
an unrecognised string. We can keep sending messages, but the translation
service won’t respond to them. We can create a monitor function that starts
another `receive` loop and watches the translation service for `exit` events.

The important line there is `register(translator, spawn_link(fun loop/0))`. The
`spawn_link` call starts a new process, returns its PID, and links the new
process to the one calling it. The `register` call creates an association in
Erlang’s registry between the name `translator` and the PID that `spawn_link`
returned.

### Thoughts

There’s plenty to like about Erlang; I like the syntax, save for the different
end-of-line characters. I like the way Erlang does concurrency; I’ve heard
concurrency can be quite awkward but with Erlang it seemed painless. If at some
point I work on a project that really needs concurrent processes, Erlang seems
like the right tool to reach for.

I found the process of manually compiling modules cumbersome, so when I
eventually return to Erlang I’d like to look into how to make a smoother
workflow. I think the options for Erlang tooling is a different rabbit hole for
a different day.

<a class="previous-post" href="/seven-languages/erlang-day-two">« Erlang: <i>Changing Forms</i></a>
<a class="next-post" href="/seven-languages/clojure-day-one">Clojure: <i>Training Luke</i> »</a>
