---
title: Using Libsass Today
date: 2015-02-11
excerpt: How I configured Libsass without node, npm, and all that needless headache.
tags: code
---

<span class="run-in"><span class="drop">I</span> recently began working</span> on a static site for vaamo. The static site generator
we chose is written in Python, so that’s one dependency. We like writing Sass
with the old indent syntax, and the original Sass implementation is written in
Ruby so that’s another dependency. I love Ruby, but I feel a bit dirty having to
depend on it in a Python project.

There is a newer, faster Sass compiler called Libsass, which is written in
C/C++. Libsass is a library and doesn’t come with an executable, so we needed
some sort of driver to control it with. It seems most people use some flavour of
Node.js wrapper, so that’s what I tried first.

I reached for node-sass, which is the most popular Node.js binding for Libsass.
This is a simple case of:

~~~sh
npm install --save node-sass
./node_modules/.bin/node-sass
~~~

On my machine, it worked. Great success. On our TravisCI build server, it
exploded with a cryptic message.

~~~sh
/home/travis/build/company/top_secret/node_modules/node-sass/lib/index.js:21
    throw new Error('`libsass` bindings not found. Try reinstalling `node-sass
          ^
Error: `libsass` bindings not found. Try reinstalling `node-sass`?
    at getBinding
(/home/travis/build/company/top_secret/node_modules/node-sass/lib/index.js:21:11)
    at Object.<anonymous>
(/home/travis/build/company/top_secret/node_modules/node-sass/lib/index.js:211:23)
    at Module._compile (module.js:456:26)
    at Object.Module._extensions..js (module.js:474:10)
    at Module.load (module.js:356:32)
    at Function.Module._load (module.js:312:12)
    at Module.require (module.js:364:17)
    at require (module.js:380:17)
    at Object.<anonymous>
(/home/travis/build/company/top_secret/node_modules/node-sass/lib/render.js:3:12)
    at Module._compile (module.js:456:26)
make: *** [sass] Error 8
~~~

<img src="/img/bimmy.png" style="float: left; border: 1px solid #333; margin:
5px 15px 5px 0;">
This is painful, but not new. Let it be known that npm is a colossal clusterfuck
of complexity and it makes me feel like a sad Jimmy Wales. Fortunately, there is
a C binary wrapper around Libsass called SassC. It is perhaps slightly less
convenient because it needs to be compiled for the machine which consumes it,
but I’d rather machines spend a little more time thinking and less time randomly
exploding because JavaScript. At a high level, we need to:

- Pull down both Libsass and SassC
- Tell the machine where to find the Libsass directory
- Compile SassC
- Compile Sass files with the resulting binary

In order to pull down Libsass and SassC, you should have wget installed on your
system. If you don’t, you can install it with [homebrew][1].

~~~sh
brew install wget
~~~

From your project directory, run the following commands:

~~~sh
wget https://github.com/sass/sassc/archive/3.1.0.tar.gz
tar xzf 3.1.0.tar.gz
cd sassc-3.1.0
wget https://github.com/sass/libsass/archive/3.1.0.tar.gz
tar xzf 3.1.0.tar.gz
cd libsass-3.1.0
export SASS_LIBSASS_PATH="$(pwd)"
cd ..
make
~~~

You should be left with a binary in the `bin` directory that you can use to
compile your stylesheets.

[1]: http://brew.sh/
