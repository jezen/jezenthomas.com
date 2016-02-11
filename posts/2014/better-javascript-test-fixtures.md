---
title: Better JavaScript Test Fixtures
date: 2014-09-27
description: Reducing the pain in setting up your test DOM
excerpt: Reducing the pain in setting up your test DOM.
tags: code, javascript
---

I’m a big fan of the Destroy All Software screencast series, and I thoroughly
enjoy watching Gary Bernhardt write isolated tests in Ruby. When he has to
write several lines of setup code to test just one assertion, I vicariously
feel his pain. I’ve become accustomed to the minimal amount of setup he does in
his better unit tests, and as a result I feel a whole new world of pain when
setting up JavaScript tests that need to interact with some markup.

Your typical webapp serves up a document, some styles, and a bunch of
JavaScript that runs functions when some markup of a certain predetermined
structure and state is found in the DOM. This means that for every UI
interaction we want to test, we first need to set the world up in a way that
will properly interface with the JavaScript we wrote.

Since we spend a relatively large amount of time setting up tests in JavaScript
with fixtures, I think it’s worth looking into ways of reducing that pain.
Let’s take a look at some markup and explore different ways of writing it for
our tests.

The markup I’ve chosen to work with is taken from the Browser Applications
chapter of *JavaScript Testing Recipes* by James Coglan. It’s a generic
template for handling account registrations in a webapp.

~~~html
<form method="post" action="/" class="test-form">
  <p>
    <label for="email">Email</label>
    <input type="text" name="email" id="email">
  </p>
  <p>
    <label for="password">Password</label>
    <input type="password" name="password" id="password">
  </p>
  <input type="submit" value="Sign up">
</form>
~~~

In the book, Coglan demonstrates the line-continuation method for generating
this markup in JavaScript. This is done by opening a string and escaping
newlines with backslashes. Here’s how it looks:

~~~javascript
var FORM_HTML = '\
  <form method="post" action="/" class="test-form"> \
    <p> \
      <label for="email">Email</label> \
      <input type="text" name="email" id="email"> \
    </p> \
    <p> \
      <label for="password">Password</label> \
      <input type="password" name="password" id="password"> \
    </p> \
    <input type="submit" value="Sign up"> \
  </form> \
';
~~~

This is quite good; it’s easy enough to write, and the markup is easily
discernible. I’m not too keen on all the backslashes, so let’s take a look at
some ways around this. How about string concatenation?

~~~javascript
var html = '<form method="post" action="/" class="test-form">' +
             '<p>' +
               '<label for="email">Email</label>' +
               '<input type="text" name="email" id="email">' +
             '</p>' +
             '<p>' +
               '<label for="password">Password</label>' +
               '<input type="password" name="password" id="password">' +
             '</p>' +
             '<input type="submit" value="Sign up">' +
           '</form>'
~~~

Here we’re connecting a bunch of lines together, which allows us to avoid
handling newlines. Doing string concatenation this way in JavaScript is
apparently “slow”, but low-level speed is not the metric we’re exercising here;
we’re trying to find the approach that’s least cumbersome.

The next obvious choice is to store each string as an element of an array, and
then immediately join the elements and remove the separator.

~~~javascript
var html = [
  '<form method="post" action="/" class="test-form">',
    '<p>' // etc...
].join('');
~~~

Each of these approaches work, but in each case we’re adding syntax to work
around the limitations of the language. Some people choose to move their markup
to actual HTML files, but I prefer to keep my markup closer to the related test
cases so it’s more obvious what document structure the function needs in order
to behave as expected.

To go a different direction entirely, we can try and reduce the verbosity of
our fixture by asking everyone’s favourite DOM manipulation library to build
tags for us. The tags don’t even need to be closed; jQuery just magically
understands what we write and gives us back a well-formed document. All hail
the mighty dollar.

~~~javascript
var $email, $password, $html;

$email = $('<p>')
  .append($('<label for="email">Email'))
  .append($('<input type="text" name="email" id="email">'));

$password = $('<p>')
  .append($('<label for="password">Password'))
  .append($('<input type="password" name="password" id="password">'));

$html = $('<form method="post" action="/" class="test-form">')
  .append($email)
  .append($password)
  .append($('<input type="submit" value="Sign up">'));

$('body').append($html);
~~~

Here we’re writing less HTML and still producing the same markup for our
fixture. We’ve separated each HTML element into components which are then
composed together. The phrases “less HTML” and “components” usually sound like
an improvement, but I think this example is so much more complex and introduces
so much cognitive load; I have to think about how to form my JavaScript so that
the HTML elements will be nested correctly. This involves writing some code,
then checking the markup that it generates. We either manually check, or write
some test, in which case we’re now writing some test code that tests our test
code. Ugly.

Actually, I much prefer the first example — the fixture in Coglan’s book. As I
mentioned, the document structure is easily discernible, especially compared to
the jQuery example. But how can we do away with the backslashes? There are a
couple of options.

The next JavaScript specification — ES6 — introduces template strings, which
effectively allows us to write multiline strings without all the cruft. When
ES6 is actually ready, that’s probably the road I’d go down. Right now, I’m not
sure I can be bothered jumping through all the ES6 -> ES5 transpilation hoops.

*__Note__: CoffeeScript makes writing multiline strings easy, but even if I
generally like CoffeeScript, this article is about JavaScript.*

I did find a simpler option in one of Sindre Sorhus’ open source projects
however; he wrote a Node.js project called
[multiline](https://github.com/sindresorhus/multiline) that acts as a sort of
precursor to ES6’s template strings.

I’m not writing a Node.js project, so the format he provides the code in
doesn’t work for me, but there’s not much there so it’s easy to extract. I
threw the code into an AMD module which I `require` into my test cases.

~~~javascript
define(function() {

  var reCommentContents = /\/\*!?(?:\@preserve)?[ \t]*(?:\r\n|\n)([\s\S]*?)(?:\r\n|\n)\s*\*\//;

  function parse(fn) {
    if (typeof fn !== 'function') { throw new TypeError('Expected a function'); }
    var match = reCommentContents.exec(fn.toString());
    if (!match) { throw new TypeError('Multiline comment missing.'); }
    return match[1];
  }

  return parse;

});
~~~

If we apply the parse function to our fixture, it looks like this:

~~~javascript
var multiline = require('vendor/multiline');

var fixture = multiline.parse(function() {/*
  <form method="post" action="/" class="test-form">
    <p>
      <label for="email">Email</label>
      <input type="text" name="email" id="email">
    </p>
    <p>
      <label for="password">Password</label>
      <input type="password" name="password" id="password">
    </p>
    <input type="submit" value="Sign up">
  </form>
*/});
~~~

I much prefer this approach for a number of reasons. This is by far the easiest
to understand, write and maintain. When ES6 does eventually become ready for
prime-time, it requires very little effort to remove Sindre’s multiline
function. Incidentally, I like that the markup is surrounded in a block
comment. In most editors, the syntax highlighter subdues comments which means
our eyes aren’t immediately drawn to and distracted by HTML’s ocean of angle
brackets.
