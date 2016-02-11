---
title: Lazy-Loading AMD Modules
date: 2014-02-20
description: An intelligent approach to JavaScript loading
excerpt: A pattern for writing modular, encapsulated and lazily-loaded JavaScript.
tags: javascript, requirejs
---

Modern web-apps run a tonne of JavaScript under the hood. If you’re writing a
web-app and you’re not structuring your code into modules, you’ll soon find
yourself jostling with a Big Ball of Mud™. This article demonstrates a pattern
for writing modular, encapsulated and lazily-loaded JavaScript.

Asynchronous Module Definition (AMD) is an API for defining JavaScript modules.
Any expansion on that is outside the scope of this article, but if you’re not
sure *why* you should be structuring your JavaScript in this format, I
recommend reading [Why AMD?](http://requirejs.org/docs/whyamd.html) from the
RequireJS documentation.

There are several implementations of AMD, and RequireJS is [probably] the most
popular of them so that’s what we’ll be using.

I’d like to thank [Derick Bailey](https://twitter.com/derickbailey) and [Kyle
Simpson](https://twitter.com/getify) for their suggestions on improving this
article.

<div id="toc"></div>

1. [Single point of entry](#toc_0)
2. [Firing up the script-loader](#toc_1)
3. [The first application module](#toc_2)
4. [A simple router](#toc_3)
5. [Encapsulation](#toc_4)
6. [A view module](#toc_5)
7. [Conclusion](#toc_6)

### Single point of entry

To begin, we’ll ensure our control flow always begins in the same place. Our
markup should only ever need one `<script>` tag — RequireJS will handle our
script-loading.

```html
<!doctype html>
<html>
  <head>
    <title>Lazy-Loading AMD Modules</title>
  </head>
  <body>
  <!-- Here comes the important bit! -->
  <script data-main="js/main" src="js/lib/require.js"></script>
  </body>
</html>
```

The `src` attribute references the script-loader, and the `data-main` attribute
references a file at `js/main.js`. We’ll use this file for configuring our
script-loader. **In case you missed it:** I excluded the `.js` extension from
the file referenced in `data-main`. This is intentional, and follows RequireJS
convention.

### Firing up the script-loader

Our script tag has referenced a file at `js/main.js`, so let’s look at that.
The purpose of this file is to set a load of configuration options before
loading the first application module. Configuration options usually include
mapping filepaths to module names, and registering dependencies for those
modules. You can read more about configuration in the [official
documentation](http://requirejs.org/docs/1.0/docs/api.html#config).

```javascript
!function() {
  'use strict';

  require.config({
    paths: {…},
    shim {…},
  });

  require(['app'], function(app) {
    app().init();
  });

}();
```

With the configuration out of the way, this IIFE with [bang
notation](https://github.com/airbnb/javascript/issues/44#issuecomment-13063933)
uses the `require` method to import the module defined in `app.js`, and invokes
the `init()` function from that module.

### The first application module

We are now looking at `js/app.js` which will implement our lazy-loader. This
module will do three things:

- Import view modules
- Define relationships between view modules and elements in the DOM
- Query the DOM for those elements, and spawn new objects from their respective associated view modules

```javascript
define(function(require) {
  'use strict';

  // Import view modules
  var aboutView = require('views/about'),
      contactView = require('views/contact');

  return function() {
    var views,
        view;

    // Define relationships
    views = {
      'about': aboutView,
      'contact': contactView
    }

    // Spawn objects
    function init() {
      for (var selector in views) {
        if (!view.hasOwnProperty(selector)) continue;
        view = document.getElementById(selector);
        if (view) views[selector]().init(view);
      }
    }

    return {init: init};
  };

});
```

First you’ll notice our `require` statements. I recommend importing modules
with this syntax because the other (and more commonly used) way quickly becomes
unwieldy when you have many modules to manage. You can read more about this
syntax in the RequireJS docs under
[sugar](http://requirejs.org/docs/whyamd.html#sugar).

Next we have our module, containing two objects. The first is a map of DOM
element selectors and view modules. The second is the exposed `init()` function
we called from `js/main.js`. I’ll talk more about encapsulation in a moment.

Inside our `init()` function we iterate over our `views` map and query the DOM
for an element matching the selector. If the element was found, the
corresponding view module is invoked. We immediately call the view module’s
`init()` function to run any necessary setup code, and pass in the DOM element
so we can cache it. We’ve already queried the DOM; there’s no need to do it
again.

### A simple router

I briefly mentioned our `views` map, but *what is it for?* 

The `views` map is essentially a simple router. In
[Backbone.js](http://backbonejs.org/#Router), the router uses regular
expressions to read URI segments, and triggers any “action” the segment is
mapped to. A typical action would be creating a new view object and running the
code inside.

A core difference between that approach and mine, is that a browser can only
display the page associated with *one* URI at a time, whereas it can display
*many* DOM elements. Backbone.js checks URIs; I check DOM elements.

Using this approach, we can manage commonly used components (headers, footers,
sidebars *etc.*) in exactly the same way as unique pages. The main content of a
unique page is a view, and a header is also a view. Unifiying the way we
categorise views might not make any difference in terms of technical
complexity, but I feel it’s a good step towards reducing *organisational
complexity*.

### Encapsulation

Our application module returned a function that contained the bulk of our code.
This function is also a module, and follows the *Revealing Module* pattern. All
variables in the module are private until we attach them to an object that we
return at the end.

You can use private functions if you feel your application calls for it, but
bear in mind there is a significant tradeoff: without some ugly hacks, it’s
*impossible* to unit test private functions. The approach I’m using is to
prefix functions with an underscore if I intend for them to be private.

### A view module

The last thing we’ll look at is a simple view module. At the very least, we’ll
return a module that contains an `init()` function. The `init()` function
should be added to the object that we return, so we can expose it to our
application module.

```javascript
define(function(require) {
  'use strict';

  var someDependency = require('some/dependency');

  return function() {
    var el;

    function init(element) {
      el = element;
      // bind some events
      // call some functions
    }

    function doStuff() {
      // this is private (but can’t be unit tested)
    }

    return {init: init};
  };

});
```

When we created the view module object from the application module earlier, we
passed the DOM element as a parameter to the `init()` function. We can cache it
here in the view module, so that we don’t need to query the DOM for it again.

I’m wrapping my setup code in the `init()` function because it gives me
finer-grained control over which functions are called and when. When a user
visits the webpage that all of this module code runs on, they’re going to want
all of the code to run. When you’re writing unit-tests however, you want to
test each function in isolation. This would be much more difficult if all of
the view setup code was run before each test.

### Conclusion

We’ve looked at how to separate our code into more easily-digestible modules,
encapsulate private data and functionality, and reduce the number of redundant
DOM queries and function calls.

This pattern is in some way inspired by the current throng of front-end
JavaScript frameworks, and this article is in some part inspired by a tweet by
[Bartek Drozdz](https://twitter.com/bartekd). If you’d like to see a complete
example, you can check out a repository I made for this [on
Github](https://github.com/jezen/amd-lazy-loading).

