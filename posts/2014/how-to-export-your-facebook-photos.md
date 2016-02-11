---
title: How to Export Your Facebook Photos
date: 2014-08-18
excerpt: How to use a sprinkle of JavaScript to grab a local copy of all the photos you’re in (including the ones you didn’t upload!)
tags: unix, javascript
---

I recently wanted to close my Facebook account permanently and go back to
having the kind of social life where I would primarily contact my friends in
person or by telephone. The problem with the pervasiveness of Facebook is that
so many of your happy memories are captured in photographs, and none of these
photographs belong to you.

Facebook does provide a tool for exporting any data *you* have created, but
there is currently no automatic way to export all photographs you are tagged in
that were taken by other people. In my case, this meant either manually saving
a few hundred photos one-by-one, or abandoning them all into Internet
deep-space. Neither option is acceptable.

## Overview

At a high-level, there are only two steps to exporting all the Facebook photos
you are tagged in. Those are:

1. Find all of the URLs pointing to the full-sized images
2. Download the images from the URLs

I’m using Chrome on OSX with `wget`, but this approach should work fine with
other *modern* browsers and operating systems.

## Finding the URLs

To find the URLs that point to the full-sized version of every image I’m tagged
in, I wrote a small snippet of JavaScript that I can run in Chrome’s developer
console. In order for this to work, you’ll need to navigate to
`https://facebook.com/<YOUR_USERNAME>/photos` and scroll all the way down until
there are no more photos for Facebook to load.

Open up Chrome’s developer console with ⌥ + ⌘ + j, and copy and paste the
following snippet of JavaScript into it.

~~~javascript
(function(){
  var urls = [],
      thumbs = document.querySelectorAll('.uiMediaThumbImg');
  [].forEach.call(thumbs, function(img) {
    var str = img.getAttribute('style');
    str = str.slice(str.indexOf('(')+1, str.length-2)
             .split('/')
             .filter(function(token) {
               return !/^p\d{3,}x\d{3,}$/.test(token);
             })
             .join('/');
    urls.push(str);
  });
  window.open('data:text/json;charset=utf-8,' + escape(urls.join('\r')));
})()
~~~

A new window will have appeared with a list of URLs. Copy the contents of the
window and save them to a new file on the desktop. It doesn’t matter what you
call the file, but as an example I will use `images.txt`.

## Downloading the images

Now that we have a list of URLs separated by new lines, we’re able to pass this
file through a program like `wget` or `curl`. I have `wget` installed and it’s
less cumbersome, so that’s what I used. If you don’t have `wget`, you can
install it with [Homebrew](http://brew.sh/).

To download the files, change into the directory where you’d like the photos
saved, and run `wget` with the `-i` flag, passing in the path to the file which
contains the URLs. For example:

~~~sh
mkdir ~/facebook_images
cd ~/facebook_images
wget -i ~/Desktop/images.txt
~~~

Your images should begin downloading sequentially. Some files may be saved
without a proper extension; in that case, append `.jpg` to the file name and it
should work as normal.

