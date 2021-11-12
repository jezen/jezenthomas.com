---
title: How I Write Invoices in Vim
date: 2015-09-05
location: Gdynia, Poland
excerpt: I use Vim for writing code, composing emails, taking notes, and just about everything. It makes sense to use it for building business paperwork too.
tags: vim
---

<span class="run-in"><span class="drop">S</span>ince starting my own
company</span> back in April, I've had to piece together some fairly mundane
business boilerplate, *e.g.*, invoices, at the end of each month.

I think people typically use something from Microsoft Office for this, or maybe
some online invoicing web application, but I'm quite happy to stay in the land
of Vim for these tasks.

I learned that there are some special configuration details that need to be
ironed out in order to make invoices render nicely, but it's mostly a
set-it-and-forget-it experience.

All of my invoices are kept in the same directory on my system, and I keep a
special `.vimrc` file in that same directory which contains a few rules that
provide a sane invoice editing experience. Here's that very small file:

~~~vim
set tw=80
set bg=light
set nolist
set nofoldenable
set nonumber
set norelativenumber
~~~

The first rule sets the text width, essentially saying I want the right-edge of
my document to fall after the 80th column. This stops me from drawing a table
that wraps across lines and destroys the formatting. The second rule is
important for rendering the invoice as a PDF, as we'll see a little later. The
rest of the rules are just there for comfort and clarity.

Vim doesn't support directory-specific `.vimrc` files by default, so I added the
following couple of rules to my global `~/.vimrc`. It's important to add that
second rule, otherwise anytime you work on a project with other people they
could add some malicious code to a directory-specific `.vimrc` and your machine
would run it as soon as you open Vim from that directory.

~~~vim
set exrc " Enable use of directory-specific .vimrc
set secure " Only run autocommands owned by me
~~~

Now we come to actually writing the invoice. It's fairly standard that an
invoice begins with the name and address of your own company, followed by the
name and address of the recipient. The name and address of your own company are
usually right-aligned, and Vim handles this with the `right` Ex command. If you
do a line-wise visual selection over your address details, hit `:`, and then
type `right` and hit enter, your text is right-aligned. The command looks like:

~~~vim
:'<,'>right
~~~

The next part of the invoice contains the table of line items that you're
invoicing for. I drew the table by copying and pasting a bunch of unicode
box-drawing characters. There are probably tools for doing this automatically,
but I only had to do this once and I can mostly reuse the table for every
invoice I create.

We don't need to leave Vim to find these box-drawing characters. We can view the
fancy characters (digraphs) by issuing the `:dig` command. If you scroll down
slightly, you'll notice the three characters I use are `hh`, `vv`, and `vh`.
Hitting `<C-k>` from insert mode followed by the digraph combination gives us
the characters we're looking for.

~~~
 DESCRIPTION         │ UNIT PRICE │ QUANTITY │   AMOUNT
─────────────────────┼────────────┼──────────┼──────────
 Line of code        │      120 € │       7  │    840 €
~~~

*n.b. I don't actually charge per line of code.*

I'm too lazy to do the above multiplication in my head, so instead of reaching
for a calculator and finding the product of 120 and 7, I use Vim's expression
register. This means going into insert mode with the cursor in the table row's
amount field, hitting `<C-r>`, then `=`, and then `120*7`, and finishing with
`<cr>`.

The rest of the document is straight-forward and uninteresting. For
completeness, you can [see an example invoice in this Gist][invoiceexample]. For
fun, I drew my company logo with ASCII art using some online generator. There
are a bunch of them out there.

Once the invoice contains all the correct information and is nicely formatted,
we need to render the document in a format companies will accept. You could
print documents directly from Vim, but I found this to be somewhat fiddly and
didn't manage to make it work as well as I'd like. Besides, I want to keep a
digital copy of the rendered version anyway.

PDF seems to be the standard way to pass these documents around. You can create
PDFs directly from Vim by doing some dance with the `:hardcopy` command and
PostScript, but the easiest thing to do is just to use a HTML document as an
intermediary.

When you issue the `:TOhtml` command, Vim turns your text document into an HTML
document in a horizontal split. From here, you save the HTML document, and issue
the `:!open %` command to open the document in a web browser. The style rules
used in the HTML document somewhat reflect the colours you use in Vim, which is
why it was important to explicitly set Vim's background colour earlier (assuming
you normally use Vim with a dark background, that is).

Once you have your invoice in a HTML document in your web browser, you can print
it and/or export it to a PDF. Remember to disable any headers and footers that
your browser adds when printing.

[invoiceexample]: https://gist.github.com/jezen/c05c0944a2fc32512321
