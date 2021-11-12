---
title: Folding Paragraphs in Vim
date: 2014-10-29
location: Göteborg, Sweden
excerpt: Tame hairy text files with a fold expression.
tags: vim
---

I find myself working with long, hairy text files surprisingly often. The
translated strings for the Vaamo web app and the legacy database annotations
for Oddjob are two examples. The format is always basically the same; chunks of
related text together, separated by blank lines:

~~~text
# messages.de

auth.login.failed=Bitte überprüfe Deine Eingaben.
auth.login.forgotPassword=Passwort vergessen?
auth.login.title=Anmeldung

error.iban.invalidChecksum=Die Prüfziffer ist leider nicht korrekt
error.max=Der Wert muss kleiner als {0} sein
error.min=Der Wert muss größer als {0} sein
~~~

Imagine instead of three lines per paragraph, it’s a hundred. I don’t want to
see every single line; it’s too much noise. What I want to see is a high-level
overview of the file so I have a chance at navigating through it. I want each
paragraph to fold into its top line. I keep forgetting how to do this in vim,
so I’m writing it here for future reference.

~~~text
:set fde=getline(v:lnum)=~'^\\s*$'&&getline(v:lnum+1)=~'\\S'?'<1':1
:set fdm=expr
~~~

This first line sets the fold expression to one which handles paragraphs. The
second line tells Vim to use the expression method for folding. There are a
couple more fold expressions to be found in `:help fold-expr`. If I still find
this painful in the future, it’s likely those lines will find their way into my
`~/.vimrc`.
