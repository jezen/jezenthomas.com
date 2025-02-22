---
title: HTTP Verbs in Yesod Forms
date: 2025-02-22
location: Chiang Mai, Thailand
excerpt:
    Form submissions in Yesod aren't restricted to only GET and POST. Yesod's
    runFormPost is about parsing the request body from a form submission,
    regardless of the actual HTTP verb that was used.
tags: haskell
image: http-verbs-yesod-forms/cover.jpg
---

The main function provided by the yesod-form library for processing form
submissions is [`runFormPost`][0].

As the name implies, this function processes `POST` requests. But what if you
want to use a different verb, like `PUT`? The library is a little confusing in
this way, because the _post_ in `runFormPost` doesn't actually refer to the HTTP
method that triggered the execution of the handler code.

The point of `runFormPost` is actually just about running the parser against the
request _body_, rather than the query string as would happen with `runFormGet`.

Conceptually, this makes some sense as a design decision when you remember that
web browsers natively only support `GET` and `POST` as values of the `method`
attribute in HTML forms. The difference between those two values is perfectly
reflected in the design of the library — the values in the form are transferred
in either the query string or the request body respectively.

While HTML forms only support `GET` and `POST` natively, a typical Yesod
project will also support [request method overriding][1], by applying a value
for the `_method` parameter in the query string.

In Hamlet, this looks something like this.

```
<form  method=post action=@?{(ExampleR, [("_method", "PUT")])}>
```

This form submission would then be routed to the appropriate handler.

```haskell
putExampleR :: Handler Html
putExampleR = do
  ((result, form), enctype) <- runFormPost exampleForm
  -- …
```

So, the browser sends a `POST` request, but the application actually interprets
this as a `PUT` request and routes it to the appropriate handler. We then process
the form in that handler with `runFormPost`, because the data is still
transferred in the request body.

[0]: https://hackage.haskell.org/package/yesod-form-1.7.9/docs/Yesod-Form-Functions.html#v:runFormPost
[1]: https://hackage.haskell.org/package/wai-extra#:~:text=Allows%20overriding%20of%20the%20HTTP%20request%20method%20via%20the%20_method%20query%20string%20parameter
