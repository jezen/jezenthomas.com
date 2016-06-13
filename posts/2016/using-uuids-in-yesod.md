---
title: Using UUIDs in Yesod
date: 2016-06-13
excerpt: How to use universally unique identifiers in your Yesod models.
tags: haskell
---

I've recently been tinkering with a small web application written in Haskell
with the Yesod framework. I decided I would like to use Universally Unique
Identifiers for one my models, as the identifier for the model would be exposed
to the user.

There are other reasons to use UUIDs, such as [decentralising ID
generation][krzywda], but simply not giving hints as to the size of one of my
database tables is enough of a reason for me.

Researching how to do this in Yesod proved difficult. I found [this
post][xavier] by Michael Xavier, and as he describes: *“all I found were vague,
closed tickets, and old irrelevant blog posts on the Yesod site that mentioned
UUIDs but didn’t give any good examples.”*

Michael proceeds by writing a guide to using UUIDs in his post, but he uses
`Control.Lens`, which strikes me as a little advanced for the task at hand.
Furthermore, I couldn't actually get his example to work.

Luckily, I managed to make it work using far less machinery, and here's how,
using a `Blog` model as an example:

First we need to add our `uuid` field and a uniqueness constraint on that field
so we know our UUIDs will be unique and lookups will be fast. The fields defined
in our model will correspond with fields in our applicative form.

```haskell
-- config/models

Blog
  title Text
  content Text
  user UserId
  createdAt UTCTime
  uuid Text default=uuid_generate_v4()
  UniqueUuid uuid
```

Then in our applicative form, we import two modules that give us the UUID
functions we need, and extract a value in either a monadic or applicative style.

```
-- Handler/Home.hs

import Data.UUID
import Data.UUID.V4

form :: UserId -> Form Blog
form userId = renderDivs $ Blog
  <$> areq textField "Title" Nothing
  <*> areq textareaField "Contents" Nothing
  <*> pure userId
  <*> lift (liftIO getCurrentTime)
  -- either use the following line (monadic style)
  <*> lift (liftIO $ do { key <- nextRandom; return $ toText key })
  -- ...or use this style (applicative style)
  <*> lift (fmap toText $ liftIO nextRandom)
```

I asked a helpful Haskeller in an IRC channel what the difference between the
two approaches was, and this was their reply:

> Somehow I personally like the Monad version more… But it is syntactically too
> expensive, so in this case I would probably go with the `fmap` version… but if I
> would have to do more processing on the UUID, I would definitely go the Monad
> version… but IO is a Monad type class so it has to implement also Functor type
> class… You can think that using an `fmap` is more "low level".

Are there downsides to my approach? Well, I'm not making use of Postgresql's
UUID type, instead using a simple `Text`. I'm not sure yet how to do that with
the Persistent library, but it's probably not a problem worth solving at this
point.

Happy Haskelling.

[krzywda]: http://andrzejonsoftware.blogspot.com/2013/12/decentralise-id-generation.html
[xavier]: http://michaelxavier.net/posts/2015-04-14-Adding-a-UUID-Column-to-a-Persistent-Table.html
