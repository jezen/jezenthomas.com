---
title: Fast Counting with PostgreSQL and Haskell
date: 2021-12-24
location: Kyïv, Ukraine
excerpt:
  You can abuse the query planner in PostgreSQL to drastically improve
  performance when counting records in large tables, although it takes some
  finesse to implement this with Persistent and Esqueleto — two Haskell
  libraries for database access.
image: fast-counting.jpg
tags: haskell
---

One of my projects leans heavily on a PostgreSQL database. It allows users to
arbitrarily filter through millions of rows of relational data. Ideally, the
system should give the user a hint of the size of the result set. If there are
too many results, they can apply more filters to narrow their search. Too few,
_vice versa_.

Naturally, every Internet user expects search to be fast.

**Problem**: Counting large result sets in PostgreSQL is… _slow_.

While researching potential solutions to this problem, I learned that
PostgreSQL _can_ provide fast counting if you're willing to sacrifice some
accuracy. Whether or not this matters of course depends on the kind of
application you're writing. In my case, I'm fairly certain my users don't care.
Knowing that there are a few million results for a query is more than good
enough.

If a query is sufficiently focused though, you probably _don't_ want to show
the user an estimate. If the page says "About 7 results", it probably wouldn't
fill them with confidence if they can see the result length is clearly only
_five_. Besides, PostgreSQL can count result sets in the hundreds and low
thousands extremely quickly.

**Solution**: First get an estimate of the result length, and if it's
sufficiently small — say, fewer than 5,000 rows — _then_ count again with total
accuracy.

This [rather good article][0] suggests several approaches for implementing fast
counting with estimates. Not all of them work in my case, because the primary
feature of my application is arbitrary filtering. Some clever abuse of the
query planner output quickly gives us an estimate.

```sql
CREATE FUNCTION count_estimate(query text) RETURNS integer AS $$
DECLARE
  rec   record;
  rows  integer;
BEGIN
  FOR rec IN EXECUTE 'EXPLAIN ' || query LOOP
    rows := substring(rec."QUERY PLAN" FROM ' rows=([[:digit:]]+)');
    EXIT WHEN rows IS NOT NULL;
  END LOOP;
  RETURN rows;
END;
$$ LANGUAGE plpgsql VOLATILE STRICT;
```

The function above only needs to be defined once. In my case, I added it to my
project's database schema with the [`postgresql-simple-migration`][1] library,
as it's perilous to try and manage database schemas by hand.

If we're writing plain SQL, we can use the `count_estimate` function like this:

```sql
SELECT count_estimate('SELECT 1 FROM items WHERE n < 1000');
```

However, the whole point of writing Haskell is to avoid the pain of software
constantly blowing up in our faces, so it's better to lean on the
[`persistent`][2] and [`esqueleto`][3] libraries where possible to leverage the
compiler and gain _some_ assurances of type safety.

I think there are a couple of ways to write the above query with the
aforementioned Haskell libraries. Implementation in terms of
[`unsafeSqlFunction`][4] might work, but I chose to use the [`rawSql`][5]
function instead[^1].


```haskell
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Database.Esqueleto.Experimental  -- from esqueleto
import Text.Shakespeare.Text (st)       -- from shakespeare

-- | The length of a result set for a query.
data ResultLength
  = Approximate Int -- ^ An estimate from the PostgreSQL query planner
  | Precise Int     -- ^ A precise result from running a real @COUNT(*)@ query
  deriving (Read, Show)

derivePersistField "ResultLength"

-- | Quickly return an estimate from the query planner.
--
-- This leans on the PostgreSQL query planner to come up with an /approximate/
-- result length. If the @count_estimate@ function is not already defined in
-- the database, this function will throw an exception.
--
-- To prevent SQL injection, the tag should be dynamically generated.
getEstimate
  -- | A randomly-generated alphabetical string.
  :: Text
  -- | The SQL query with placeholders, and a list of parameters.
  -> (Text, [PersistValue])
  -- | The 'ResultLength' here will always be an 'Approximate'.
  -> DB (Maybe ResultLength)
getEstimate tag (query, params) =
  let resultLength :: [Single Int64] -> Maybe ResultLength
      resultLength = fmap (Approximate . fromIntegral . unSingle) . listToMaybe
   in resultLength <$> rawSql [st|
      SELECT count_estimate($#{tag}$#{query}$#{tag}$);
      |] params
```

We define a new data type with two constructors to represent the idea that the
length of a result set can either be _approximate_ (if there are more than
5,000 results), or _precise_ by running a real `COUNT(*)` query.

To be able to return that type from a database query, the compiler needs to
know how values of that type should be represented. I don't actually care how
they're represented, so I'm happy to use Template Haskell to just derive the
`PersistField` instance. If I _did_ care, I'd probably opt to serialise these
values as JSON objects. But I don't, so I didn't.

Deriving that instance this way uses the type's `Read` and `Show` instances
under the hood, so that's why those are also derived.

The `DB` type in the return type for `getEstimate` is actually a type alias
used for the sake of legibility. It's included in the scaffolding for any new
`yesod-postgres` project. Its use necessitates a couple of language extensions,
and it has the following definition.


```haskell
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

type DB a = forall (m :: * -> *). (MonadIO m) => ReaderT SqlBackend m a
```

Use of the `rawSql` function would ordinarily give us a list of single column
values, which in a type signature would look like `[Single a]`. However, this
isn't the type I wish I had. I'm using a few functions to massage the value the
query returns into something I actually want floating through my system.

1. The query is only ever expected to return one result, so a list doesn't make
   much sense. The `listToMaybe` function can safely turn it into a
   `Maybe (Single Int64)` instead.
2. The `unSingle` function unwraps whatever is inside that `Single` column
   value, giving us a `Maybe Int64`.
3. As the database actually returns an `Int64` value, the `fromIntegral`
   function is used to change that value into an `Int`.
4. The `Approximate` constructor allows us to tell our `Int` value apart from
   a precise result. This will be important for rendering the value in the UI.

With the type-juggling out of the way, we can look at using the `rawSql`
function to express the SQL query. The use of a quasiquoter here makes it neat
to interpolate values into the text value. Like Template Haskell, the
appropriate language extension needs to be enabled.

This function takes two arguments:

1. The SQL query, potentially with `?` characters to denote placeholders
2. A list of values to fill those placeholders

Conveniently enough, `esqueleto` actually provides a function called
[`renderQuerySelect`][6] for rendering a `SELECT` query into a tuple that
neatly matches these two arguments. We can pair this function with some
arbitrary query. For example, assume we have some persistent entity in our
system called a `Thing` which has a field called `thingStatus`, and we want to
select all of the things in our database where the `thingStatus` is either
`Active` or `Pending`.

```haskell
-- Get all our active and pending things…
getThings :: SqlQuery (SqlExpr (Entity Thing))
getThings = do
  thing <- from $ table @Thing
  where_ $ thing^.ThingStatus `in_` valList [ Active, Pending ]
  pure thing
```

To render this query, we should run `renderQuerySelect` with `getThings` as its
argument in the database querying monad.

```haskell
runDB $ renderQuerySelect getThings
```

We can't just stuff the result of this into the `getEstimate` function however.
The tricky part here is that we are essentially having to write one SQL query
_inside another SQL query!_ Without being careful to escape the single quotes
that were added by `rawSql`, we would end up with a query like this being sent
to the database.

```sql
SELECT count_estimate('SELECT * FROM things WHERE status IN ('Active', 'Pending')');
```

Of course, this fails as it's a syntax error.

Even the syntax highlighting on this page reveals that it is a syntax error.

One approach might be to escape the single quotes, and try to generate a query
like the following.

```sql
SELECT count_estimate('SELECT * FROM things WHERE status IN (''Active'', ''Pending'')');
```

This works, but I think it's error prone. You would need to escape the single
quotes in each of the query parameters, and _also_ escape the single quotes in
the query itself. You would need to use unescaped literals, and then add the
escaping back in manually.

Fortunately, there's a better way.

PostgreSQL supports _dollar quoting_, which means the rendered query can be
embedded like this instead (syntax highlighting disabled to avoid confusion).
With this approach, we don't need to worry about correctly escaping single
quotes.

```
SELECT count_estimate($$SELECT * FROM things WHERE status IN ('Active', 'Pending')$$);
```

This is easier to work with, but it's still not a total solution. If a user
found a way to send a `$$` sequence into your application, there would be a
syntax error again, and possibly even a SQL injection vulnerability.

A way to mitigate that threat is to add tags to the dollar quotes.

```
SELECT count_estimate($someTag$SELECT * FROM things WHERE status IN ('Active', 'Pending')$someTag$);
```

The tags must match on both sides, and as far as I can tell (though I didn't
see this specified in the documentation), the tag should only contain
alphabetical characters.

To avoid the possibility of an attacker brute-forcing the tag inside the dollar
quotes, a random alphabetical string can be generated every time the query is
run, and that random string can be used as the tag.

To tie this all together, here's the kind of thing you would write in your HTTP
request handler. If the query planner thinks the result set will be greater
than 5,000 rows then we don't bother trying to precisely count the results and
instead return the approximation. Otherwise, run the real `COUNT(*)` query.

```haskell
getThingsR :: Handler Html
getThingsR = do
  -- some handler code…

  -- Random string generation from the random-strings package
  tag <- pack <$> liftIO (randomWord (onlyAlpha randomASCII) 10)
  resultLen <- runDB $ do
    rendered <- renderQuerySelect getThings
    getEstimate tag rendered >>= \case
      Just (Approximate n)
        | n > 5000  -> pure $ Approximate n -- Return the estimate
        | otherwise -> getThingsCount       -- Run an accurate @COUNT(*)@ query
      _ -> pure $ Precise 0                 -- Should never happen…

  -- more handler code…
```

Finally, when it comes to presenting the result length to the user, we're able
to implement a more informative UI owing to the fact that our figures are
meaningfully tagged.

```
<p.results-length>
  $case resultLen
    $of Approximate n
      About #{format commas n} results
    $of Precise n
      $if n == 1
        1 result
      $else
        #{format commas n} results
```

This approach has soothed my counting performance woes in [my hobby
business][7] and I'm quite pleased with the result. Further performance gains
could likely be had from tuning the database configuration, a smarter use of
indexes, or perhaps just throwing better hardware at the problem.

I think this code would benefit from some property-based tests, which I am yet
to write. I'd like to verify that none of this function chain causes a syntax
error in the database, no matter what kind of query you throw at it. I may find
that I need to carefully sanitise inputs in addition to the dollar quote
escaping.

Hopefully this also shows that Haskell is perfectly suitable — _ideal_, even —
for real world work.

[0]: https://www.citusdata.com/blog/2016/10/12/count-performance/
[1]: https://hackage.haskell.org/package/postgresql-simple-migration
[2]: https://hackage.haskell.org/package/persistent
[3]: https://hackage.haskell.org/package/esqueleto
[4]: https://hackage.haskell.org/package/esqueleto-3.5.3.0/docs/Database-Esqueleto-Internal-Internal.html#v:unsafeSqlFunction
[5]: https://hackage.haskell.org/package/persistent-2.13.2.1/docs/Database-Persist-Sql.html#v:rawSql
[6]: https://hackage.haskell.org/package/esqueleto-3.5.3.0/docs/Database-Esqueleto-Internal-Internal.html#v:renderQuerySelect
[7]: https://newbusinessmonitor.co.uk/

[^1]: Yes, I'm aware neither `unsafeSqlFunction` nor `rawSql` are particularly _safe_ functions, but sometimes we need escape hatches. Let's not throw the baby out with the bath water.
