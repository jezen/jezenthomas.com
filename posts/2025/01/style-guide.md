---
title: Supercede's House Style for Haskell
date: 2025-01-20
location: Đà Nẵng, Vietnam
excerpt:
  The house style that has emerged for all Haskell code written at Supercede.
tags: haskell
image: haskell-style-guide/cover.jpg
---

Over the years, a house style has emerged for all the Haskell code we write at
Supercede. We expect all new code to generally follow this style, and since
style pertains to more than just the language itself, this style guide will
touch on some aspects of the Yesod ecosystem and software design more broadly.

## Philosophy

Style is taste, and taste is subjective[^1].

There isn't one right way to write Haskell code, which is why I format most
Haskell code manually. I might use [tabular][tabular] to align some record
fields or case matches, and I always use [stylish-haskell][stylish] to neatly
sort and align language extensions and module imports. But beyond that, it's
manual.

Good Haskell code tends to use a kind of visual shape to improve readability.
Carelessly written code will just _look_ haphazard. This is why you need to use
your eyes. The other programmers who work with the code you write will also be
using their eyes.

Avoid the kind of learned helplessness where you decide that since style is
subjective, you couldn't possibly individually determine whether your code is
neat and tidy, and therefore _must_ use one of the more invasive code
formatters like Ormolu. Use your eyes. Develop some taste.

And while it's good to develop an opinion on style, be cool with it. Don't go
too far the other way and decide your sense of style is absolutely superior and
then start reformatting everyone else's code as soon as you see it.

Be intentional with the changes you make.

Write code with care, but don't be precious.

## Whitespace

In general, indent code with 2 spaces.

Sometimes you may wish to use 3 spaces, as in cases like this. This is fine.

```haskell
someFunction x =
  let h = g . f
   in h x
```

Separate functions with a blank line. Separate language extensions from the
module header with a blank line. Sometimes it can be clearer to separate case
matches with blank lines. Use your judgement.

Don't put a blank line between a type signature and the function definition.

```haskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Home where

import Control.Error.Util (note)
import Data.Aeson
import Data.Text.Format.Numbers (prettyI)
-- etc…
```

There should be no trailing whitespace. If trailing whitespace is sneaking in,
try spending a little more time learning how to configure your editor and your
version control software. If this is alien to you, ask for help.

You can (and should) configure stylish-haskell to strip trailing whitespace. Here's the relevant configuration.

```yaml
steps:
  - trailing_whitespace: {}
```

In general, surround binary operators with a single space on either side.
Sometimes people prefer to eliminate whitespace surrounding the entity field
projection operator `(^.)` when writing SQL queries with the esqueleto library.
At least within each query, this should be consistent. Writing part of your
query with `f^.FooId` and part of it with `f ^. FooId` is just sloppiness.

```haskell
-- This is what I would do
select $ do
people <- from $ table @Person
where_ $ people ^. PersonName ==. val "Doomguy"
pure people

-- This is also fine, but maintain consistency within queries
select $ do
people <- from $ table @Person
where_ $ people^.PersonName ==. val "Doomguy"
pure people
```

When using a `where` clause, indent it by one level. If your `where` clause is
short, it's fine to write it on one line. When you have several definitions,
write them under the `where` keyword. Avoid hanging indents.

```haskell
-- This is fine
someFunction :: a -> b
someFunction a = rickRoll a
  where rickRoll = giveYouUp . neverGonna

-- This is also fine
runHandler :: Handler a -> YesodExample App a
runHandler handler = setup >> handler >> tearDown
  where
  setup = do
    -- …
  tearDown = do
    -- …

-- This looks rather awkward…
hangingIndent :: MonadIO m => a -> m b
hangingIndent a = fromAToB a
  where fromAToB = do
          someEffect
          someOtherEffectForNoReason
          thisCodeLooksABitWeird
```

Don't use tabs.

## Line Length

Aim to limit lines to 80 columns. Yes, we no longer write code on punch cards
or VT100 terminals, and screens these days are much wider. But _human eyes
haven't changed_!

Newspapers and magazines limit line lengths in the articles they publish. This
style guide you're reading now sets a maximum width on paragraphs. Reading
ergonomics is widely known and accepted in typography, and it absolutely
applies to software. Code is, after all, written primarily for human
consumption.

If you can't work within an 80 column limit, then the names you're using are
excessively verbose or you're trying to do too much all at once. Or both.

![](/static/img/haskell-style-guide/linelength.gif)

## Comments

Write comments that explain why the code exists, or why it's implemented the
way that it is if it isn't obvious. Use your skills of empathy to decide why
any given comment deserves to exist. Write in plain English with correct
sentences. If you're documenting parts of the code, use valid Haddock syntax.

Don't add boilerplate comments. Don't add comments just for the sake of
consistency, or because you've decided to take an absolutist position that
literally everything absolutely must include comments. If a comment doesn't
help the reader understand something, then it's noise, which means it's making
it _harder_ for the reader to follow and understand the code.

## Unicode

Haskell supports unicode! You can write code like this!

```haskell
writeFileStream ∷ ∀ α . (MonadIO m, IOData α) ⇒ FilePath → α → m ()
writeFileStream = writeStream ∘ File
```

You can, but don't. It's fiddly, and it will frustrate your colleagues.

If using the fancy symbols brings you that much joy, then use something like
[vim-haskellConcealPlus][conceal]. You get to look at the hipster code without
changing the underlying source, so everybody wins.

## Alignment

### Language Extensions

Sort language extensions, one per line. The `LANGUAGE` part should be
uppercase. Don't align the right-side of the pragma. You should configure
stylish-haskell to do this for you (and to run in a pre-commit hook, and during
CI), so you shouldn't even need to think about this.

```haskell
-- This is good
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnicodeSyntax #-}

-- This is not good
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE UnboxedTuples     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

-- This is definitely not good
{-# language
  BlockArguments, Strict, LambdaCase, MagicHash,
  UnboxedTuples, OverloadedStrings, UnicodeSyntax
#-}
```

The stylish-haskell configuration for this part should look like this.

```yaml
steps:
  - language_pragmas:
      style: vertical
      align: false
      remove_redundant: false
```

Module-specific GHC options come before language extensions.

```haskell
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
```

### Module Imports

Write import qualification in postpositive position. Don't align imports. Keep
all the imports together.

It should look like this.

```haskell
import App
import Control.Lens (Lens', _Just, lens, view, (&), (.~), (?~), (^.), (^?))
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BL
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8')
import Instances ()
-- etc…
```

Use stylish-haskell for this. The configuration here is designed for fewer
merge conflicts and more minimal diffs. These properties are more important
than visual alignment.

```yaml
steps:
  - imports:
      align: none
      list_align: with_module_name
      pad_module_names: false
      long_list_align: new_line_multiline
      empty_list_align: inherit
      list_padding: 7 # length "import "
      separate_lists: false
      space_surround: false
      post_qualify: true

columns: 80
```

Matt Parsons wrote an [in-depth explanation][imports] for this configuration if
you want to learn more.

### Module Exports

You don't always need to specify what your module exports, but it's generally a
good idea. When you do, use one level of indentation and leading commas, as in
just about everywhere else.

```haskell
module Foo
  ( getUsersIndexR
  , postUsersIndexR
  , getNewUserR
  , getUserR
  , putUserR
  , deleteUserR
  ) where
```

### Sum Types

If you have a small sum type, it's fine to write it on one line. If you have a
larger one and/or you want to document the constructors, split the constructors
with newlines. In the latter case, use one level of indentation for the
constructors.

```haskell
-- This is fine
data Small = This | Is | Fine
  deriving (Eq, Read, Show)

-- This is also fine
data Cocktail
  = Mojito
  | Negroni
  | DryMartini
  | Cosmopolitan
  | PinaColada
  | OldFashioned

-- NOOOOOOOOO!! God! No! NOoOoooOoOOOO!!!!1!
data Mocktail = VirginMojito
              | ShirleyTemple
              | ArnoldPalmer
```

### Records

Sometimes record fields align nicely, and sometimes they don't. Don't try too
hard to make them align nicely. Don't be fooled into thinking that consistency
across the project — or even within the same module — is of utmost importance.
It just isn't.

In general, use leading commas.

```haskell
-- This is fine
data User = User
  { userName :: UserName
  , userCreatedAt :: UTCTime
  , userEmail :: Email
  , userIsArchived :: Bool
  }

-- This is also fine
data User = User
  { userName       :: UserName
  , userCreatedAt  :: UTCTime
  , userEmail      :: Email
  , userIsArchived :: Bool
  }

-- This is silly. Don't do this.
data Company = Company
  { companyName                         :: Text
  , companyRegisteredOfficeAddressLine1 :: Text
  , companyCity                         :: Text
  }
```

## Type Signatures

All top-level functions should include type signatures.

If your function is small and for the sake of expediency you don't document it,
aim to write the type signature on one line.

```haskell
myFunction :: Foo -> Bar -> Baz
myFunction = _
```

If your type signature is more complex and/or you want to document the purpose
of its arguments, shuffle each argument onto its own line. Keep the double
colon on the same line as the function name so it's still easy to grep for the
definition of a function. Align the arrows on the left. Typeclass constraints
can be parenthesised, or split onto new lines. Use your eyes and your
judgement.

```haskell
-- | Do an important business thing with a user and a different company
--
-- For a given user and some company they don't belong to, do foo bar baz…
doTheThing ::
     MonadIO m
  => MonadLogger m
  => UserId -- ^ The currently logged in user
  -> CompanyId -- ^ The company the user wishes to foo bar baz
  -> SqlPersistT m ()
doTheThing userId companyId = _
```

Sometimes it's nice to align the Haddock comments which document the meaning or
purpose of each argument, and sometimes it isn't! Don't try too hard to make
this align nicely visually. If it works, great. If it doesn't, leave it.

When documenting function arguments, conventional wisdom applies — don't add
comments that just repeat what the type signature already tells you. Comments
explain things. They don't just repeat what's right there in the code.

## Names

Naming things is one of the classically hard parts in software engineering.

In Haskell, it's idiomatic to be laconic. Short — even single letter — names
can be ok. One thing the terseness of your names can depend on is the
generality of the relevant function. For example, if you're trying to
demonstrate function composition without distracting the user by implying some
context which doesn't exist, it would _only_ make sense to use single-letter
names.

```haskell
f :: a -> b
f = _

g :: b -> c
g = _

h :: a -> c
h = g . f
```

Most functions you write in production Haskell code won't be quite so general,
so it makes sense to give them slightly more verbose names. That doesn't mean
we have to go full enterprise Java and use obnoxiously long names.

```haskell
-- Don't do this.
theFunctionWhichSometimesDoesTheThingButAlsoDoesNotDoTheThingOnSundays :: IO ()
theFunctionWhichSometimesDoesTheThingButAlsoDoesNotDoTheThingOnSundays = do
  currentDay <- utctDay <$> getCurrentTime
  let (_, _, weekDay) = toWeekDate currentDay
  if weekDay == 7
  then
    print "You survived this round."
  else
    void $ readProcess $ shell "rm -rf /* --no-preserve-root"
```

While shorter names are generally better, try to avoid using jargon.
Abbreviations are a form of jargon, and some of it is fine. When you're writing
a data-intensive application, it's reasonable to use `DB` in place of
`Database`. Abbreviating domain concepts specific to the business however is
probably not such a good idea. At Supercede, our software models the
reinsurance domain. One of the things we model is a _line of business_, and
this is often abbreviated as `LOB` in our codebase. But this is not good. It
won't be obvious to a programmer new to the project what this means. It might
not even be obvious to me after a bottle of wine.

```haskell
-- No
getLOBs :: DB [ Entity LOB ]
getLOBs = _

-- Yes
getLinesOfBusiness :: DB [ Entity LineOfBusiness ]
getLinesOfBusiness = _
```

Avoid using names that are so generic they are near enough meaningless, like
"helper".

Design for qualified imports. If you have module called `Email`, it should
probably expose a function called `parse` so that it can be imported qualified
and called with `Email.parse`, rather than the clumsy `Email.parseEmail`.

When naming record fields, prefix each field with the name of the record. Avoid
abbreviating the field name prefixes. Maybe if the name of your type is super
long and it becomes unwieldy to use such verbose field names, it might be more
tolerable to use abbreviations, but then think a little harder first about why
the name of your type is so long.

```haskell
-- Yes
data User = User
  { userName :: UserName
  , userEmail :: Email
  , userDateOfBirth :: Day
  }

-- Avoid
data User = User
  { uName :: UserName
  , uEmail :: Email
  , uDateOfBirth :: Day
  }
```

## Lambda Case

The `LambdaCase` language extension is a benign syntax sugaring, and it helps
keep the code searchable by reducing the number of times the function name is
repeated in its definition. Use it liberally.

```haskell
-- Not great. This makes it harder to search for mixDrink.
mixDrink :: Cocktail -> IO ()
mixDrink Mojito = _
mixDrink Negroni = _
mixDrink DryMartini = _
mixDrink Cosmopolitan = _
mixDrink PinaColada = _
mixDrink OldFashioned = _

-- Totally better. Less noise. Easier to read and search.
mixDrink :: Cocktail -> IO ()
mixDrink = \case
  Mojito -> _
  Negroni -> _
  DryMartini -> _
  Cosmopolitan -> _
  PinaColada -> _
  OldFashioned -> _
```

## Wildcards

Avoid using wildcards in pattern matches. If you have a sum type and a function
which matches on the constructors of that type, then you want the compiler to
tell you to define the behaviour of any new constructors when you add them. If
you use wildcards then the compiler won't help you.

```haskell
data Thing = Foo | Bar | Baz | Quux

-- Avoid using wildcards like this
case thing of
  Foo -> _
  Bar -> _
  Baz -> _
  _   -> print "This probably isn't the behaviour you wanted"

-- This is better. The compiler will tell us we're missing a case for Quux.
case thing of
  Foo -> _
  Bar -> _
  Baz -> _
```

## Compiler Warnings

All compiler warnings should be upgraded to errors. Your code should not be
producing warnings. Enable all the warnings. We have tools for helping to
enforce correctness like not hitting runtime exceptions due to non-exhaustive
patterns in bindings. We need to use our tools.

## Tests

Having an awesome type system doesn't absolve us of writing good tests[^2].

Reaching 100% code coverage is not a goal per se. Using tests to drive the
design of your system _is_ a goal. Try to write tests first. Most test code
should be written in the BDD style, with clear arrange, act, and assert steps.

```haskell
spec :: Spec
spec = withApp $ do

  describe "getFirstDomainWithoutRecentFavicon" $ do

    describe "when a domain has no favicon" $ do

      it "returns the domain" $ do
        now <- liftIO getCurrentTime
        let domainName = toDomainName "example.com"
            domain = Domain (fromJust domainName) 0 0 now
        domainId <- runDB $ insert domain
        result <- runDB $ getFirstDomainWithoutRecentFavicon now
        liftIO $ result `shouldBe` Just (Entity domainId domain)
```

The test descriptions should ideally concatenate into intelligible sentences in
English. You should be able to naturally read this test as

> `getFirstDomainWithoutRecentFavicon`, when a domain has no favicon, returns the domain.

Tests should be self-contained as much as is reasonable. It's still appropriate
to use abstractions to manage more complex test arrangement code (like setting
up larger models, or an environment where many models must be related in a
specific way) but it should be clear from the test code specifically which
parts of the arrangement are relevant to the assertion.

The programmer reading the test code shouldn't have to go hunting for
implementation details elsewhere to be able to make sense of the test code.

## Handlers

A significant proportion of code in a typical Yesod project will live inside
handler code.

```haskell
getUserR :: UserId -> Handler Html
getUserR userId = do
  user <- runDB $ get404 userId
  defaultLayout $ do
    setTitle $ toHtml $ userName user
    $(widgetFile "user")
```

A function this small is completely fine, but handler code is versatile. You
have access to the current request. You can do logging. You can run anything in
IO. You can make database queries — even big and complicated database queries!

Handler code shouldn't be allowed to grow so much. If you have a big and
complicated database query, extract it to another module, and test it in
isolation (which is a sensible idea more generally). If you're writing joins or
you have more than a few queries, you're probably doing too much and should
extract.

If your handler needs to process a form submission, extract the form code to
another function (but keep it in the same module).

If you have functions which transform the data you're trying to serve, move
them to a `where` clause, or make them top-level if you want to test them in
isolation. Maybe it should be library code?

Use the visual size and shape of the handler code to guide you towards neat
mechanical refactorings.

Handler code is inherently harder to test, because it requires integrated
tests, and often requires stubbing external services. Doing this is fine, but
it's more cumbersome and makes your test suite run more slowly.

Avoid this if you can help it.

## Hamlet

Skip writing the tag name when writing a `div`. All elements are implicitly
`div`s unless otherwise specified.

Use the `newIdent` function to have the runtime generate unique identifiers.
Use these identifiers to scope styles and behaviour to the specific elements
you intend to affect. Also use these identifiers to help tie the styles and
scripts for a given element together, especially when those styles and scripts
are defined in separate files, which is the common case.

Use descriptive names for elements. Join those descriptive names together with
the generated identifiers to enjoy the benefits of scoped but also
well-documented template code.

Use internationalisation when you have ambitions of world domination.

This is some nice, idiomatic Hamlet code.

```
$newline never

$# I am a comment, and I will not appear in the rendered source.

<##{theId}>
  <h2##{theId}-title>
    _{MsgTheTitle}
  <p##{theId}-description>
    _{MsgDescription}

  $if not (null things)
    <ul##{theId}-things>
      $forall thing <- things
        <li.#{theId}-thing>
          #{thing}
```

Don't forget that your generated HTML still needs to be valid! Run the
generated HTML through one of the available validators.

## Esqueleto

Use the new, _experimental_ syntax when writing SQL queries with the esqueleto
library.

Aim to design modules such that many esqueleto functions are kept together with
not much else, so that you can use the functions without qualification.

```haskell
-- This is nice!
select $ do
(people :& blogPosts) <-
  from $ table @Person
  `leftJoin` table @BlogPost
  `on` (\(people :& blogPosts) ->
          just (people ^. PersonId) ==. blogPosts ?. BlogPostAuthorId)
where_ (people ^. PersonAge >. just (val 18))
pure (people, blogPosts)

-- This is super noisy. Not so nice.
E.select $ do
(people E.:& blogPosts) <-
  E.from $ E.table @Person
  `E.leftJoin` E.table @BlogPost
  `E.on` (\(people :& blogPosts) ->
            E.just (people E.^. PersonId) E.==. blogPosts E.?. BlogPostAuthorId)
E.where_ (people E.^. PersonAge E.>. E.just (E.val 18))
pure (people, blogPosts)
```

Try to strike a balance between line lengths and function length. Separate new
join clauses onto individual lines. Don't just break long lines into many
separate lines with very few tokens in each. You shouldn't end up with a weird
column of code in the middle of your page.

Don't do this. The `where_` clause here is hard to read.

```haskell
select $ do
  (people :& blogPosts) <-
  from $ table @Person
  `leftJoin` table @BlogPost
  `on` (\(people :& blogPosts) ->
          just (people ^. PersonId) ==. blogPosts ?. BlogPostAuthorId)
  where_
    ( people
        -- ಠ_ಠ
        ^. PersonAge
        >. just (val 18)
        &&. people
        ^. PersonIsAdmin
        ==. val True
        &&. people
        ^. PersonCreatedAt
        <. val oneWeekAgo
        &&. blogPosts
        ^. BlogPostPublished
        ==. val True
    )
pure (people, blogPosts)
```

## Persistent Entities

We use persistent to describe our tables and their corresponding Haskell types.

Pluralise table names. Avoid primitive obsession here — don't just model
everything with `Text` values. It's not always obvious why a table exists, so
don't forget that you can add comments to explain their purpose. Align the
types if you wish, but don't try too hard to make them align. Almost all models
should have a `createdAt` field, and most of them should also have a `createdBy
:: UserId` field.

```
-- This looks good
User sql=users
  name UserName
  email Email
  dateOfBirth Day
  createdAt UTCTime
  UniqueUser email

-- This… not so much
Company
  regNumber                    Text
  registeredOfficeAddressLine1 Text
  registeredOfficeAddressLine2 Text
  city                         Text
  postcode                     Text
```

## Type Aliases

In practice, type aliases don't bring much utility. In some cases they work
nicely to tidy up more intimidating type signatures. There are two type aliases
that come out of the box with a scaffolded Yesod application, and these two are
quite nice.

```haskell
-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a = forall (m :: Type -> Type).
  (MonadUnliftIO m) => ReaderT SqlBackend m a
```

In my opinion, it's not generally useful to rename primitives. Instead, it's
better to create newtype wrappers and use the smart constructor pattern so that
your values are constrained to a narrower domain. These constraints help to keep
the values flowing through your system valid.

[^1]: Thanks, Captain Obvious.
[^2]: And no, the necessity of tests does not somehow make Haskell's type system less important.

[tabular]: https://github.com/godlygeek/tabular
[stylish]: https://github.com/haskell/stylish-haskell
[conceal]: https://github.com/enomsg/vim-haskellConcealPlus
[imports]: https://www.parsonsmatt.org/2020/03/17/gradual_import_style_improvements.html
