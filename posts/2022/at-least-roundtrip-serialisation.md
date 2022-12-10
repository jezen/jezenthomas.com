---
title: At Least Assert Your Serialisation Roundtrips
date: 2022-12-10
location: Kraków, Poland
excerpt:
  How to use property-based testing with Hedgehog and Hspec to ensure that the
  values in your system can be serialised and deserialised symmetrically.
image: reflection.jpg
tags: haskell, testing
---

When you're writing a web application, the values that flow through your system
need to be serialised and deserialised. This could be for representation as
JSON, XML, a parameter in a URI query string, or a field in a database, _etc_.

The problem with teaching your program how to serialise or deserialise values
is that it is manual and error-prone.

Consider the follow enumeration type which models the status of a company in
the United Kingdom.

```haskell
data Status
  = Active
  | AdministrationOrder
  | AdministrationOrderAdministrativeReceiver
  | AdministrationOrderReceiverManager
  | AdministrationOrderReceivership
  | AdministrativeReceiver
  | ConvertedOrClosed
  | ConvertedToPLC
  | ConvertedToUKEIG
  | ConvertedToUKSocietas
  -- 23 more constructors…
  deriving (Bounded, Enum, Eq, Ord, Read, Show)
```

If we wanted to teach our program to serialise each of these constructors as
JSON but we stuck with the defaults, we would end up with values like
`"Active"` and `"AdministrationOrder"` which mirror how the constructors are
written.

This isn't what we want though — a consumer of this JSON data would expect
something in perhaps snake case or kebab case, _i.e._, `"administration_order"`
or `"administration-order"` respectively.

Of course, we could type out the desired conversions manually, like this:

```haskell
instance FromJSON Status where
  parseJSON = \case
    (String "active") -> Active
    (String "administration_order") -> AdministrationOrder
    (String "administration_order_administrative_receiver") -> AdministrationOrderAdministrativeReceiver
    (String "administration_order_receiver_manager") -> AdministrationOrderReceiverManager
    (String "administration_order_receivership") -> AdministrationOrderReceivership
    (String "administrative_receiver") -> AdministrativeReceiver
    (String "converted_or_closed") -> ConvertedOrClosed
    (String "converted_to_plc") -> ConvertedToPLC
    (String "converted_to_ukeig") -> ConvertedToUKEIG
    (String "converted_to_uk_societas") -> ConvertedToUKSocietas
    -- 23 more lines of tedium…
    _ -> fail "Could not parse Status - was not a String"

instance ToJSON Status where
  toJSON = String . \case
    Active -> "active"
    AdministrationOrder -> "administration_order"
    AdministrationOrderAdministrativeReceiver -> "administration_order_administrative_receiver"
    AdministrationOrderReceiverManager -> "administration_order_receiver_manager"
    AdministrationOrderReceivership -> "administration_order_receivership"
    AdministrativeReceiver -> "administrative_receiver"
    ConvertedOrClosed -> "converted_or_closed"
    ConvertedToPLC -> "converted_to_plc"
    ConvertedToUKEIG -> "converted_to_ukeig"
    ConvertedToUKSocietas -> "converted_to_uk_societas"
    -- This isn't what I signed up for ಠ_ಠ
```

While this does model exactly the representation we want, clearly this is not
the kind of code we wish to maintain. At scale, it is inevitable that you or
one your colleagues will mistype one of the strings, or forget to handle a case
entirely.

A better approach would be to use some function — we are programmers after all
— to handle the conversions for us.

We can neatly convert the representations of each of these constructors
automatically with some string conversion function, _e.g._, those available in
the [`casing`][0] library. This would lead us to write JSON instances for our
type like this:

```haskell
import Control.Monad.Fail (fail)
import Text.Casing (kebab, pascal)
import Text.Read (readEither)

instance FromJSON Status where
  parseJSON = \case
    (String s) -> case deserialise s of
      Left err -> fail (unpack err)
      Right status -> pure status
    _ -> fail "Could not parse Status - was not a String"

instance ToJSON Status where
  toJSON = String . serialise

serialise :: Status -> Text
serialise = pack . kebab . show

deserialise :: Text -> Either Text Status
deserialise t =
  case readEither (pascal (unpack s)) of
    Left _ -> Left $ "Could not parse Status: " <> t
    Right status -> pure status
```

This is an improvement. We now have 15 lines of code rather than the 71 lines
of code we would have had after laboriously typing out all of the constructors.

There could be problems in the logic though, which we'll need to write a test
for. The best way I have found for writing a test for this kind of problem is
with a roundtrip property-based test.

Roundtripping in this context just means that if you take some value and
serialise it, then you'll get back the same value when you deserialise it.

I'm partial to both the `hspec` and `hedgehog` testing libraries, so naturally
it makes sense to combine them with the [`hspec-hedgehog`][1] library.

```haskell
module Model.StatusSpec (spec) where

import qualified Data.Aeson as Aeson
import qualified Hedgehog.Gen as Gen
import Model.Status (Status)
import Test.Hspec.Hedgehog (Gen, forAll, hedgehog, tripping)
import TestImport

spec :: Spec
spec = do

  describe "Status" $ do

    it "roundtrips JSON" $ hedgehog $ do
      x <- forAll (Gen.enumBounded :: Gen Status)
      tripping x Aeson.encode Aeson.eitherDecode
```

This test says:

> Generate all the `Status` values and ensure each of them roundtrip.

Hedgehog is able to give us all the values in our `Status` enumeration because
we asked the compiler to derive instances for both the `Enum` and `Bounded`
classes when we defined our type.

What happens when we run this test?

```
Model.Status
  Status
    roundtrips JSON FAILED [1]

Failures:

  ./test/Model/StatusSpec.hs:20:7:
  1) Model.Status.Status roundtrips JSON
         ✗ <interactive> failed at ./test/Model/StatusSpec.hs:20:7
           after 1 test.

              ┏━━ ./test/Model/StatusSpec.hs ━━━
           13 ┃ spec :: Spec
           14 ┃ spec = do
           15 ┃
           16 ┃   describe "Status" $ do
           17 ┃
           18 ┃     it "roundtrips JSON" $ hedgehog $ do
           19 ┃       x <- forAll (Gen.enumBounded :: Gen Status)
              ┃       │ ConvertedToPLC
           20 ┃       tripping x Aeson.encode Aeson.eitherDecode
              ┃       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
              ┃       │ ━━━ Intermediate ━━━
              ┃       │ "\"converted-to-plc\""
              ┃       │ ━━━ - Original) (+ Roundtrip ━━━
              ┃       │ - Right ConvertedToPLC
              ┃       │ + Left "Error in $: Could not parse Status: converted-to-plc"

           This failure can be reproduced by running:
           > recheck (Size 0) (Seed 15174119819274148648 1778119947192687319) <property>
```

It failed!

If we fire up GHCi and import both our `Status` type and the `Text.Casing` module, we can see why this fails.

```
λ kebab $ show ConvertedToPLC
"converted-to-plc"

λ pascal $ kebab $ show ConvertedToPLC
"ConvertedToPlc"
```

The `pascal` function is converting the serialised `"converted-to-plc"` into
`"ConvertedToPlc"`, but this doesn't match the derived `Read` instance for that
type! For the same reason, this code would also fail on the `ConvertedToUKEIG`
and `ConvertedToUKSocietas` constructors.

To fix this we'll have to handle those edge cases manually, like this:

```haskell
deserialise :: Text -> Either Text Status
deserialise = \case
  "converted-to-plc" -> pure ConvertedToPLC
  "converted-to-ukeig" -> pure ConvertedToUKEIG
  "converted-to-uk-societas" -> pure ConvertedToUKSocietas
  s -> case readEither (pascal (unpack s)) of
    Left _ -> Left $ "Could not parse Status: " <> s
    Right status -> pure status
```

Running this again shows us that our tests pass, and we can have confidence
that our test coverage is exhaustive because we aren't relying on human
discipline to exercise all of the paths through this code.

I don't think there's any reason not to test your custom serialisation and
deserialisation code this way, and if you're new to property-based testing then
this is the easiest place to start.

[0]: https://hackage.haskell.org/package/casing-0.1.4.1/docs/Text-Casing.html
[1]: https://hackage.haskell.org/package/hspec-hedgehog-0.0.1.2/docs/Test-Hspec-Hedgehog.html
