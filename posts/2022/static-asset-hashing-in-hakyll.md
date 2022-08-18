---
title: Static Asset Hashing in Hakyll
date: 2022-08-18
location: Odessa, Ukraine
excerpt:
  You can do cache-busting of static assets in a Hakyll website by generating a
  hash of the file's contents and using that as the path to the compiled asset
  instead.
image: blog.jpg
tags: haskell
---

A problem I encountered while working on this website is that when I edit one
of the CSS files and publish my changes to the Internet, there's a good chance
your browser will have cached the previous CSS files that it served you, and
you won't see the new styles that I have written.

To mitigate this, I decided to hack the compilation step in Hakyll so that my
stylesheets are concatenated together, compressed, and then turned into an MD5
hash. This hash is then used as the path to the resulting file which forces
the browser to download the new file whenever I update the styles.

The first step is to list the filepaths of the CSS files that should be
compiled. The order in which rules are declared in CSS is important, so I
prefer to list these filepaths explicitly.

```haskell
-- | All CSS files which should be compiled
styleSheets :: [FilePath]
styleSheets =
  [ "css/normalize.css"
  , "css/default.css"
  , "css/syntax.css"
  ]
```

Next, we need to generate the MD5 hash for the compiled CSS file's ultimate
filepath. By leveraging Hakyll's `preprocess` function, it's possible to run
some arbitrary IO effects during the site's compilation step.

The approach here is to:

1. Read the contents of each CSS file.
2. Concatenate the CSS files into one big string with `mconcat`.
3. Compress the string with Hakyll's default [`compressCss`][0] function.
4. Pack the string into a lazy bytestring, and build an MD5 digest with the [`md5`][1] function.
5. Convert the MD5 digest back into a filepath and return it.

```haskell
main :: IO ()
main = hakyllWith config $ do

  compiledStylesheetPath <- preprocess $ do
    styles <- mapM readFile styleSheets
    let h = md5 $ fromStrict $ pack $ compressCss $ mconcat styles
    pure $ "css/" <> show h <> ".css"

  let cssPathCtx = constField "cssPath" compiledStylesheetPath

  -- …
```

The `cssPathCtx` binding is a convenience. It's handy because most pages that
Hakyll generates will want CSS applied, which means most pages will need to
know where to find the compiled stylesheet.

Continuing on in our `Rules` monad, we need to add a rule that generates the
file at the filepath we calculated in the preprocessing step. The route to this
file is already correct, so we keep it as is with `route idRoute`. In the
compilation step for this rule, we are again loading our list of CSS filepaths
and adding them to some page context. The context here is only used in a
special file that we'll use in a moment to render all of the CSS.

```haskell
main :: IO ()
main = hakyllWith config $ do

  -- …

  create [fromFilePath compiledStylesheetPath] $ do
    route idRoute
    compile $ do
      styles <- mapM (load . fromFilePath) styleSheets
      let ctx = listField "styles" pageCtx (pure styles)
      makeItem "" >>= loadAndApplyTemplate "templates/all.css" ctx

  -- …
```

The snippet above references a template which doesn't exist yet. We create that
file, and add a single line of Hakyll's templating DSL to render all of the
contents of each of the CSS files by iterating over the `styles` context we
defined.

```
$for(styles)$$body$$endfor$
```

At this point, the compiled stylesheet is being generated correctly, but the
site's outermost template layer doesn't reference it yet. Any time you load and
apply the outermost template layer — in this case called
`templates/default.html` — you'll need to pass in the `cssPathCtx` bound
earlier, likely monoidally joined with some other context for that template.

```haskell
main :: IO ()
main = hakyllWith config $ do

  -- …

  match "index.html" $ do
    route idRoute
    compile $ do
      let ctx = cssPathCtx <> someOtherCtx
      getResourceBody
        >>= applyAsTemplate (field "posts" (const (recentPostList 3)))
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= cleanIndexUrls

  -- …
```

Now that the default template knows the path to the compiled CSS file, we can update that reference.

```html
<!DOCTYPE html>
<html>
  <head>
    <title>$title$</title>
    <link rel="stylesheet" type="text/css" href="/$cssPath$" />
  </head>
  <body>
    <!-- etc … -->
```

Everything works as expected when compiling the site from scratch, but a
typical development workflow involves running Hakyll's filesystem monitor to
watch for changes and incrementally recompile the site. If you run the `watch`
command from your compiled Hakyll application binary and change one of the
stylesheets, the files referencing the compiled stylesheet don't yet know that
they _also_ need to be recompiled to reflect the new stylesheet path.

Fortunately, Hakyll provides some building blocks for declaring extra
dependencies in rules. Here's the final necessary change.

```haskell
main :: IO ()
main = hakyllWith config $ do

  -- …

  match "index.html" $ do
    route idRoute
    dep <- makePatternDependency "css/*"
    rulesExtraDependencies [dep] $ compile $ do
      let ctx = cssPathCtx <> someOtherCtx
      getResourceBody
        >>= applyAsTemplate (field "posts" (const (recentPostList 3)))
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= cleanIndexUrls

  -- …
```

Now during local development the outermost template layer will always reference
the correct compiled stylesheet.

An added benefit of this approach is the concatenation step, which results in
only a single HTTP request necessary to fetch all of the styles on page load.

This approach is working nicely on this website. If there's a neater way to do
it, please let me know.

[0]: https://hackage.haskell.org/package/hakyll-4.15.1.1/docs/Hakyll-Web-CompressCss.html#v:compressCss
[1]: https://hackage.haskell.org/package/pureMD5-2.1.4/docs/Data-Digest-Pure-MD5.html#v:md5
