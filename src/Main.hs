{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Monad (forM_)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (fromStrict)
import Data.Digest.Pure.MD5 (md5)
import Data.List (groupBy, intercalate, isSuffixOf, sortOn)
import Data.Maybe
import Data.Monoid ((<>))
import Data.Ord (Down(..), comparing)
import Data.Text qualified as T
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (UTCTime, utctDay)
import Data.Time.Format (formatTime)
import Data.Time.Locale.Compat (defaultTimeLocale)
import Debug.Trace
import Hakyll hiding (host)
import Redirects
import System.FilePath.Posix (takeBaseName, takeDirectory, (</>))
import Text.Pandoc
import Text.Pandoc.Citeproc
import Text.Pandoc.Highlighting (pygments, styleToCss, zenburn)
import Text.Pandoc.Options
import Text.Pandoc.Walk (walkM)

host :: String
host = "https://jezenthomas.com"

postsPattern :: Pattern
postsPattern = "posts/*/*/*"

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
  { feedTitle = "jezenthomas.com"
  , feedDescription = "Thoughts on Haskell, Business, Unix, and more."
  , feedAuthorName = "Jezen Thomas"
  , feedAuthorEmail = "jezen@jezenthomas.com"
  , feedRoot = host
  }

copyFiles :: [Pattern]
copyFiles =
  [ "static/**"
  , "404.html"
  , "robots.txt"
  , "doom.gif"
  ]

main :: IO ()
main = hakyll $ do

  forM_ copyFiles $ \ptrn ->
    match ptrn $ do
      route idRoute
      compile copyFileCompiler

  let postCtx = postSlugField "slug" <> pageCtx

  let utcCtx = field "utcOrdinal" getItemUTCOrdinal
            <> field "utcDay" getItemUTCDay
            <> field "utcMonth" getItemUTCMonth
            <> field "utcYear" getItemUTCYear

  postsMeta <- getAllMetadata "posts/*/*/*"

  version "redirects" $ createRedirects redirects

  match "posts/*/*/*" $ do
    route postCleanRoute
    compile $ do
      ident <- getUnderlying
      blogCompiler
        >>= loadAndApplyTemplate "templates/post-content.html" postCtx
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/post.html" (postCtx <> utcCtx)
        >>= loadAndApplyTemplate "templates/default.html" (postCtx <> boolField "page-blog" (const True))
        >>= cleanIndexUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      let ctx =  constField "title" "Jezen Thomas | Haskell, Unix, Minimalism, and Entrepreneurship."
              <> postCtx
              <> boolField "page-home" (const True)

      getResourceBody
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= cleanIndexUrls

  create ["about/index.html"] $ do
    route idRoute
    compile $ makeItem $ Redirect "/"

  create ["posts/index.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*/*/*"
      let ctx =  constField "title" "All Posts | Jezen Thomas"
              <> boolField "page-blog" (const True)
              <> publishedGroupField "years" posts (postCtx <> utcCtx)
              <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/posts.html" ctx
        >>= loadAndApplyTemplate "templates/post-content.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= cleanIndexUrls

  create ["sitemap.xml"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*/*/*"

      let allPosts = return posts
      let sitemapCtx = mconcat
                       [ listField "entries" pageCtx allPosts
                       , constField "host" host
                       , defaultContext
                       ]

      makeItem ""
        >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
        >>= cleanIndexHtmls

  create ["feed.xml"] $ do
    route   idRoute
    compile $ do
      let feedCtx = pageCtx <> bodyField "description"
      posts <- fmap (take 10) . recentFirst
        =<< loadAllSnapshots "posts/*/*/*" "content"
      renderRss myFeedConfiguration feedCtx posts >>= cleanIndexHtmls

  match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
pageCtx :: Context String
pageCtx = mconcat
  [ modificationTimeField "mtime" "%U"
  , constField "host" host
  , dateField "date" "%B %e, %Y"
  , defaultContext
  ]

blogCompiler :: Compiler (Item String)
blogCompiler =
  pandocCompilerWithTransformM
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
    pygmentsHighlight
  where
  pygmentsHighlight :: Pandoc -> Compiler Pandoc
  pygmentsHighlight = walkM \case
    CodeBlock (_, listToMaybe -> mbLang, _) (T.unpack -> body) -> do
      let lang = T.unpack (fromMaybe "text" mbLang)
      RawBlock "html" . T.pack <$> callPygs lang body
    block -> pure block
  callPygs :: String -> String -> Compiler String
  callPygs lang = unixFilter "pygmentize" [ "-l", lang, "-f", "html" ]

-- custom routes
--------------------------------------------------------------------------------
postCleanRoute :: Routes
postCleanRoute = cleanRoute
 `composeRoutes` gsubRoute "(posts|drafts)/" (const "")

cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
    createIndexRoute ident =
      takeDirectory p </> takeBaseName p </> "index.html"
        where p = toFilePath ident

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll ptrn replacement)
  where
    ptrn = "/index.html"
    replacement = const "/"

cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
  where idx = "index.html"

-- utils
--------------------------------------------------------------------------------
postSlugField :: String -> Context a
postSlugField key = field key $ return . baseName
  where baseName = takeBaseName . toFilePath . itemIdentifier

publishedGroupField ::
     String           -- name
  -> [Item String]    -- posts
  -> Context String   -- Post context
  -> Context String   -- output context
publishedGroupField name posts postContext = listField name groupCtx $ do
    traverse extractTime posts
      >>= mapM makeItem . fmap merge . groupByYear
    where
      groupCtx = field "year" (return . show . getYear . fst . itemBody)
                  <> listFieldWith "posts" postContext (return . snd . itemBody)

      merge :: [(UTCTime, Item a)] -> (UTCTime, [Item a])
      merge gs = (fst (head gs), snd <$> sortByTime gs)

      groupByYear :: [(UTCTime, Item a)] -> [[(UTCTime, Item a)]]
      groupByYear = groupBy (\(a, _) (b, _) -> getYear a == getYear b)

      sortByTime :: [(UTCTime, a)] -> [(UTCTime, a)]
      sortByTime = sortOn (Down . fst)

      getYear :: UTCTime -> Integer
      getYear time = year
        where (year, _, _) = (toGregorian . utctDay) time

      extractTime :: Item a -> Compiler (UTCTime, Item a)
      extractTime item = getItemUTC defaultTimeLocale (itemIdentifier item)
        >>= \time -> pure (time, item)

getItemUTCDay :: Item String -> Compiler String
getItemUTCDay item = do
  utc <- getItemUTC defaultTimeLocale $ itemIdentifier item
  pure $ formatTime defaultTimeLocale "%e" utc

getItemUTCMonth :: Item String -> Compiler String
getItemUTCMonth item = do
  utc <- getItemUTC defaultTimeLocale $ itemIdentifier item
  pure $ formatTime defaultTimeLocale "%B" utc

getItemUTCYear :: Item String -> Compiler String
getItemUTCYear item = do
  utc <- getItemUTC defaultTimeLocale $ itemIdentifier item
  pure $ formatTime defaultTimeLocale "%Y" utc

getItemUTCOrdinal :: Item String -> Compiler String
getItemUTCOrdinal item = do
  utc <- getItemUTC defaultTimeLocale $ itemIdentifier item
  pure $ ordinalSuffix utc
  where
  ordinalSuffix :: UTCTime -> String
  ordinalSuffix t =
    let dayOfMonth = (\(_, _, a) -> a) (toGregorian (utctDay t))
        suffix n
          | n `elem` [1,21,31] = "st"
          | n `elem` [2,22]    = "nd"
          | n `elem` [3,23]    = "rd"
          | otherwise          = "th"
    in suffix dayOfMonth
