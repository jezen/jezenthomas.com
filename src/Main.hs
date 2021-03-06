{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)
import Data.List (groupBy, isSuffixOf, sortOn)
import Data.Monoid ((<>))
import Data.Ord (Down(..), comparing)
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (UTCTime, utctDay)
import Data.Time.Locale.Compat (defaultTimeLocale)
import Hakyll hiding (host)
import System.FilePath.Posix (takeBaseName, takeDirectory, (</>))

host :: String
host = "https://jezenthomas.com"

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
  [ "static/img/*"
  , "static/js/*"
  , "404.html"
  , "robots.txt"
  , "favicon.ico"
  , "loadtestertool.xml"
  ]

config :: Configuration
config = defaultConfiguration
  { deployCommand = "yarn surge _site jezenthomas.com" }

main :: IO ()
main = hakyllWith config site

site :: Rules ()
site = do
  forM_ copyFiles $ \ptrn ->
    match ptrn $ do
      route idRoute
      compile copyFileCompiler

  match "css/*" $ route idRoute >> compile compressCssCompiler

  let postCtx = mconcat [ postSlugField "slug", pageCtx ]

  match "posts/*/*" (postRules postCtx)

  create ["index.html"] $ do
    route idRoute
    compile $ do
      let indexCtx = mconcat
            [ constField "title" "Jezen Thomas | Haskell, Unix, Minimalism, and Entrepreneurship."
            , defaultContext
            ]

      makeItem ""
        >>= loadAndApplyTemplate "templates/index.html" indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= cleanIndexUrls

  create ["meet/index.html"] $ do
    route idRoute
    compile $ do
      let ctx = mconcat
            [ constField "title" "Arrange a Meeting with Jezen Thomas"
            , defaultContext
            ]

      makeItem ""
        >>= loadAndApplyTemplate "templates/meet.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= cleanIndexUrls

  create ["projects/index.html"] $ do
    route idRoute
    compile $ do
      let ctx = mconcat
            [ constField "title" "Projects | Jezen Thomas"
            , defaultContext
            ]

      makeItem ""
        >>= loadAndApplyTemplate "templates/projects.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= cleanIndexUrls

  create ["twitter/index.html"] $ do
    route idRoute
    compile $ do
      let ctx = mconcat
            [ constField "title" "Twitter | Jezen Thomas"
            , defaultContext
            ]

      makeItem ""
        >>= loadAndApplyTemplate "templates/twitter.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= cleanIndexUrls

  create ["posts/index.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*/*"
      let ctx =  constField "title" "All Posts | Jezen Thomas"
              <> publishedGroupField "years" posts postCtx
              <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/posts.html" ctx
        >>= loadAndApplyTemplate "templates/post-content.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= cleanIndexUrls

  create ["sitemap.xml"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*/*"

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
           posts <- fmap (take 10) . recentFirst =<<
                   loadAllSnapshots "posts/*/*" "content"
           renderRss myFeedConfiguration feedCtx posts
             >>= cleanIndexHtmls

  match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
pageCtx :: Context String
pageCtx = mconcat
  [ modificationTimeField "mtime" "%U"
  , constField "host" host
  , dateField "date" "%B %e, %Y"
  , defaultContext
  ]

postRules :: Context String -> Rules ()
postRules ctx = do
  route postCleanRoute
  compile $ pandocCompiler
    >>= loadAndApplyTemplate "templates/post-content.html" ctx
    >>= saveSnapshot "content"
    >>= loadAndApplyTemplate "templates/post.html" ctx
    >>= loadAndApplyTemplate "templates/default.html" ctx
    >>= cleanIndexUrls

-- custom routes
--------------------------------------------------------------------------------
postCleanRoute :: Routes
postCleanRoute = cleanRoute
 `composeRoutes` gsubRoute "(posts|drafts)/[0-9]{4}/" (const "")

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
