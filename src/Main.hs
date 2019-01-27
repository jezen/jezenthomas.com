{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad                   (forM, forM_)
import           Data.List                       (intercalate, isSuffixOf, sort)
import qualified Data.Map                        as M
import           Data.Monoid                     ((<>))
import           Hakyll                          hiding (host)
import           System.FilePath.Posix           (takeBaseName, takeDirectory,
                                                  (</>))
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A

host :: String
host = "http://jezenthomas.com"

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle = "jezenthomas.com"
    , feedDescription = "Thoughts on Haskell, Business, Unix, and more."
    , feedAuthorName = "Jezen Thomas"
    , feedAuthorEmail = "jezen@jezenthomas.com"
    , feedRoot = host
    }

copyFiles :: [Pattern]
copyFiles = [ "static/img/*"
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
  forM_ copyFiles $ \pattern ->
    match pattern $ do
      route idRoute
      compile copyFileCompiler

  match "css/*" $ route idRoute >> compile compressCssCompiler

  let postCtx = mconcat [ postSlugField "slug", pageCtx ]

  match "posts/*/*" (postRules postCtx)

  create ["index.html"] $ do
    route idRoute
    compile $ do
      posts <- fmap (take 5) . recentFirst =<< loadAll "posts/*/*"
      let indexCtx = mconcat
            [ listField "posts" postCtx (return posts)
            , constField "title" "Jezen Thomas | Haskell, Unix, Minimalism, and Entrepreneurship."
            , defaultContext
            ]

      makeItem ""
        >>= loadAndApplyTemplate "templates/index.html" indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls
        >>= cleanIndexUrls

  create ["posts/index.html"] $ do
    route idRoute
    compile $ do
      posts <- loadAll "posts/*/*" >>= recentFirst
      let ctx =  constField "title" "All Posts | Jezen Thomas"
              <> listField "posts" postCtx (return posts)
              <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/posts.html" ctx
        >>= loadAndApplyTemplate "templates/post-content.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls
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
           let feedCtx = pageCtx `mappend` bodyField "description"
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
  route   $ postCleanRoute
  compile $ pandocCompiler
    >>= loadAndApplyTemplate "templates/post-content.html" ctx
    >>= saveSnapshot "content"
    >>= loadAndApplyTemplate "templates/post.html" ctx
    >>= loadAndApplyTemplate "templates/default.html" ctx
    >>= relativizeUrls
    >>= cleanIndexUrls

-- custom routes
--------------------------------------------------------------------------------
postCleanRoute :: Routes
postCleanRoute = cleanRoute
 `composeRoutes` (gsubRoute "(posts|drafts)/[0-9]{4}/" (const ""))

cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
                            where p = toFilePath ident

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
  where
    pattern = "/index.html"
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

