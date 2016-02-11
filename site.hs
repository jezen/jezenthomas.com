--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad (forM_, forM)
import qualified Data.Map as M
import           Hakyll
import           Data.List              (isSuffixOf, intercalate, sort)
import           System.FilePath.Posix  (takeBaseName, takeDirectory, (</>))
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A

--------------------------------------------------------------------------------
host::String
host = "http://jezenthomas.com"

myFeedConfiguration:: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle = "jezenthomas.com"
    , feedDescription = "Thoughts on vim, unix, ruby, javascript, and more."
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
    { deployCommand = "cd _site && surge . jezenthomas.com" }

main :: IO ()
main = hakyllWith config site

site :: Rules ()
site = do
  forM_ copyFiles $ \pattern->
      match pattern $ do
         route   idRoute
         compile copyFileCompiler

  match "css/*" $ do
         route   idRoute
         compile compressCssCompiler

  tags <- buildTags "posts/*/*" (fromCapture "tags/*.html")
  years <- buildYears "posts/*/*"
  let postCtx = mconcat [ tagsField "tags" tags
                        , postSlugField "slug"
                        , postYearField "year"
                        , field "allTags" (\_ -> renderTagCloud 100 100 tags)
                        , field "archiveYears" (\_ -> renderYears years)
                        , pageCtx ]

  match "posts/*/*" (postRules postCtx)
  match "drafts/*/*" (postRules postCtx)

  match "pages/*" $ do
         route   $ cleanRoute `composeRoutes` (gsubRoute "pages/" (const ""))
         compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/page.html" pageCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" pageCtx
            >>= relativizeUrls
            >>= cleanIndexUrls

  create ["index.html"] $ do
         route   idRoute
         compile $ do
           posts <- fmap (take 7) . recentFirst =<< loadAll "posts/*/*"
           let indexCtx = mconcat
                          [ listField "posts" postCtx (return posts)
                          , constField "title" "Recent posts"
                          , field "allTags" (\_ -> renderTagCloud 100 100 tags)
                          , field "archiveYears" (\_ -> renderYears years)
                          , defaultContext
                          ]

           makeItem ""
            >>= loadAndApplyTemplate "templates/index.html" indexCtx
            >>= loadAndApplyTemplate "templates/default.html" indexCtx
            >>= relativizeUrls
            >>= cleanIndexUrls

  forM_ years $ \(year, _)->
      create [yearId year] $ do
         route   idRoute
         compile $ do
           posts <- recentFirst =<< loadAll (fromGlob $ "posts/" ++ year ++"/*")
           let postsCtx = mconcat
                          [ listField "posts" postCtx (return posts)
                          , constField "title" ("Posts published in " ++ year)
                          , field "allTags" (\_ -> renderTagCloud 100 100 tags)
                          , field "archiveYears" (\_ -> renderYears years)
                          , defaultContext
                          ]
           makeItem ""
            >>= loadAndApplyTemplate "templates/posts.html" postsCtx
            >>= loadAndApplyTemplate "templates/default.html" postsCtx
            >>= relativizeUrls
            >>= cleanIndexUrls

  tagsRules tags $ \tag pattern -> do
         -- Copied from posts, need to refactor
         route cleanRoute
         compile $ do
           posts <- recentFirst =<< loadAll pattern
           let postsCtx = mconcat
                          [ listField "posts" postCtx (return posts)
                          , constField "title" ("Posts tagged: " ++ tag)
                          , field "allTags" (\_ -> renderTagCloud 100 100 tags)
                          , field "archiveYears" (\_ -> renderYears years)
                          , defaultContext
                          ]

           makeItem ""
            >>= loadAndApplyTemplate "templates/posts.html" postsCtx
            >>= loadAndApplyTemplate "templates/default.html" postsCtx
            >>= relativizeUrls
            >>= cleanIndexUrls

  create ["sitemap.xml"] $ do
         route   idRoute
         compile $ do
           posts <- recentFirst =<< loadAll "posts/*/*"
           pages <- loadAll "pages/*"

           let allPosts = (return (pages ++ posts))
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

type Year = String

buildYears :: MonadMetadata m => Pattern -> m [(Year, Int)]
buildYears pattern = do
    ids <- getMatches pattern
    return . frequency . (map getYear) $ ids
  where
    frequency xs =  M.toList (M.fromListWith (+) [(x, 1) | x <- xs])

postSlugField :: String -> Context a
postSlugField key = field key $ return . baseName
  where baseName = takeBaseName . toFilePath . itemIdentifier

postYearField :: String -> Context a
postYearField key = field key $ return . getYear . itemIdentifier

getYear :: Identifier -> Year
getYear = takeBaseName . takeDirectory . toFilePath

yearPath :: Year -> FilePath
yearPath year = "archive/" ++ year ++ "/index.html"

yearId :: Year -> Identifier
yearId = fromFilePath . yearPath

renderYears :: [(Year, Int)] -> Compiler String
renderYears years = do
  years' <- forM (reverse . sort $ years) $ \(year, count) -> do
      route' <- getRoute $ yearId year
      return (year, route', count)
  return . intercalate ", " $ map makeLink years'

  where
    makeLink (year, _, count) =
      (renderHtml (H.a ! A.href (yearUrl year) $ toHtml year)) ++
      " (" ++ show count ++ ")"
    yearUrl = toValue . toUrl . yearPath
