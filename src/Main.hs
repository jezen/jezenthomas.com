{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (fromStrict)
import Data.Digest.Pure.MD5 (md5)
import Data.List (groupBy, isSuffixOf, sortOn)
import Data.Monoid ((<>))
import Data.Ord (Down(..), comparing)
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (UTCTime, utctDay)
import Data.Time.Locale.Compat (defaultTimeLocale)
import Hakyll hiding (host)
import System.FilePath.Posix (takeBaseName, takeDirectory, (</>))
import Text.Pandoc.Options

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
  [ "static/**"
  , "404.html"
  , "robots.txt"
  , "doom.gif"
  ]

styleSheets :: [FilePath]
styleSheets =
  [ "css/normalize.css"
  , "css/default.css"
  , "css/header.css"
  , "css/syntax.css"
  , "css/katex.css"
  ]

config :: Configuration
config = defaultConfiguration
  { deployCommand = "yarn surge _site jezenthomas.com" }

main :: IO ()
main = hakyllWith config $ do

  compiledStylesheetPath <- preprocess $ do
    styles <- mapM readFile styleSheets
    let h = md5 $ fromStrict $ pack $ compressCss $ mconcat styles
    pure $ "css/" <> show h <> ".css"

  let cssPathCtx = constField "cssPath" compiledStylesheetPath

  forM_ copyFiles $ \ptrn ->
    match ptrn $ do
      route idRoute
      compile copyFileCompiler

  match "css/*" $ route idRoute >> compile compressCssCompiler

  create [fromFilePath compiledStylesheetPath] $ do
    route idRoute
    compile $ do
      styles <- mapM (load . fromFilePath) styleSheets
      let ctx = listField "styles" pageCtx (pure styles)
      makeItem ""
        >>= loadAndApplyTemplate "templates/empty.html" ctx

  let postCtx = postSlugField "slug" <> pageCtx <> cssPathCtx

  match "posts/*/*" $ do
    route postCleanRoute
    dep <- makePatternDependency "css/*"
    rulesExtraDependencies [dep] $ compile $ blogCompiler
      >>= loadAndApplyTemplate "templates/post-content.html" postCtx
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/post.html" postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= cleanIndexUrls

  match "index.html" $ do
    route idRoute
    dep <- makePatternDependency "css/*"
    rulesExtraDependencies [dep] $ compile $ do
      let recentPostList :: Int -> Compiler String
          recentPostList n = do
            posts   <- loadAllSnapshots "posts/*/*" "content" >>= recentFirst
            itemTpl <- loadBody "templates/post-index.html"
            applyTemplateList itemTpl postCtx (take n posts)
      let ctx =  constField "title" "Jezen Thomas | Haskell, Unix, Minimalism, and Entrepreneurship."
              <> postCtx
              <> cssPathCtx

      getResourceBody
        >>= applyAsTemplate (field "posts" (const (recentPostList 3)))
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= cleanIndexUrls

  create ["about/index.html"] $ do
    route idRoute
    dep <- makePatternDependency "css/*"
    rulesExtraDependencies [dep] $ compile $ do
      let ctx =  constField "title" "About | Jezen Thomas"
              <> cssPathCtx
              <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/about.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= cleanIndexUrls

  create ["posts/index.html"] $ do
    route idRoute
    dep <- makePatternDependency "css/*"
    rulesExtraDependencies [dep] $ compile $ do
      posts <- recentFirst =<< loadAll "posts/*/*"
      let ctx =  constField "title" "All Posts | Jezen Thomas"
              <> publishedGroupField "years" posts postCtx
              <> cssPathCtx
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

blogCompiler :: Compiler (Item String)
blogCompiler = pandocCompilerWith readerOpts writerOpts
  where
  readerOpts :: ReaderOptions
  readerOpts = defaultHakyllReaderOptions
    { readerExtensions =
        (readerExtensions defaultHakyllReaderOptions) <> extensionsFromList
           [ Ext_tex_math_single_backslash  -- TeX math btw (..) [..]
           , Ext_tex_math_double_backslash  -- TeX math btw \(..\) \[..\]
           , Ext_tex_math_dollars           -- TeX math between $..$ or $$..$$
           , Ext_latex_macros               -- Parse LaTeX macro definitions (for math only)
           ]
      }

  writerOpts :: WriterOptions
  writerOpts = defaultHakyllWriterOptions
    { writerHTMLMathMethod = MathJax ""
    }

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
