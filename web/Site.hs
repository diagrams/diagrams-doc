{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE FlexibleContexts          #-}

module Site where

import           Control.Monad   (forM_, (>=>))
import           Data.List       (isPrefixOf, sortBy)
import           Data.Maybe      (fromMaybe)
import           Data.Monoid     ((<>))
import           Data.Ord        (comparing)
import           Data.Text       (empty, pack, replace, unpack)
import           Data.String     (IsString, fromString)

import           System.FilePath ((</>), splitFileName, replaceExtension)

import           Text.Pandoc

import           Hakyll

pages :: IsString s => [s]
pages = map (fromString . (++".markdown"))
  [ "download"
  , "tutorials"
  , "reference"
  , "sightings"
  , "community"
  , "releases"
  ]

imgExt :: String
imgExt = "png"

main :: IO ()
main = do

  hakyll $ do
    -- Build Tags
    tags <- buildTags "blog/*" (fromCapture "tags/*.html")

    -- CSS, templates, JavaScript -----------------
    match "css/*" $ do
        route   idRoute
        let cssCtx = constField "ext" imgExt
        compile (compressCssCompiler >>= applyAsTemplate cssCtx)

    match "templates/*" $ compile templateCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- User manual --------------------------------
    match "doc/*.html" $ do
        route idRoute
        compile (getResourceBody >>= mainCompiler defaultContext)

    match ("doc/**" .&&. complement "doc/*.html") $ do
        route idRoute
        compile copyFileCompiler

    -- Index --------------------------------------

    match "news/*" $ do
      compile pandocCompiler

    create ["index.html"] $ do
      route idRoute
      compile $ do
        news <- fmap (take 5) . recentFirst =<< loadAll "news/*"
        let indexCtx =
              constField "title" "About Diagrams" <>
              listField "news" defaultContext (return news) <>
              defaultContext
        empty <- makeItem ""
        tpl <- loadBody "templates/index.markdown"
        b <- applyTemplate tpl indexCtx empty

        let b' = renderMarkdownPandocWith
                   defaultHakyllReaderOptions
                   defaultHakyllWriterOptions
                   b
        indexCompiler indexCtx b'

    -- Blog ---------------------------------------
    match "blog/*.html" $ do
        route idRoute
        compile (getResourceBody >>= blogCompiler (tagsCtx tags) >>= mainCompiler (tagsCtx tags))

    match "blog/images/*" $ do
        route idRoute
        compile copyFileCompiler

    create ["posts.html"] $ do
        route idRoute
        compile $ do
            posts <- loadAll "blog/*"
            sorted <- recentFirst posts
            itemTpl <- loadBody "templates/postitem.html"
            list <- applyTemplateList itemTpl postCtx sorted
            makeItem list
                >>= loadAndApplyTemplate "templates/posts.html" allPostsCtx
                >>= applyDefaultTemplate allPostsCtx
                >>= relativizeUrls

    create ["blog.html"] $ do
        route $ constRoute "blog/index.html"
        compile $ do
            posts <- loadAll "blog/*"
            sorted <- take 10 <$> recentFirst posts
            itemTpl <- loadBody "templates/postitem.html"
            list <- applyTemplateList itemTpl postCtx sorted
            makeItem list
                >>= loadAndApplyTemplate "templates/blog.html" (blogCtx tags list)
                >>= applyDefaultTemplate (blogCtx tags list)
                >>= relativizeUrls

    -- Post tags
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged :" ++ tag
        route idRoute
        compile $ do
            list <- postList tags pattern recentFirst
            makeItem ""
                >>= loadAndApplyTemplate "templates/blog.html"
                  ( constField "body" list `mappend` (blogCtx tags list))
                >>= applyDefaultTemplate
                  ( constField "title" title `mappend` (blogCtx tags list))
                >>= relativizeUrls

    -- Render RSS feed
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            loadAllSnapshots "blog/*" "content"
                >>= mapM (externalizeUrls (feedRoot feedConfiguration) "blog")
                >>= recentFirst
                >>= renderRss feedConfiguration feedCtx

    -- API documentation --------------------------

    match "haddock/**" $ do
        route idRoute
        compile copyFileCompiler

    -- Static images ------------------------------

    match "*.ico" $ do
        route idRoute
        compile copyFileCompiler

    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    -- Normal .html pages, built from .markdown ---
    forM_ pages $ flip match $ do
        route   $ setExtension "html"
        compile $ pandocCompiler >>= mainCompiler defaultContext

    -- Example gallery ----------------------------

    match "gallery/images/*.png" $ do
        route idRoute
        compile copyFileCompiler

    match "gallery/images/*.svg" $ do
        route idRoute
        compile copyFileCompiler

    match "banner/images/*.png"  $ do
        route idRoute
        compile copyFileCompiler

    match "banner/images/*.svg" $ do
        route idRoute
        compile copyFileCompiler

    match "gallery.markdown" $ do
        route $ setExtension "html"

        compile $ do
          galleryContent <- pandocCompiler
          lhss <- loadAll ("gallery/*.lhs" .&&. hasVersion "gallery")
          gallery <- buildGallery galleryContent lhss
          mainCompiler defaultContext gallery

      -- build syntax-highlighted source code for examples
    match "gallery/*.lhs" $ version "gallery" $ do
        route $ setExtension "html"
        compile $ getResourceBody
            >>= loadAndApplyTemplate "templates/wrapExample.lhs" defaultContext
            >>= withMathJax
            >>= loadAndApplyTemplate "templates/exampleHi.html"
                  ( mconcat
                    [ setImgURL
                    , setHtmlURL
                    , markdownFieldsCtx ["description"]
                    , defaultContext
                    ]
                  )
            >>= mainCompiler defaultContext

      -- export raw .lhs of examples for download
    match "gallery/*.lhs" $ version "raw" $ do
        route idRoute
        compile $ getResourceBody
            >>= loadAndApplyTemplate "templates/wrapExample.lhs" defaultContext

    match "banner/template.css" $ do
        compile $ getResourceBody

    match "banner/banner.hs" $ version "template" $ do
        compile $ do
            b <- getResourceBody
            c <- buildBannerCSS b
            h <- buildBannerHtml b
                >>= loadAndApplyTemplate "templates/banner.html" defaultContext
            let ctx =  constField "bannerCSS"  (itemBody c)
                    <> constField "template" "$body$"
                    <> constField "navbarStyle" "navbar-inverse"
                    <> constField "title" "$title$"
                    <> defaultContext
            (readTemplate <$>) <$> loadAndApplyTemplate "templates/default.html" ctx
                                      (escapeForTemplate <$> h)
                                      -- The .hs source may contain $'s which
                                      -- will throw off templating, escape them
                                      -- before this becomes a template.

escapeForTemplate :: String -> String
escapeForTemplate = concatMap f
  where
    f '$' = "$$"
    f x   = [x]

buildBannerCSS :: Item String -> Compiler (Item String)
buildBannerCSS b = do
    t <- loadBody (fromFilePath "banner/template.css")
    m <- loadAndApplyTemplate "templates/banner.markdown" defaultContext b
    return $ renderMarkdownPandocWith
               defaultHakyllReaderOptions
               defaultHakyllWriterOptions
                 { writerStandalone = True
                 , writerTemplate = t
                 }
               m

buildBannerHtml :: Item String -> Compiler (Item String)
buildBannerHtml b = do
    b <- getResourceBody
    m <- loadAndApplyTemplate "templates/banner.markdown" defaultContext b
    return $ renderMarkdownPandoc m

renderMarkdownPandocWith :: ReaderOptions -> WriterOptions -> Item String -> Item String
renderMarkdownPandocWith ropt wopt = writePandocWith wopt . fmap (either (const mempty) id . readMarkdown ropt)

renderMarkdownPandoc :: Item String -> Item String
renderMarkdownPandoc = renderMarkdownPandocWith
                         defaultHakyllReaderOptions
                         defaultHakyllWriterOptions

withMathJax :: Item String -> Compiler (Item String)
withMathJax = fmap (writePandoc . fmap (bottomUp latexToMathJax)) . readPandoc
  where latexToMathJax (Math InlineMath str)
          = RawInline "html" ("\\(" ++ str ++ "\\)")
        latexToMathJax (Math DisplayMath str)
          = RawInline "html" ("\\[" ++ str ++ "\\]")
        latexToMathJax x = x

indexCompiler :: Context String -> Item String -> Compiler (Item String)
indexCompiler ctx = loadAndApplyTemplate
                      (setVersion (Just "template") $ fromFilePath "banner/banner.hs") ctx
               >=> relativizeUrls

applyDefaultTemplate :: Context String -> Item String -> Compiler (Item String)
applyDefaultTemplate ctx s = do
     t <- readTemplate . itemBody <$> loadAndApplyTemplate "templates/default.html" ctx' e
     applyTemplate t ctx s
  where
    ctx' =  constField "bannerCSS"  ""
         <> constField "template" "$body$"
         <> constField "navbarStyle" "navbar-default"
         <> defaultContext
    e = const "" <$> s

mainCompiler :: Context String -> Item String -> Compiler (Item String)
mainCompiler ctx = applyDefaultTemplate ctx >=> relativizeUrls

blogCompiler :: Context String -> Item String -> Compiler (Item String)
blogCompiler ctx = loadAndApplyTemplate "templates/post.html" ctx
               >=> saveSnapshot "content"

setThumbURL, setImgURL, setHtmlURL :: Context String
setThumbURL  = setURL "images" ("thumb." ++ imgExt)
setImgURL    = setURL "images" ("big." ++ imgExt)
setHtmlURL   = setURL "" "html"

setURL :: FilePath -> String -> Context String
setURL dir ext = field (extNm ++ "url") fieldVal
  where extNm = takeWhile (/= '.') ext
        fieldVal i = do
          u <- fmap (maybe "" toUrl) . getRoute . itemIdentifier $ i
          let (path,f) = splitFileName u
          return (path </> dir </> replaceExtension f ext)

-- | Take the content of the specified fields and make them available
--   after typesetting them as Markdown via pandoc.
markdownFieldsCtx :: [String] -> Context String
markdownFieldsCtx = mconcat . map markdownFieldCtx

markdownFieldCtx :: String -> Context String
markdownFieldCtx f = field f $ \i -> do
  markdown <- fromMaybe "" <$> getMetadataField (itemIdentifier i) f
  return $ case readMarkdown defaultHakyllReaderOptions markdown of
    Right p -> writeHtmlString defaultHakyllWriterOptions p
    Left e  -> show e

buildGallery :: Item String -> [Item String] -> Compiler (Item String)
buildGallery content lhss = do
  -- reverse sort by date (most recent first)
  lhss' <- mapM addDate lhss
  let exs = reverse . map snd . sortBy (comparing fst) $ lhss'

      galleryCtx = mconcat
        [ listField "examples" exampleCtx (return exs)
        , defaultContext
        ]
      exampleCtx = mconcat
        [ setHtmlURL
        , setThumbURL
        , defaultContext
        ]

  loadAndApplyTemplate "templates/gallery.html" galleryCtx content

  where
    addDate lhs = do
      d <- fromMaybe "" <$> getMetadataField (itemIdentifier lhs) "date"
      return (d,lhs)

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

allPostsCtx :: Context String
allPostsCtx =
    constField "title" "All posts" `mappend`
    postCtx

blogCtx :: Tags -> String -> Context String
blogCtx tags list =
    constField "posts" list `mappend`
    constField "title" "Recent Posts" `mappend`
    field "taglist" (\_ -> renderTagList tags) `mappend`
    defaultContext

tagsCtx :: Tags -> Context String
tagsCtx tags =
    tagsField "prettytags" tags `mappend`
    postCtx

feedCtx :: Context String
feedCtx =
    bodyField "description" `mappend`
    postCtx

-- Feed configuration

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Diagrams Blog - RSS feed"
    , feedDescription = "Diagrams Blog Posts"
    , feedAuthorName  = "diagrams contributors"
    , feedAuthorEmail = "diagrams-discuss@googlegroups.com"
    , feedRoot        = "http://projects.haskell.org/diagrams"
    }

-- Auxiliary compilers

externalizeUrls :: String -> String -> Item String -> Compiler (Item String)
externalizeUrls root rel item = return $ fmap (externalizeUrlsWith root rel) item

externalizeUrlsWith :: String  -- ^ Path to the site root
                    -> String  -- ^ Relative path from site root to working dir
                    -> String  -- ^ HTML to externalize
                    -> String  -- ^ Resulting HTML
externalizeUrlsWith root rel = withUrls ext
  where
    ext x
      | isExternal x = x
      | isAbsolute x = root ++ x   -- don't use </>, it ignores root when x starts with / !
      | otherwise    = root </> rel </> x
    isAbsolute = isPrefixOf "/"

postList :: Tags -> Pattern -> ([Item String] -> Compiler [Item String])
         -> Compiler String
postList tags pattern preprocess' = do
    postItemTpl <- loadBody "templates/postitem.html"
    posts <- preprocess' =<< loadAll pattern
    applyTemplateList postItemTpl (tagsCtx tags) posts
