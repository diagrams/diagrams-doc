{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Site where

import           Control.Monad   (forM_, (>=>))
import           Data.Functor    ((<$>))
import           Data.List       (sortBy)
import           Data.Maybe      (fromMaybe)
import           Data.Monoid
import           Data.Ord        (comparing)

import           Data.String

import           System.FilePath
import           System.Process  (readProcess)

import           Text.Pandoc

import           Hakyll

pages :: IsString s => [s]
pages = map (fromString . (++".markdown"))
  [ "index"
  , "download"
  , "documentation"
  , "community"
  , "releases"
  ]

main :: IO ()
main = do
  cairoPkg <- readProcess "ghc-pkg" ["list", "--simple-output", "diagrams-cairo"] ""
  let useSVG = null cairoPkg
      imgExt | useSVG    = "svg"
             | otherwise = "png"

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
                >>= loadAndApplyTemplate "templates/default.html" allPostsCtx
                >>= relativizeUrls

    create ["blog.html"] $ do
        route idRoute
        compile $ do
            posts <- loadAll "blog/*"
            sorted <- take 3 <$> recentFirst posts
            itemTpl <- loadBody "templates/postitem.html"
            list <- applyTemplateList itemTpl postCtx sorted
            makeItem list
                >>= loadAndApplyTemplate "templates/blog.html" (blogCtx tags list)
                >>= loadAndApplyTemplate "templates/default.html" (blogCtx tags list)
                >>= relativizeUrls

    -- Post tags
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged '" ++ tag ++ "'"
        route idRoute
        compile $ do
            list <- postList tags pattern recentFirst
            makeItem ""
                >>= loadAndApplyTemplate "templates/blog.html"
                  ( constField "body" list `mappend` (blogCtx tags list))
                >>= loadAndApplyTemplate "templates/default.html"
                  ( constField "title" title `mappend` (blogCtx tags list))
                >>= relativizeUrls

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

    match "gallery.markdown" $ do
        route $ setExtension "html"

        compile $ do
          galleryContent <- pandocCompiler
          lhss <- loadAll ("gallery/*.lhs" .&&. hasVersion "gallery")
          gallery <- buildGallery imgExt galleryContent lhss
          mainCompiler defaultContext gallery

      -- build syntax-highlighted source code for examples
    match "gallery/*.lhs" $ version "gallery" $ do
        route $ setExtension "html"
        compile $ withMathJax
            >>= loadAndApplyTemplate "templates/exampleHi.html"
                  ( mconcat
                    [ setImgURL imgExt
                    , setHtmlURL imgExt
                    , markdownFieldsCtx ["description"]
                    , defaultContext
                    ]
                  )
            >>= mainCompiler defaultContext

      -- export raw .lhs of examples for download
    match "gallery/*.lhs" $ version "raw" $ do
        route idRoute
        compile getResourceBody

withMathJax :: Compiler (Item String)
withMathJax =
  pandocCompilerWithTransform
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
    (bottomUp latexToMathJax)
  where latexToMathJax (Math InlineMath str)
          = RawInline "html" ("\\(" ++ str ++ "\\)")
        latexToMathJax (Math DisplayMath str)
          = RawInline "html" ("\\[" ++ str ++ "\\]")
        latexToMathJax x = x

mainCompiler :: Context String -> Item String -> Compiler (Item String)
mainCompiler ctx = loadAndApplyTemplate "templates/default.html" ctx
               >=> relativizeUrls

blogCompiler :: Context String -> Item String -> Compiler (Item String)
blogCompiler ctx = loadAndApplyTemplate "templates/post.html" ctx

setThumbURL, setImgURL, setHtmlURL :: String -> Context String
setThumbURL  imgExt = setURL "images" ("thumb." ++ imgExt)
setImgURL    imgExt = setURL "images" ("big." ++ imgExt)
setHtmlURL  _imgExt = setURL "" "html"

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
  return
    . writeHtmlString defaultHakyllWriterOptions
    . readMarkdown defaultHakyllReaderOptions
    $ markdown

buildGallery :: String -> Item String -> [Item String] -> Compiler (Item String)
buildGallery imgExt content lhss = do
  -- reverse sort by date (most recent first)
  lhss' <- mapM addDate lhss
  let exs = reverse . map snd . sortBy (comparing fst) $ lhss'

      galleryCtx = mconcat
        [ listField "examples" exampleCtx (return exs)
        , defaultContext
        ]
      exampleCtx = mconcat
        [ setHtmlURL imgExt
        , setThumbURL imgExt
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

postList :: Tags -> Pattern -> ([Item String] -> Compiler [Item String])
         -> Compiler String
postList tags pattern preprocess' = do
    postItemTpl <- loadBody "templates/postitem.html"
    posts <- preprocess' =<< loadAll pattern
    applyTemplateList postItemTpl (tagsCtx tags) posts