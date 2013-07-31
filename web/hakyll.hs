{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

import           Control.Monad          (forM_, (>=>))
import           Data.Char              (isAlphaNum)
import           Data.Functor           ((<$>))
import           Data.List              (sortBy)
import           Data.Maybe             (fromMaybe)
import           Data.Monoid
import           Data.Ord               (comparing)

import           Data.String

import qualified Data.ByteString.Lazy   as LB
import           System.FilePath
import           System.Process         (system)

import           Text.Pandoc.Definition
import           Text.Pandoc.Generic

import           Hakyll

main :: IO ()
main = hakyll $ do
    -- CSS, templates, JavaScript -----------------
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "templates/*" $ compile templateCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- User manual --------------------------------
    match "manual/diagrams-manual.html" $ do
      route idRoute
      let manualCtx = constField "title" "User manual" `mappend` defaultContext
      compile (getResourceBody >>= mainCompiler manualCtx)

    match ("manual/**" .&&. complement "manual/*.html" .&&. complement "manual/manual") $ do
                         -- last pattern needed due to some sort of weird bug in shake?? =(
        route idRoute
        compile copyFileCompiler

    -- API documentation --------------------------

    match "doc/**" $ do
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

      -- make all gallery/*.lhs available for building gallery.html
    match "gallery/*.lhs" $ compile pandocCompiler

      -- build gallery.html from gallery.markdown and gallery/*.lhs
      -- note the inGroup Nothing, which ensures we don't get
      -- gallery/*.lhs resources from other groups, since we are using
      -- gallery/*.lhs to build several different things
    match "gallery.markdown" $ do
        route $ setExtension "html"

        compile $ do
          galleryContent <- pandocCompiler
          lhss <- loadAll ("gallery/*.lhs" .&&. hasNoVersion)
          gallery <- buildGallery galleryContent lhss
          mainCompiler
            ( mconcat
              [ setHtmlURL
              , setImgURL
              , defaultContext
              ]
            )
            gallery

      -- generate .png from .lhs
    match "gallery/*.lhs" $ version "png" $ do
        route $ setExtension "png"
        compile $ do
          i <- getUnderlying
          unsafeCompiler (compilePng False i)

    match "gallery/*.lhs" $ version "png-thumb" $ do
        route $ gsubRoute ".lhs" (const "-thumb.png")
        compile $ do
          i <- getUnderlying
          unsafeCompiler (compilePng True i)

      -- build syntax-highlighted source code for examples
    match "gallery/*.lhs" $ version "gallery" $ do
        route $ setExtension "html"
        compile $ withMathJax
            >>= loadAndApplyTemplate "templates/exampleHi.html"
                  ( mconcat
                    [ setImgURL
--                    , pandocFieldsCtx ["description"]
                    , defaultContext
                    ]
                  )
            >>= mainCompiler defaultContext

      -- export raw .lhs of examples for download, stripping off the
      -- metadata first
    forM_ lhsPages $ \l -> match l $ version "raw" $ do
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

compilePng :: Bool -> Identifier -> IO (Item LB.ByteString)
compilePng isThumb i = do
    let thumbFlag | isThumb   = "--thumb 175 "
                  | otherwise = ""
    _ <- system $ "cd gallery && ../../.make/web/gallery/Build.hs.exe " ++ thumbFlag ++ moduleName ++ " " ++ tmpPath
    Item i <$> LB.readFile tmpPath
  where
    moduleName = takeBaseName $ toFilePath i
    tmpPath    = "/tmp/" ++ moduleName ++ (if isThumb then "-thumb" else "") ++ ".png"

mainCompiler :: Context String -> Item String -> Compiler (Item String)
mainCompiler ctx = loadAndApplyTemplate "templates/default.html" ctx
               >=> relativizeUrls

setThumbURL, setImgURL, setHtmlURL :: Context String
setThumbURL  = setURL "-thumb.png"
setImgURL    = setURL "png"
setHtmlURL   = setURL "html"

setURL :: String -> Context String
setURL ext = field (extNm ++ "url") fieldVal
  where extNm = filter isAlphaNum ext
        fieldVal i = do
          u <- getMetadataField' (itemIdentifier i) "url"
          return (dropExtension u ++ ext')
        ext' | '.' `elem` ext = ext
             | otherwise      = '.' : ext

-- pandocFieldsCtx :: [String] -> Context String
-- pandocFieldsCtx = mconcat . map pandocFieldCtx

-- pandocFieldCtx :: String -> Context String
-- pandocFieldCtx f = mapField f (writePandoc . readPandoc)

-- mapField :: String -> (Item String -> Item String) -> Context a
-- mapField k f = field k comp
--   where
--     comp i = maybe "" f <$> getMetadataField (itemIdentifier i) k

buildGallery :: Item String -> [Item String] -> Compiler (Item String)
buildGallery content lhss = do
  lhss' <- mapM addDate lhss
  exs   <- mapM compileExample . map snd . sortBy (comparing fst) $ lhss'
  return (fmap (++ (concatMap itemBody exs)) content)
  where
    addDate lhs = do
      d <- fromMaybe "" <$> getMetadataField (itemIdentifier lhs) "date"
      return (d,lhs)

compileExample :: Item String -> Compiler (Item String)
compileExample = loadAndApplyTemplate "templates/example.html"
                 ( mconcat
                   [ setHtmlURL
                   , setThumbURL
                   , defaultContext
                   ]
                 )

pages, mdPages, lhsPages :: IsString s => [s]

pages = mdPages ++ lhsPages

mdPages = map (fromString . (++".markdown"))
  [ "index"
  , "download"
  , "documentation"
  , "community"
  , "releases"
  ]

lhsPages = [ "tutorial/DiagramsTutorial.lhs", "gallery/*.lhs" ]
