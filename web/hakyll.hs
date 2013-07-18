{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

import           Control.Monad          (forM_, (>=>))
import           Data.Char              (isAlphaNum)
import           Data.List              (sortBy)
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
      compile (readItemCompiler >>>
               addDefaultFields >>>
               arr (setField "title" "User manual") >>>
               mainCompiler)

    match (predicate (\i -> matches "manual/**" i
                         && not (matches "manual/*.html" i)
                         && not (matches "manual/manual" i)
                         -- above needed due to some sort of weird bug in shake?? =(
                     )
          )
      $ do
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
        compile $ pandocCompiler >>= mainCompiler

    -- Example gallery ----------------------------

      -- make all gallery/*.lhs available for building gallery.html
    match "gallery/*.lhs" $ compile (pandocCompiler >>^ (setHtmlURL . setImgURL))

      -- build gallery.html from gallery.markdown and gallery/*.lhs
      -- note the inGroup Nothing, which ensures we don't get
      -- gallery/*.lhs resources from other groups, since we are using
      -- gallery/*.lhs to build several different things
    match "gallery.markdown" $ do
        route $ setExtension "html"

        compile $ pandocCompiler
            >>> requireAllA ("gallery/*.lhs" `mappend` inGroup Nothing) buildGallery
            >>> mainCompiler

      -- generate .png from .lhs
    match "gallery/*.lhs" $ version "png" $ do
        route $ setExtension "png"
        compile $ unsafeCompiler (compilePng False)

    match "gallery/*.lhs" $ version "png-thumb" $ do
        route $ gsubRoute ".lhs" (const "-thumb.png")
        compile $ unsafeCompiler (compilePng True)

      -- build syntax-highlighted source code for examples
    match "gallery/*.lhs" $ version "gallery" $ do
        route $ setExtension "html"
        compile $ withMathJax
            >>> arr setImgURL
            >>> arr (pandocFields ["description"])
            >>> applyTemplateCompiler "templates/exampleHi.html"
            >>> mainCompiler

      -- export raw .lhs of examples for download, stripping off the
      -- metadata first
    group "raw" $ forM_ lhs $ flip match $ do
        route idRoute
        compile (readItemCompiler >>^ pageBody)

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

compilePng :: Bool -> Resource -> IO LB.ByteString
compilePng isThumb resource = do
    let thumbFlag | isThumb   = "--thumb 175 "
                  | otherwise = ""
    _ <- system $ "cd gallery && ../../.make/web/gallery/Build.hs.exe " ++ thumbFlag ++ moduleName ++ " " ++ tmpPath
    LB.readFile tmpPath
  where
    moduleName = takeBaseName $ unResource resource
    tmpPath    = "/tmp/" ++ moduleName ++ (if isThumb then "-thumb" else "") ++ ".png"

mainCompiler :: Item String -> Compiler (Item String)
mainCompiler = loadAndApplyTemplate "templates/default.html" undefined -- context
           >=> relativizeUrls

setThumbURL, setImgURL, setHtmlURL :: Item a -> Item a
setThumbURL  = setURL "-thumb.png"
setImgURL    = setURL "png"
setHtmlURL   = setURL "html"

setURL :: String -> Item a -> Item a
setURL ext p = trySetField (extNm ++ "url") fieldVal p
  where extNm = filter isAlphaNum ext
        fieldVal = dropExtension (getField "url" p) ++ ext'
        ext' | '.' `elem` ext = ext
             | otherwise      = '.' : ext

pandocFields :: [String] -> Item String -> Item String
pandocFields = foldr (.) id . map pandocField

pandocField :: String -> Item String -> Item String
pandocField f p = setField f newField p
  where newField = writePandoc . readPandoc Markdown Nothing $ getField f p


buildGallery :: Compiler (Item String, [Item String]) (Item String)
buildGallery = second (mapCompiler compileExample >>> sortDate >>> arr (map pageBody))
               >>> arr (\(bod, exs) -> modBody (++ (concat exs)) bod)
  where sortDate = arr (sortBy $ flip (comparing (getField "date")))

compileExample :: Compiler (Item String) (Item String)
compileExample = (setHtmlURL . setThumbURL) ^>> applyTemplateCompiler "templates/example.html"

modBody :: (a -> b) -> Item a -> Item b
modBody f p = p { pageBody = f (pageBody p) }

pages, md, lhs :: IsString s => [s]

pages = md ++ lhs

md = map (fromString . (++".markdown"))
  [ "index"
  , "download"
  , "documentation"
  , "community"
  , "releases"
  ]

lhs = [ "tutorial/DiagramsTutorial.lhs", "gallery/*.lhs" ]
