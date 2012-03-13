{-# LANGUAGE OverloadedStrings
           , NoMonomorphismRestriction
   #-}
import Control.Arrow ((>>>), (>>^), (^>>), arr, second)
import Control.Monad (forM_)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Char (isAlphaNum)
import Data.Monoid

import Data.String

import System.FilePath
import System.Process (system)
import qualified Data.ByteString.Lazy as LB

import Text.Pandoc.Definition
import Text.Pandoc.Generic

import Hakyll

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
      compile (readPageCompiler >>>
               addDefaultFields >>>
               arr (setField "title" "User manual") >>>
               mainCompiler)

    match (predicate (\i -> matches "manual/**" i && not (matches "manual/*.html" i)))
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
        compile $ pageCompiler >>> mainCompiler

    -- Example gallery ----------------------------

      -- make all gallery/*.lhs available for building gallery.html
    match "gallery/*.lhs" $ compile (pageCompiler >>^ (setHtmlURL . setImgURL))

      -- build gallery.html from gallery.markdown and gallery/*.lhs
      -- note the inGroup Nothing, which ensures we don't get
      -- gallery/*.lhs resources from other groups, since we are using
      -- gallery/*.lhs to build several different things
    match "gallery.markdown" $ do
        route $ setExtension "html"

        compile $ pageCompiler
            >>> requireAllA ("gallery/*.lhs" `mappend` inGroup Nothing) buildGallery
            >>> mainCompiler

      -- generate .png from .lhs
    group "png" $ match "gallery/*.lhs" $ do
        route $ setExtension "png"
        compile $ unsafeCompiler (compilePng False)

    group "png-thumb" $ match "gallery/*.lhs" $ do
        route $ gsubRoute ".lhs" (const "-thumb.png")
        compile $ unsafeCompiler (compilePng True)

      -- build syntax-highlighted source code for examples
    group "gallery" $ match "gallery/*.lhs" $ do
        route $ setExtension "html"
        compile $ pageCompilerWithMathJax
            >>> arr setImgURL
            >>> arr (pandocFields ["description"])
            >>> applyTemplateCompiler "templates/exampleHi.html"
            >>> mainCompiler

      -- export raw .lhs of examples for download, stripping off the
      -- metadata first
    group "raw" $ forM_ lhs $ flip match $ do
        route idRoute
        compile (readPageCompiler >>^ pageBody)

pageCompilerWithMathJax :: Compiler Resource (Page String)
pageCompilerWithMathJax =
  pageCompilerWithPandoc defaultHakyllParserState defaultHakyllWriterOptions
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
    _ <- system $ "cd gallery && ./Build.exe " ++ thumbFlag ++ moduleName ++ " " ++ tmpPath
    LB.readFile tmpPath
  where
    moduleName = takeBaseName $ unResource resource
    tmpPath    = "/tmp/" ++ moduleName ++ (if isThumb then "-thumb" else "") ++ ".png"

mainCompiler :: Compiler (Page String) (Page String)
mainCompiler = applyTemplateCompiler "templates/default.html"
           >>> relativizeUrlsCompiler

setThumbURL, setImgURL, setHtmlURL :: Page a -> Page a
setThumbURL  = setURL "-thumb.png"
setImgURL    = setURL "png"
setHtmlURL   = setURL "html"

setURL :: String -> Page a -> Page a
setURL ext p = trySetField (extNm ++ "url") fieldVal p
  where extNm = filter isAlphaNum ext
        fieldVal = dropExtension (getField "url" p) ++ ext'
        ext' | '.' `elem` ext = ext
             | otherwise      = '.' : ext

pandocFields :: [String] -> Page String -> Page String
pandocFields = foldr (.) id . map pandocField

pandocField :: String -> Page String -> Page String
pandocField f p = setField f newField p
  where newField = writePandoc . readPandoc Markdown Nothing $ getField f p


buildGallery :: Compiler (Page String, [Page String]) (Page String)
buildGallery = second (mapCompiler compileExample >>> sortDate >>> arr (map pageBody))
               >>> arr (\(bod, exs) -> modBody (++ (concat exs)) bod)
  where sortDate = arr (sortBy $ flip (comparing (getField "date")))

compileExample :: Compiler (Page String) (Page String)
compileExample = (setHtmlURL . setThumbURL) ^>> applyTemplateCompiler "templates/example.html"

modBody :: (a -> b) -> Page a -> Page b
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