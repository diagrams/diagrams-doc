{-# LANGUAGE OverloadedStrings
           , NoMonomorphismRestriction
   #-}
import Control.Arrow ((>>>), (>>^), (^>>), arr, second)
import Control.Monad (forM_)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Monoid

import Data.String

import System.FilePath
import System.Process (system)
import qualified Data.ByteString.Lazy as LB

import Hakyll

main :: IO ()
main = hakyll $ do
    -- CSS, templates -----------------------------
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "templates/*" $ compile templateCompiler

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

      -- export .png files
--    match "gallery/*.png" $ do
--        route idRoute
--        compile copyFileCompiler

      -- generate .png from .lhs
    group "png" $ match "gallery/*.lhs" $ do
        route $ setExtension "png"
        compile $ unsafeCompiler compilePng

      -- build syntax-highlighted source code for examples
    group "gallery" $ match "gallery/*.lhs" $ do
        route $ setExtension "html"
        compile $ pageCompiler
            >>> arr setImgURL
            >>> applyTemplateCompiler "templates/exampleHi.html"
            >>> mainCompiler

      -- export raw .lhs of examples for download, stripping off the
      -- metadata first
    group "raw" $ forM_ lhs $ flip match $ do
        route idRoute
        compile (readPageCompiler >>^ pageBody)

compilePng :: Resource -> IO LB.ByteString
compilePng resource = do
    _ <- system $ "cd gallery && ./Build " ++ moduleName ++ " " ++ tmpPath
    LB.readFile tmpPath
  where
    moduleName = takeBaseName $ unResource resource
    tmpPath    = "/tmp/" ++ moduleName ++ ".png"

mainCompiler = applyTemplateCompiler "templates/default.html"
           >>> relativizeUrlsCompiler

setImgURL    = setURL "png"
setHtmlURL   = setURL "html"
setURL ext p = trySetField (ext ++ "url") (replaceExtension (getField "url" p) ext) p

buildGallery :: Compiler (Page String, [Page String]) (Page String)
buildGallery = second (mapCompiler compileExample >>> sortDate >>> arr (map pageBody))
               >>> arr (\(main, exs) -> modBody (++ (concat exs)) main)
  where sortDate = arr (sortBy $ flip (comparing (getField "date")))

compileExample :: Compiler (Page String) (Page String)
compileExample = (setHtmlURL . setImgURL) ^>> applyTemplateCompiler "templates/example.html"

modBody :: (a -> b) -> Page a -> Page b
modBody f p = p { pageBody = f (pageBody p) }

pages = md ++ lhs

md = map (fromString . (++".markdown"))
  [ "index"
  , "download"
  , "documentation"
  , "community"
  ]

lhs = [ "tutorial/DiagramsTutorial.lhs", "gallery/*.lhs" ]