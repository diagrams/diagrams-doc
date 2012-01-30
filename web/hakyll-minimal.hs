{-# LANGUAGE OverloadedStrings #-}

import Hakyll

import Control.Arrow ((>>^), (>>>))

main :: IO ()
main = hakyll $ do

    -- make parsed .lhs files available for creating the gallery
    match "gallery/*.lhs" $ compile pageCompiler

    -- Make use of the .lhs files to create the gallery
    match "gallery.markdown" $ do
        route $ setExtension "html"

        compile $ pageCompiler
            >>> requireAllA "gallery/*.lhs" buildGallery
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    -- Make raw .lhs files available for download, strip off metadata
    group "raw" $ match "gallery/*.lhs" $ do
        route idRoute
        compile (readPageCompiler >>^ pageBody)

buildGallery :: Compiler (Page String, [Page String]) (Page String)
buildGallery = undefined  -- irrelevant

{- Trying to run this generates the error message

  [ ERROR] Hakyll.Core.Compiler.getDependency: gallery/test1.lhs (raw)
    was found in the cache, but does not have the right type: expected
    [Char] but got Hakyll.Web.Page.Internal.Page [Char]

   I think I see why: the very first match associates the
   "gallery/*.lhs" identifiers with (Page String) values.  But in the
   "raw" group we are associating the same identifiers with (String)
   values.

   So the question is not why this happens, but how to best accomplish
   what I'm trying to do: take some files and *both* make them available
   for download, *and* use them as resources for building another page?
-}
