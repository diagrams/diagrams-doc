{-# LANGUAGE OverloadedStrings
           , NoMonomorphismRestriction
   #-}
import Control.Arrow ((>>>), arr)
import Control.Monad (forM_)

import Data.String

import Hakyll

main :: IO ()
main = hakyll $ do
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "templates/*" $ compile templateCompiler

    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    forM_ pages $ \page ->
        match page $ do
            route   $ setExtension "html"
            compile $ pageCompiler
                >>> applyTemplateCompiler "templates/default.html"
                >>> relativizeUrlsCompiler

    group "raw" $ forM_ lhs $ \lhs ->
        match lhs $ do
            route idRoute
            compile (readPageCompiler >>> arr pageBody)


pages = md ++ lhs

md = map (fromString . (++".markdown"))
  [ "index"
  , "download"
  , "documentation"
  , "gallery"
  , "community"
  ]

lhs = [ "tutorial/DiagramsTutorial.lhs" ]