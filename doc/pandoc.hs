{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) 2015 diagrams-lib team (see LICENSE)
-- License     :  GPL (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Specialised pandoc filter for converting docs into PDFs via
-- diagrams-pgf or HTMLs via diagrams-svg. This can be used as normal
-- pandoc filter but the template has to set for the result to compile.
-- A typical call would look something like:
--
-- @
-- pandoc ../tutorials/tutorials.md -o tutorials.tex --template default.latex -F ../../pandoc/filter
-- @
--
-- @
-- pandoc ../tutorials/tutorials.md -o tutorials.pdf --template default.html -F ../../pandoc/filter
-- @
--
-- Note you must be in the directory of the target file or the diagram
-- files won't be saved properly.
--
-----------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.IO.Class
import qualified Data.DList             as D
import           Data.List              (intersect)
import           Data.Maybe
import           Diagrams.Builder
import           Diagrams.Pandoc
import           Text.Pandoc.JSON
import           Text.Pandoc.Walk

------------------------------------------------------------------------
-- Pandoc filter
------------------------------------------------------------------------

main :: IO ()
main = toJSONFilter docFilter

docFilter :: Maybe Format -> Pandoc -> IO Pandoc
docFilter mf = pFilter mf
  where
    pFilter = case mf of
      Just (Format "latex") -> pandocFilter latexFilter
      Just (Format "html")  -> htmlPandoc
      _                     -> pandocFilter dFilter

-- Default filter to turn code blocks into diagrams
dFilter :: MonadIO m => Meta -> Maybe Format -> Block -> m [Block]
dFilter = backendFilter adjust defaultFilters
  where
    adjust = set diaExpr "example"
           . set hashCache (Just "diagrams")

------------------------------------------------------------------------
-- Latex
------------------------------------------------------------------------

findClass :: Attr -> Maybe String
findClass = listToMaybe . intersect ["warning", "todo", "exercises"] . view _2

-- | Expand divs into respective tex environments. (Doesn't look like
--   pandoc has another way to do this)
latexFilter :: Meta -> Maybe Format -> Block -> IO [Block]
latexFilter m mf (Div (findClass -> Just c) bs) = do
  bs' <- mapM (latexFilter m mf) bs
  return $ [RawBlock "latex" ("\\begin{" ++ c ++ "}")]
        ++ concat bs'
        ++ [RawBlock "latex" ("\\end{" ++ c ++ "}")]
latexFilter m mf b = dFilter m mf b

------------------------------------------------------------------------
-- HTML
------------------------------------------------------------------------

div_ :: String -> [String] -> [Block] -> Block
div_ i xs = Div (i,xs,[])

-- Filter

htmlPandoc :: Maybe Format -> Pandoc -> IO Pandoc
htmlPandoc mf p = do
  Pandoc m bs <- pandocFilter dFilter mf p
  let headings = getHeadings bs
  -- This is not done correctly!
  let b = div_ "wrap" []
            [ div_ "" ["col-md-3"] [htmlToc headings]
            , div_ "" ["col-md-9"] [div_ "" ["bs-docs-container"] bs]
            ]
  return $ Pandoc m [b]

-- | Make the toc (table of contents) with the sites classes.
htmlToc :: [(Int,String,[Inline])] -> Block
htmlToc hs =
  -- This is not done correctly!
  Div ( ""
      , ["bs-sidebar", "hidden-print"]
      , [("role","complementary"),("data-spy","affix")]
      ) [mkToc hs]

-- | Make a tree structured unordered list from a document's headings.
mkToc :: [(Int,String,[Inline])] -> Block
mkToc hs = BulletList (go hs)
  where
    go ((i,link,inlines):xs) = (heading : bullets i as) : go bs
      where
        (as, bs) = span ((>i) . view _1) xs
        heading  = Plain [Link inlines ('#':link,"")]
    go _ = []

    bullets i as@((i',_,_):_)
      | i' == i+1 = [BulletList (go as)]
      | i' >  i   = [BulletList [bullets (i+1) as]]
    bullets _ _   = []

getHeadings :: Walkable Block b => b -> [(Int, String, [Inline])]
getHeadings = D.toList . query qHeadings
  where
    qHeadings (Header i (ref,_,_) inlines) = D.singleton (i,ref,inlines)
    qHeadings _                            = D.empty

