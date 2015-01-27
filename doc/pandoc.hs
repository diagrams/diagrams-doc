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
-- Since Links uses the ghc package, this needs to compiled with
-- @-package ghc@.
--
-----------------------------------------------------------------------------
import           Control.Applicative
import           Control.Lens
import           Control.Monad.IO.Class
import qualified Data.DList             as D
import           Data.List              (intersect)
import           Data.Semigroup
import           Data.Maybe
import           Diagrams.Builder
import           Diagrams.Pandoc
import           Text.Pandoc.JSON
import           Text.Pandoc.Walk
import           System.IO.Unsafe
import qualified Data.Map as M

import Links

------------------------------------------------------------------------
-- Pandoc filter
------------------------------------------------------------------------

main :: IO ()
main = toJSONFilter docFilter

docFilter :: Maybe Format -> Pandoc -> IO Pandoc
docFilter mf = fmap (walk linkInline) . pFilter mf
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

------------------------------------------------------------------------
-- Linking to Hackage
------------------------------------------------------------------------

-- Current issues:
--
-- * d i a g ... are getting exported from Diagrams.Example.Logo
-- * name' is not escaping the ' properly
-- * things from base aren't getting a package in hackage link
-- * only single worded code is linked

-- Pandoc stuff

-- annoyingly pandoc doesn't support links in code so for now only
-- support linking single terms (we'll need to implement separate ones
-- for each supported format)
linkInline :: Inline -> Inline
linkInline (Code (_id,classes,_keys) code)
  | length (words code) > 1 = codeI
  | "hs"  `elem` classes = mkLink nameLink
  | "mod" `elem` classes = mkLink modLink
  | "pkg" `elem` classes = mkLink pkgLink
  | otherwise            = mkLink guess -- experiment
  where
    codeI   = Code nullAttr code
    mkLink  = maybe codeI (Link [codeI])

    nameLink = nameLookup code
    modLink  = moduleLookup code
    pkgLink  = Just (hackage ++ code, code)
    guess = nameLink
        <|> modLink
        <|> if code `elem` (diagramsPackages ++ otherPackages)
               then pkgLink
               else Nothing
linkInline i = i

-- | Packages that will be on the site.
diagramsPackages :: [String]
diagramsPackages =
  [ "linear" -- linear should be first so overlaps gets replaced
  , "diagrams-core", "diagrams-lib"
  , "SVGFonts", "palette", "diagrams-contrib"
  , "diagrams-pgf", "diagrams-svg", "diagrams-rasterific"
  ]

-- | Package whose modules we'll link to hackage for.
otherPackages :: [String]
otherPackages =
  [ "base", "lens" ]

-- Helpers stuff

-- this just makes things easier

moduleMap :: ModuleMap
nameMap :: NameMap
(moduleMap, nameMap) = unsafePerformIO getMaps

nameLookup, moduleLookup :: String -> Maybe Target
nameLookup n = diagramsName <$> M.lookup n nameMap
moduleLookup m = diagramsModule <$> M.lookup m moduleMap

getMaps :: IO (ModuleMap, NameMap)
getMaps = do
  (diaMods, diaNames) <- buildPackageMaps diagramsPackages

  -- We need other modules so we can link to things like @Control.Lens@
  --
  -- We don't need the names because (arguably) we shouldn't use any
  -- names not exported by a diagrams module (which shouldn't have any
  -- conflicts).
  (otherMods, _) <- buildPackageMaps otherPackages

  return (diaMods <> otherMods, diaNames)

-- | Make a link to the diagrams site if it's a diagrams module,
--   otherwise link to hackage. Includes hyperlink to the name.
diagramsName :: Name -> Target
diagramsName n = (link, showName n)
  where
    link = fst (diagramsModule (nameModule n)) ++ nameAnchor n

-- | Make a link to the diagrams site if it's a diagrams module,
--   otherwise link to hackage.
diagramsModule :: Module -> Target
diagramsModule m = (link, showModule m)
  where
    link
      | isDiagramMod = diagramsLink
      | otherwise    = hackageModule m
    isDiagramMod = showModulePkg m `elem` diagramsPackages
    diagramsLink =
      "http://projects.haskell.org/diagrams/haddock/" ++ showModuleD m

