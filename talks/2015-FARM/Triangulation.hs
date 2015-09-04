{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TypeFamilies              #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Triangulation where

import           BoltzmannTrees
import           Data.Colour.Palette.BrewerSet
import           Diagrams.Prelude

treeSize :: Tree a -> Int
treeSize (Leaf _) = 0
treeSize (Branch _ l r) = 1 + treeSize l + treeSize r

colors = brewerSet YlOrRd 9

triangulation :: _ => Tree' -> Diagram b
triangulation t = mconcat
  [ triangDia
  , head ps ~~ last ps
  , drawTree labelledT
  ]
  # lc grey
  where
    (triangDia, labelledT) = triangulation' ps t
    ps :: [P2 Double]
    ps = regPoly (treeSize t + 2) 1
    -- invariant: length pts == treeSize t + 2
    triangulation' :: _ => [P2 Double] -> Tree' -> (Diagram b, Tree (P2 Double))
    triangulation' [p,q]   (Leaf _)   = (p ~~ q, Leaf (lerp 0.5 p q))
    triangulation' pts (Branch _ l r) =
      ( mconcat
          [ head pts ~~ mid
          , last pts ~~ mid
          , diags1
          , diags2
          ]
      , Branch (centroid [head pts, last pts, mid]) l' r'
      )
      where
        (pts1, mid:pts2) = splitAt (treeSize l + 1) pts
        (diags1, l') = triangulation' (pts1++[mid]) l
        (diags2, r') = triangulation' (mid:pts2) r
    drawTree :: _ => Tree (P2 Double) -> Diagram b
    drawTree (Leaf _) = mempty
    drawTree (Branch p l r) = drawTree' 0 p l <> drawTree' 0 p r
    drawTree' :: _ => Int -> P2 Double -> Tree (P2 Double) -> Diagram b
    drawTree' n parent (Leaf p) = drawEdge n parent (p .+^ (p .-. parent))
    drawTree' n parent (Branch p l r) = drawEdge n parent p <> drawTree' (n+1) p l <> drawTree' (n+1) p r
    drawEdge n p q = circle 0.1 # fc c # lw none # moveTo q <> (p ~~ q) # lc c
      where c = colors !! (n `mod` 9)

-- main :: IO ()
-- main = do
--   Just t <- runGenM 200 0.1 genTree
--   mainWith (triangulation t # frame 1)
