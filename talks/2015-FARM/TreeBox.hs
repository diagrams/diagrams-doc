{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TypeFamilies              #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module TreeBox where

import           BoltzmannTrees
import           Data.Colour.Palette.BrewerSet
import           Diagrams.Prelude
import           Diagrams.TwoD.Layout.Tree

colors = brewerSet YlOrRd 9

treeBoxes :: _ => Tree () -> Diagram b
treeBoxes = treeBoxes' 0
  where
    treeBoxes' n (Leaf _) = -- pointDiagram origin # named "node"
      -- square 1 # fc black
      square 1 # lw none
    treeBoxes' n (Branch _ l r) = children
      -- # (withNameAll "node" $ \subs -> atop $ mconcat (map ((origin ~~) . location) subs))
      -- # localize
      -- # named "node"
      <> boundingRect children # centerXY # lw none # fc (colors !! (n `mod` 9))
      where
        children = (if even n then hsep else vsep) 1 [treeBoxes' (n+1) l, treeBoxes' (n+1) r] # frame 1 # centerXY

-- main :: IO ()
-- main = do
--   Just t <- runGenM 1000 0.15 genTree
--   mainWith (treeBoxes t # frame 1)
