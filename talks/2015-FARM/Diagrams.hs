{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TypeFamilies              #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Diagrams where

import Diagrams.Prelude

import           BinLayout
import           BoltzmannTrees
import qualified Data.Tree                 as T
import qualified Diagrams.TwoD.Layout.Tree as LT
import           Dyck
import           TreeBox
import           Triangulation
-- import Diagrams.TwoD.Sunburst
-- , sunburst (treeToTree t)    -- doesn't seem to work right

n = 20

treeToTree :: Tree a -> T.Tree a
treeToTree (Leaf a) = T.Node a []
treeToTree (Branch a l r) = T.Node a (map treeToTree [l,r])

treeVisuals :: _ => [[Bool]] -> Diagram b
treeVisuals hl =
  let Just t = seedGenM 22 (2*n + 1) 0 genTree
      radTree
        = LT.renderTree (const (circle 0.1 # fc black)) (~~)
        . LT.radialLayout
        . treeToTree
        $ t   -- $
  in
    vsep 2
    . map (centerX . hsep 2 . map (sized (dims2D 10 10) . centerXY))
    . zipWith (zipWith (\b d -> d # frame 2 # if b then bg lightcyan else id)) hl
    $   -- $
    [ [ treeBoxes t, drawTree t, drawDyck False t ]
    , [ triangulation t, radTree ]
    ]
