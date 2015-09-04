{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TypeFamilies              #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module BinLayout where

import           BoltzmannTrees
import           Diagrams.Prelude

node = mempty -- circle 0.1 # fc black

drawTree :: _ => Tree () -> Diagram b
drawTree (Leaf _) = node
drawTree (Branch _ l r) =
  vsep 2
    [ node
    , children # centerX
    ]
  # (withNames ["L", "R"] $ \[lt,rt] ->
       atop (origin ~~ location lt <> origin ~~ location rt)
    )
  # localize
  where
    children = hsep 1
      [ drawTree l # named "L"
      , drawTree r # named "R"
      ]

-- main :: IO ()
-- main = do
--   Just t <- runGenM 1000 0.15 genTree
--   mainWith (drawTree t # frame 1)
