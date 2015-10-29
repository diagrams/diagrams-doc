{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

import           BoltzmannTrees
import           Data.Colour.Palette.BrewerSet
import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude

colors = brewerSet YlOrRd 9

boxes :: Tree () -> Diagram B
boxes = boxes' 0

boxes' :: Int -> Tree () -> Diagram B
boxes' n (Leaf _)       = square 1 # fc (colors !! (n `mod` 9))
boxes' n (Branch _ l r) =
  mconcat
    [ children
    , boundingRect (children # frame 1)
    ]
    # centerXY
    # rotate (10 @@ deg)
    # fc (colors !! (n `mod` 9))
    # lw none
  where
    children = (if even n then hsep else vsep)
      1 [ boxes' (n+1) l, boxes' (n+1) r ]

main :: IO ()
main = do
  Just t <- runGenM 30 0.15 genTree
  mainWith (boxes t # frame 1)
