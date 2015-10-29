{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

import           BoltzmannTrees
import           Data.Colour.Palette.BrewerSet
import           Data.Maybe                          (fromJust)
import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude                    hiding (size)

grid :: Int -> Diagram B
grid n = mconcat
  [ bars
  , bars # rotateBy (1/4)
  , dashingG [0.1, 0.1] 0 . uncurry (~~) . fromJust . getCorners . boundingBox $ bars
  ]
  # alignBL
  # lw veryThin
  # lc grey
  where
    bars = hsep 1 (replicate (n+1) (vrule (fromIntegral n)))
         # centerXY

treeToPath :: Tree () -> [V2 Double]
treeToPath (Leaf _) = [unitY]
treeToPath (Branch _ l r) = unitX : treeToPath l ++ treeToPath r

drawPath :: [V2 Double] -> Diagram B
drawPath vs = mconcat $ zipWith translate (scanl (^+^) zero vs) (map arrowV vs)

upperRegion :: Int -> [V2 Double] -> Trail V2 Double
upperRegion l vs = mempty

lowerRegion :: Int -> [V2 Double] -> Trail V2 Double
lowerRegion l vs = mempty

drawDyck :: Tree () -> Diagram B
drawDyck t = mconcat
  [ drawPath vs
  , grid n
  , upperRegion n vs # strokeT # fc yellow  # alignBL
  , lowerRegion n vs # strokeT # fc skyblue # alignBL
  ]
  where
    vs = treeToPath t
    n = (size t - 1) `div` 2

n = 10

main :: IO ()
main = do
  Just t <- runGenM (2*n + 1) 0 genTree
  mainWith (drawDyck t # frame 1)
