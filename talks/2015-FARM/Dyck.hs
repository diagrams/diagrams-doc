{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Dyck where

import           BoltzmannTrees
import           Data.Colour.Palette.BrewerSet
import           Data.Maybe                    (fromJust)
import           Diagrams.Prelude hiding (size)

grid :: _ => Int -> Diagram b
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

treeToPath  :: Tree () -> [V2 Double]
treeToPath = init . treeToPath'

treeToPath' :: Tree () -> [V2 Double]
treeToPath' (Leaf _) = [unitY]
treeToPath' (Branch _ l r) = unitX : treeToPath' l ++ treeToPath' r

drawPath :: _ => Bool -> [V2 Double] -> Diagram b
drawPath heads vs
  = alignBL . mconcat
  $ zipWith translate (scanl (^+^) zero vs) (map (arrowV' opts) vs)
  where
    opts = with & (arrowHead .~ if heads then dart else noHead)

upperRegion :: Int -> [V2 Double] -> Trail V2 Double
upperRegion l vs =
  fromOffsets (fromIntegral l *^ unit_X : fromIntegral l *^ unit_Y : vs)
  # glueTrail

lowerRegion :: Int -> [V2 Double] -> Trail V2 Double
lowerRegion l vs =
  fromOffsets (fromIntegral l *^ unit_Y : fromIntegral l *^ unit_X : vs)
  # glueTrail

drawDyck :: _ => Bool -> Tree () -> Diagram b
drawDyck heads t = mconcat
  [ drawPath heads vs
  , grid n
  , upperRegion n vs # strokeT # fc yellow # alignBL
  , lowerRegion n vs # strokeT # fc skyblue # alignBL
  ]
  where
    vs = treeToPath t
    n = (size t - 1) `div` 2

-- main :: IO ()
-- main = do
--   Just t <- runGenM (2*n + 1) 0 genTree
--   mainWith (drawDyck t # frame 1)
