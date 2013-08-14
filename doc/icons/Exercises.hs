{-# LANGUAGE NoMonomorphismRestriction #-}

module Exercises where

import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

weights :: [Double] -> Diagram Cairo R2
weights hs
  = hs
  # map (\h -> roundedRect 1 h 0.2)
  # hcat' with {sep = 0.3}

dumbbell :: Double -> [Double] -> Diagram Cairo R2
dumbbell w hs = hcat' with {sep = 0.5} [cap, wts, bar, wts # reflectX, cap]
  where
    wts = weights hs
    cap = rect 0.8 1
    bar = rect w 1

main :: IO ()
main = defaultMain (dumbbell 3 [4,5] # fc black # centerXY # pad 1.1)
