{-# LANGUAGE CPP                       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Exercises where

#ifdef USE_SVG
import           Diagrams.Backend.SVG.CmdLine
#else
import           Diagrams.Backend.Cairo.CmdLine
#endif
import           Diagrams.Prelude
import           Control.Lens ((&), (.~))

weights hs
  = hs
  # map (\h -> roundedRect 1 h 0.2)
  # hcat' (with & sep .~ 0.3)

dumbbell :: Double -> [Double] -> Diagram B V2 Double
dumbbell w hs = hcat' (with & sep .~ 0.5) [cap, wts, bar, wts # reflectX, cap]
  where
    wts = weights hs
    cap = rect 0.8 1
    bar = rect w 1

main :: IO ()
main = defaultMain (dumbbell 3 [4,5] # fc black # centerXY # pad 1.1)
