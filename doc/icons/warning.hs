{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

signSide = hrule 1 <> arc (-1/4 :: CircleFrac) ((-1/4) + 1/3) # scale 0.3

sign = mconcat . take 3 . iterate (rotateBy (1/3)) $ signSide

d = text "!" # fc black # bold
 <> strokeT sign # lc black # fc yellow # lw 0.1 # centerXY 
 

main = defaultMain $ pad 1.15 d