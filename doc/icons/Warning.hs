{-# LANGUAGE CPP                       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Warning where

#ifdef USE_SVG
import           Diagrams.Backend.SVG.CmdLine
#else
import           Diagrams.Backend.Cairo.CmdLine
#endif
import           Diagrams.Prelude

signSide = hrule 1 <> arc (-1/4 @@ turn) ((-1/4) + 1/3 @@ turn) # scale 0.3

sign = mconcat . take 3 . iterate (rotateBy (1/3)) $ signSide

d = text "!" # fc black # bold
 <> strokeT sign # lc black # fc yellow # lw 0.1 # centerXY


main = defaultMain $ pad 1.15 d
