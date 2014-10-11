{-# LANGUAGE CPP                       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Warning where

#ifdef USE_SVG
import           Diagrams.Backend.SVG.CmdLine
#else
import           Diagrams.Backend.Cairo.CmdLine
#endif
import           Diagrams.Prelude

signSide = hrule 1 <> arc (rotateBy (-1/4) xDir) (1/3 @@ turn) # scale 0.3

sign = mconcat . take 3 . iterate (rotateBy (1/3)) $ signSide

d :: Diagram B V2 Double
d = text "!" # fc black # bold
 <> strokeLoop (glueLine sign) # lc black # lw (Output 3) # center


main = defaultMain $ pad 1.15 d
