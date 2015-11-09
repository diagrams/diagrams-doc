{-# LANGUAGE CPP                       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Warning where

#ifdef USE_SVG
import           Diagrams.Backend.SVG.CmdLine
#else
import           Diagrams.Backend.Rasterific.CmdLine
#endif
import           Diagrams.Prelude

signSide = hrule 1 <> arc (rotateBy (-1/4) xDir) (1/3 @@ turn) # scale 0.3

sign = mconcat . take 3 . iterate (rotateBy (1/3)) $ signSide

trapazoid :: (InSpace V2 n t, TrailLike t, OrderedField n)
  => n -> n -> n -> t
trapazoid h wt wb = trailLike $ ss `at` p2 (-wb/2, -h/2)
  where
    ss = closeTrail . trailFromSegments $ map (straight . r2)
          [ (wb,0)
          , ((wt-wb)/2,h)
          , (-wt,0)
          ]

bang :: Diagram B
bang = center $ vcat' (with & sep .~ 0.4) [trapazoid 3 1 0.8, circle 0.55]

d :: Diagram B
d = (bang # fc black # bold # pad 1.4 # sizedAs l) <> l
  where
    l :: Diagram B
    l = strokeLoop (glueLine sign) # lc black # lw (output 3) # center


main = defaultMain $ pad 1.15 d
