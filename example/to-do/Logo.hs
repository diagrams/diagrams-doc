{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.TwoD.Path

import qualified Data.Colour as C

d = (stroke $
   circlePath 2 # alignBR # translateX (-0.5)
   <> (hcat' with { sep = 0.2 } . map (vcat' with {sep = 0.2})
        $ (replicate 2 (replicate 9 (reversePath $ circlePath 0.3)))) # alignBR)
    # fc red
    # lw 0

{-
dAngle :: Rad
dAngle = tau / 6

dHeight = 5
dWidth  = 1

dRad = 1.5

dPath = pathFromTrail . close $
        arc dAngle (tau - dAngle) # scale dRad
     <> fromOffsets [ (0, -(1 - cos (tau/4 - getRad dAngle)) * dRad)
                    , (dWidth, 0)
                    , (0, dHeight)
                    , (-dWidth, 0)
                    ]

dotMatrix d path
  = mconcat [ if (isInsideWinding p path)
                 then circle (d / 2 - d/10) # fc red # lw 0 # moveTo p
                 else mempty
            | x <- [ xlo, xlo + d .. xhi ]
            , y <- [ ylo, ylo + d .. yhi ]
            , let p = P (x,y)
            ]
  where (xlo, xhi) = extentX path
        (ylo, yhi) = extentY path

matrixAng = 1/20

d = dotMatrix 0.09 (dPath # rotateBy matrixAng) # rotateBy (-matrixAng)
-}

{-
d = mconcat . map alignBR
  $ [ dBody
    , rect 0.5 6.5 # fc black
    ]

blues = iterate (C.blend 0.1 white) blue
      # take 11 # reverse

dBody = mconcat . reverse . zipWith fc blues . take 11 . iterate (rotateBy (-1/20)) . rotateBy (-1/4) $ halfC

halfC = arc 0 (1/2 :: CircleFrac)
      # scale 1.8
      # stroke
      # lw 0
-}

i = (circle 1 === strutY 0.5 === roundedRect (2,4) 0.4)
    # lw 0.05
    # lc blue
    # fc yellow

sierpinski 1 = polygon with { polyType = PolyRegular 3 1 }
sierpinski n = s === (s ||| s) # centerX
  where s = sierpinski (n-1)

a1 = sierpinski 4
     # fc black
     # scale (1/2)

grid = verts # centerXY <> horiz # centerXY
  where verts = hcat' with {sep=0.5} $ replicate 20 (vrule 10)
        horiz = rotateBy (1/4) verts

gbkg = grid
    # lc gray
    # rotateBy (-1/20)
    # clipBy p
    # withBounds (p :: Path R2)
  where p = square 5

g = (text "G" # fontSize 4 # rotateBy (-1/20)) <> gbkg

r = text "r" # fontSize 5
  <> square 1 # lw 0 # scale 5

a2 = text "a" # fontSize 5
  <> square 1 # lw 0 # scale 5

m = square 5.5 <>
    text "m"
      # fontSize 6 # italic # font "freeserif" # fc green

vs = [(5,5), (3,6), (1,5), (1,4), (3,3), (5,2), (4,0), (0,0.5)]
s = (mconcat (map (\v -> translate v (dot blue)) vs) <>
    cubicSpline False (map P vs) # lw 0.20)
    # scale 0.8

dot c = circle 0.4 # fc c # lw 0

logo = (hcat' with {sep = 0.5} . map alignB $ [ d, i, a1, g, r, a2, m, s ])
       # centerXY

main = defaultMain (pad 1.1 logo)