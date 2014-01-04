{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.TwoD.Path

import Diagrams.TwoD.Layout.Tree
import Diagrams.TwoD.Path.Turtle

import Control.Monad

import qualified Data.Colour as C

d = (stroke $
   circle 2 # alignBR # translateX (-0.5)
   <> (hcat' with { sep = 0.2 } . map (vcat' with {sep = 0.2})
        $ (replicate 2 (replicate 9 (reversePath $ circle 0.3)))) # alignBR)
    # fc red
    # lw 0

ico_d = (stroke $
        circle 2 # alignBR # translateX (-0.5)
        <> (vcat' with {sep = 0.3} $ replicate 5 (reversePath $ circle 0.5)) # alignBR)
        # fc red
        # lw 0

{-
dAngle :: Angle
dAngle = tau / 6 @@ rad

dHeight = 5
dWidth  = 1

dRad = 1.5

dPath = pathFromTrail . close $
        arc dAngle (tau - dAngle) # scale dRad
     <> fromOffsets [ (0, -(1 - cos (tau/4 - (dAngle ^. rad))) * dRad)
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

i = (circle 1 === strutY 0.5 === roundedRect 2 4 0.4)
    # lw 0.05
    # lc blue
    # fc yellow

sierpinski 1 = polygon with { polyType = PolyRegular 3 1 }
sierpinski n = s === (s ||| s) # centerX
  where s = sierpinski (n-1)

a1 = sierpinski 4
     # fc navy
     # lw 0
     # scale (1/2)

grid = verts # centerXY <> horiz # centerXY
  where verts = hcat' with {sep=0.5} $ replicate 20 (vrule 10)
        horiz = rotateBy (1/4) verts

gbkg = grid
    # lc gray
    # rotateBy (-1/20)
    # clipBy p
    # withEnvelope (p :: Path R2)
    # lw 0.05
  where p = square 5

g = (text "G" # fontSize 4 # rotateBy (-1/20)) <> gbkg


{-
r = text "r" # fontSize 5
  <> square 1 # lw 0 # scale 5
-}

r = sketchTurtle (setHeading 90 >> forward 5 >> right 90
                 >> replicateM 5 (forward 0.9 >> right 36)
                 >> forward 0.9 >> left 135 >> forward 3
                 )
  # reversePath
  # stroke' with { vertexNames = [["end"]] }
  # lw 0.3
  # lineJoin LineJoinRound
  # lineCap LineCapRound
  # lc orange
  # (withName "end" $ atop . place turtle . location)
  where
    turtle = eqTriangle 1 # scaleY 1.3 # rotate (-135 @@ deg)
             # lw 0.1

aTree = BNode () f f
  where f = BNode () (leaf ()) (leaf ())

a2 = renderTree (\_ -> circle 0.5 # fc purple) (~~) t'' # lw 0.1
  where Just t' = uniqueXLayout 1 2 aTree
        t''     = forceLayoutTree t'

m = square 5 # lw 0.05 <>
    text "m"
      # fontSize 6 # italic # font "freeserif" # fc green

vs = map (uncurry (&)) [(5,5), (3,6), (1,5), (1,4), (3,3), (5,2), (4,0), (0,0.5)]
s = (mconcat (map (\v -> translate v (dot blue)) vs) <>
    cubicSpline False vs # lw 0.20)
    # scale 0.8

dot c = circle 0.4 # fc c # lw 0

logo = (hcat' with {sep = 0.5} . map alignB $ [ d, i, a1, g, r, a2, m, s ])
       # centerXY

main = defaultMain (pad 1.1 logo)

-- main = defaultMain (pad 1.05 ico_d)
