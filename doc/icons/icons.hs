{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Icons where

import Diagrams.Prelude
import Diagrams.TwoD.Text

import Diagrams.Backend.PGF
import Diagrams.Backend.SVG (SVG, Options(..))
import Diagrams.Backend.Rasterific (Rasterific, Options(..))
import Diagrams.Backend.Build

main = do
  makeIcons savePGF
  makeIcons saveSVG
  makeIcons saveRaster

makeIcons f = do
  f "warning" warning
  f "exercises" exercises
  f "to-write" toWrite

savePGF x d    = saveDia (x <> ".pdf") (with & sizeSpec .~ dims2D 100 100) (d :: Diagram PGF)
saveSVG x d    = saveDia (x <> ".svg") (SVGOptions (dims2D 100 100) Nothing) (d :: Diagram SVG)
saveRaster x d = saveDia (x <> ".png") (RasterificOptions $ dims2D 100 100) (d :: Diagram Rasterific)

type R2Backend b n = (Renderable (Path V2 n) b, TypeableFloat n, Renderable (Text n) b)

------------------------------------------------------------------------
-- Warning
------------------------------------------------------------------------

signSide :: RealFloat n => Trail' Line V2 n
signSide = hrule 1 <> arc (rotateBy (-1/4) xDir) (1/3 @@ turn) # scale 0.3

sign :: RealFloat n => Trail' Line V2 n
sign = mconcat . take 3 . iterate (rotateBy (1/3)) $ signSide

warning :: R2Backend b n => QDiagram b V2 n Any
warning = pad 1.2
  $ text "!" # bold
 <> strokeLoop (glueLine sign)
      # fc orange
      # lwO 8
      # center

------------------------------------------------------------------------
-- Execise
------------------------------------------------------------------------

weights hs
  = hs
  # map (\h -> roundedRect 1 h 0.2)
  # hcat' (with & sep .~ 0.3)

-- dumbbell :: R2Backend b n => n -> [n] -> QDiagram b V2 n Any
dumbbell w hs = hcat' (with & sep .~ 0.5) [cap, wts, bar, wts # reflectX, cap]
  where
    wts = weights hs
    cap = rect 0.8 1
    bar = rect w 1

exercises = dumbbell 3 [4,5] # fc black # centerXY
         <> roundedRect 15 15 3 # fc lightskyblue # lwO 8 # pad 1.2

------------------------------------------------------------------------
-- Pencil
------------------------------------------------------------------------

pencil = fc black . lw none $ hcat' (with & sep .~ 0.1 ) [tip, middle, rubber]
  where
    tip    = centerY . scaleUToY 0.6 . rotate (210 @@ deg) . strokeLoop . closeLine
           $ signSide <> hrule 1 # rotateBy (1/3)
    middle = rect 2 0.6
    rubber = roundedRect' 0.5 0.6 (with & radiusTR .~ 0.1 & radiusBR .~ 0.1)


toWrite = pencil # lwL 0.4 # center # rotate (50 @@ deg)
        <> circle 2 # fc plum # lwO 8 # pad 1.2

------------------------------------------------------------------------
-- Tick
------------------------------------------------------------------------

tick = fromOffsets [V2 0.4 (-0.4), V2 1 1]

toWrite2 = tick # lwL 0.4 # center
        <> circle 1.2 # fc green # lwO 8 # pad 1.2

------------------------------------------------------------------------
-- Paper
------------------------------------------------------------------------

page  = rect 8.5 11
      # lw (local 0.4)
      # lineJoin LineJoinRound

textLines = vcat' (with & sep .~ 1.5 ) [s,l,l,s,l,l]
          # centerXY
          # lw (local 0.5)
          # lineCap LineCapRound
  where l = hrule 6 # alignL
        s = hrule 5 # alignL

paper = page <> textLines
