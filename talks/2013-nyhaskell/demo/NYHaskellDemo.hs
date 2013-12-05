import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

hex :: Diagram B R2
hex = regPoly 6 1 # rotateBy (1/6)
                # reflectX
                # scaleX 1.5
                # rotateBy (1/100)

circles = zipWith named [1 :: Int .. 6]
  (repeat (circle 0.2 # fc green))

dots = zipWith (<>)
  (map (fc white . scale 0.2
        . text . show) [1..6])
  circles

dia :: Diagram B R2
dia = decorateTrail hex dots
    # applyAll
      [ connectOutside' opts i j
      | i <- [1 :: Int .. 5]
      , j <- [i+1 .. 6]
      ]

opts =
  (with & headSize .~ 0.14
        & headGap .~ 0.1
        & tailGap .~ 0.1)


-- hexagon :: (TrailLike t, V t ~ R2)
--   => Double -> t

main = mainWith dia
