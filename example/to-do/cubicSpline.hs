import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import Diagrams.CubicSpline.Internal

import Control.Newtype


poly cs t = foldr mul 0 cs
  where mul a b = a + b * t

-- Interpolate directly on the spline coefficents
interpolate closed vs = concatMap (\c -> map (poly c) (map fromRational [0.0,0.01..1.0])) cs
  where
    cs = solveCubicSplineCoefficients closed vs


main = defaultMain (pad 1.1 (mconcat [a,b,c]))
  where 
    vs = map P [(0,10),(2,2),(3.5,8),(4.5,6)]
    closed = True
    
    a = (lc blue  . lw 0.05 . stroke . fromVertices) (overF P (interpolate closed) vs)
    b = (lc red   . lw 0.07 . stroke)                (cubicSpline closed vs)
    c = (lc green . lw 0.1  . stroke . fromVertices) vs
