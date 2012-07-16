{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

function f bound start stop 
  = cubicSpline False (map (\x -> P (x, f x)) ps)
    <> if bound 
         then (
               (P (-1, f start) ~~ P (start, f start))
            <> (P (stop, f stop) ~~ P (1, f stop))
              )
         else mempty
         
  where ps | bound = [start,start + 0.1 .. stop]
           | otherwise = [-1, -0.9 .. 1]

mark = vrule 2 # dashing [0.05,0.03] 0 # lc grey

dia f b start stop = function f b start stop 
                <> mark # translateX start <> mark # translateX stop

d b = dia (\x -> x^2 - 0.5) b (-0.4) (0.7)

dBound = d True
dNoBound = d False

main = defaultMain dBound