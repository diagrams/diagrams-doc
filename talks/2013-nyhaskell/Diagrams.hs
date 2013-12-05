{-# LANGUAGE NoMonomorphismRestriction #-}

module Diagrams where

import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

dot = circle 0.1 # fc black # lw 0

theTrail = fromOffsets [1 ^& 1, 1 ^& (-0.5), 1 ^& 0.25, 0.25 ^& 1, 1 ^& 0]

showTrail :: Trail R2 -> Diagram B R2
showTrail t =
  mconcat
  [ mconcat (map (place dot) (trailVertices (t `at` origin)))
  , strokeTrail t # lc green
  ]

showTrailSegs :: Trail R2 -> Diagram B R2
showTrailSegs t =
  explodeTrail (t `at` origin)
  # map (\l -> arrowBetween' (with & arrowShaft .~ unLoc l) (loc l) (loc l .+^ trailOffset (unLoc l)) # lc green)
  # mconcat
