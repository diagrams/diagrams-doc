{-# LANGUAGE NoMonomorphismRestriction #-}

module Diagrams where

import           Diagrams.Backend.Postscript.CmdLine
import           Diagrams.Prelude

dot = circle 0.1 # fc black # lw 0

showTrail :: Trail R2 -> Diagram B R2
showTrail t =
  mconcat
  [ mconcat (map (place dot) (trailVertices (t `at` origin)))
  , strokeTrail t
  ]
