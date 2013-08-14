{-# LANGUAGE NoMonomorphismRestriction #-}

module Exercises where

import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

main = defaultMain (pad 1.1 (text "e" # fc green <> square 1 # lw 0))
