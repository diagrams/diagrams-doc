{-# LANGUAGE CPP                       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module ToWrite where

#ifdef USE_SVG
import           Diagrams.Backend.SVG.CmdLine
#else
import           Diagrams.Backend.Cairo.CmdLine
#endif

import           Diagrams.Prelude
import           Control.Lens ((&), (.~))

frame = rect 8.5 11
      # lw 0.4
      # lineJoin LineJoinRound

textLines = vcat' (with & sep .~ 1.5 ) [s,l,l,s,l,l]
          # centerXY
          # lw 0.5
          # lineCap LineCapRound
  where l = hrule 6 # alignR
        s = hrule 5 # alignR

d = frame <> textLines

main = defaultMain (pad 1.1 d)
