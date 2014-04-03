{-# LANGUAGE NoMonomorphismRestriction #-}

module LGMDiagrams where

import           Diagrams.Example.Logo
import           Diagrams.Prelude

enbox sz d ls =
  vcat' (with & sep .~ 0.2) $ -- $
  [ mconcat
    [ d # centerXY # sizedAs (square sz :: D R2)
    , roundedRect (1.2*sz) (1.2*sz) (0.15*sz)
    ]
  , vcat (map (\l -> text l # fontSize 0.3 <> strutY 0.3) ls)
  ]

-- TODO: change to SVGFonts for better-looking lambda
pl = enbox 1 (text "λ" <> strutX 1 <> strutY 1)
       ["Math/PL theory"]

vgDia = mconcat
  [ arrow' vgAOpts 1 # rotate (70 @@ deg) # lc green
  , arrow' vgAOpts 1 # rotate (10 @@ deg) # lc blue
  ] # centerXY
  where
    vgAOpts = with & headSize .~ 0.15
                   & arrowHead .~ tri

vg = enbox 1 (vgDia <> strutX 1 <> strutY 1)
       ["Vector graphics"]

mkDiagrams d = hcat' (with & sep .~ 2)
  [ pl # named "pl"
  , enbox 1.3 d ["Diagrams"] # named "d"
  , vg # named "vg"
  ]
  # connectOutside' aOpts "pl" "d"
  # connectOutside' aOpts "vg" "d"
  # centerXY
  # frame 1

aOpts = with & gap .~ 0.2

mkColor c = roundedRect 1 1 0.1 # lw 0 # fc c

main = return ()
