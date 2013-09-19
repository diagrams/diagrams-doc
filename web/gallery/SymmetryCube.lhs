---
title: Symmetry Cube
author: Chris Mears
authorurl: http://www.cmears.id.au/
date: 2013-08-09
description: Basic diagram with text, boxes and arrows.
tags: arrows, text
height: 400
width: 600
---

> import Data.List.Split
> import Data.Maybe
> import Diagrams.Backend.Cairo
> import Diagrams.BoundingBox
> import Diagrams.Core.Envelope
> import Diagrams.Coordinates
> import Diagrams.Prelude
> import Graphics.SVGFonts

The diagram is the boxes (the "cube") and the lines between the boxes.

> example = let c = cube
>           in pad 1.1 . centerXY $ c <> drawLines c

A "box" is a diagram (the "innards") surrounded by a rounded
rectangle.  First the innards are padded by a fixed amount, then we
compute its height and width -- that's the size of the surrounding
rectangle.

> box innards padding =
>     let padded =                  strutY padding
>                                        ===
>              (strutX padding ||| centerXY innards ||| strutX padding)
>                                        ===
>                                   strutY padding
>         height = diameter (r2 (0,1)) padded
>         width  = diameter (r2 (1,0)) padded
>     in centerXY innards <> roundedRect width height 0.1
>
> textOpts s n = TextOpts s lin2 INSIDE_H KERN False 1 n

A single string of text.

> text' :: String -> Double -> Diagram Cairo R2
> text' s n = textSVG_ (textOpts s n) # fc black # lw 0

Several lines of text stacked vertically.

> centredText ls n = vcat' with { catMethod = Distrib, sep = (n) }
>                      (map (\l -> centerX (text' l n)) ls)
> centredText' s = centredText (splitOn "\n" s)

Diagram-specific parameters, including the positioning vectors.

> padAmount = 0.5
>
> down :: R2
> down = (0& (-10))
>
> upright :: R2
> upright = (7&5)
>
> right :: R2
> right = (15&0)

A box with some interior text and a name.

> mybox s n = (box (centredText' s 1) padAmount) # named n

The cube is just several boxes superimposed, positioned by adding
together some positioning vectors.

> cube :: Diagram Cairo R2
> cube = mconcat
>   [ mybox "Permutation" "perm"
>   , mybox "Permutation\ngroup" "permgroup"                     # translate right
>   , mybox "Symmetry" "sym"                                     # translate upright
>   , mybox "Parameterised\npermutation" "paramperm"             # translate down
>   , mybox "Parameterised\npermutation\ngroup" "parampermgroup" # translate (right ^+^ down)
>   , mybox "Parameterised\nsymmetry" "paramsym"                 # translate (down ^+^ upright)
>   , mybox "Symmetry\ngroup" "symgroup"                         # translate (upright ^+^ right)
>   , mybox "Parameterised\nsymmetry\ngroup" "paramsymgroup"     # translate (down ^+^ right ^+^ upright)
>                ]

For each pair (a,b) of names, draw an arrow from diagram "a" to
diagram "b".

> drawLines :: Diagram Cairo R2 -> Diagram Cairo R2
> drawLines cube = foldr (.) id (map (uncurry connectBox) pairs) cube
>   where pairs = [ ("perm","permgroup")
>                 , ("perm","sym")
>                 , ("perm","paramperm")
>                 , ("paramperm","paramsym")
>                 , ("sym","symgroup")
>                 , ("paramsym","paramsymgroup")
>                 , ("permgroup","symgroup")
>                 , ("paramperm","parampermgroup")
>                 , ("symgroup","paramsymgroup")
>                 , ("sym","paramsym")
>                 , ("permgroup","parampermgroup")
>                 , ("parampermgroup","paramsymgroup")
>                 ]

Draw an arrow from diagram named "n1" to diagram named "n2".  The
arrow lies on the line between the centres of the diagrams, but is
drawn so that it stops at the boundaries of the diagrams, using traces
to find the intersection points.

> connectBox n1 n2 = withName n1 $ \b1 ->
>                 withName n2 $ \b2 ->
>                   let v = location b2 .-. location b1
>                       midpoint = location b1 .+^ (v/2)
>                       p1 = fromJust $ traceP midpoint (-v) b1
>                       p2 = fromJust $ traceP midpoint v b2
>                   in atop (arrowBetween' with { headSize=0.8
>                                               , shaftWidth=0.01} p1 p2)
