---
title: Star
author: Brent Yorgey
authorurl: http://www.cis.upenn.edu/~byorgey/
date: 2011-06-19
description: Star construction using straight lines.
tags: star, names, connect
width: 400
---

> {-# LANGUAGE NoMonomorphismRestriction, TupleSections #-}
> import Diagrams.Prelude

To create one quarter of the diagram, we first create some "axes"
which are just lines with evenly spaced named points along them which
we can refer to later.  Names can be almost any type; here we choose
`(String, Int)` pairs.

> axes n = h <> v
>   where p = fromOffsets . replicate n
>         h = stroke' with {vertexNames = [map ("x",) [0..n]]} (p unitX)
>         v = stroke' with {vertexNames = [map ("y",) [0..n]]} (p unitY)

To connect two named using index `i`, we request the points
corresponding to those names, and superimpose a line between the points:

> connect n i = withNames [("x",i), ("y", n - i)]
>               $ atop . fromVertices . map location

Finish creating one quarter of the diagram by connecting corresponding
points.

> pic n = applyAll (map (connect n) [0..n]) (axes n) # centerXY # lw 0.05

The final diagram is created by assembling four copies of the above.
 
> d n = half === rotateBy (1/2) half
>   where half = (rotateBy (1/4) (pic n) ||| pic n) # centerX
>         
> example = pad 1.1 $ d 20 # lc blue
