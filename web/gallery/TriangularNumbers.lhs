---
title: Triangular number identity
author: Brent Yorgey
authorurl: http://www.cis.upenn.edu/~byorgey/
date: 2011-04-14
description: A visual proof of an identity involving triangular numbers, created for [this blog post](http://mathlesstraveled.com/2011/04/14/triangular-number-equations-via-pictures-solutions/), which also contains several other similar proofs.
tags: triangular, number, proof, identity
width: 400
---

> {-# LANGUAGE NoMonomorphismRestriction #-}
> import Diagrams.Prelude
> 
> import Data.Colour

Draw a group of dots in a triangular array, all with the same color
and backed by a solid-colored triangle to visually group them.  Note
how the dots are laid out by creating a trail called `edge`, rotating
it 60 degrees, and using `decorateTrail` to lay out the rows of dots.

> tri c n = dots <> (strokeT edges # lc c # lw 0.2 # fcA (c `withOpacity` 0.5))
>   where rows = map (hcat' with { sep = 1 })
>              . zipWith replicate [n,n-1..1]
>              . repeat
>              $ dot c
>         dots = decorateTrail (rotateBy (1/6) edge) rows
>         edge = fromOffsets . replicate (n-1) $ unitX # scale 3
>         edges = close (edge <> rotateBy (1/3) edge <> rotateBy (2/3) edge)
> 
> dot c = unitCircle
>       # lw 0
>       # fc c
> 
> rowSpc = height (rotateBy (1/6) $ strutY 1 :: D R2)

`row k n s c` draws a row of `k` size-`n` triangles with color `c`,
separated by enough space for `s` dots.

> row k n s c = hcat' with {sep = 1 + 3*s} (replicate k (tri c n))

The visual proof, which simply consists in assembling various
sub-triangles into a larger triangle, using appropriately transformed
and aligned instances of `row`.

> law4 k n c1 c2 = vcat' with {sep = rowSpc} (map tRow [1..k])
>   where tRow k = (row k n 0 c1 # centerX # alignT)
>                  <>
>                  (row (k-1) (n-1) 1 c2 # reflectY # centerX # alignT)

Finally, create a row of diagrams showing the proof at different
sizes.

> exampleRow f = hcat' with {sep = 4} . map (alignB . f)
> 
> law4Dia = exampleRow law4' [2..4]
>   where law4' k = law4 k 3 purple gold
> 
> example = pad 1.1 $ law4Dia # centerXY
