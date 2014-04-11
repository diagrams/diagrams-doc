---
title: Vector field
author: Jeffrey Rosenbluth
authorurl: https://www.projects.haskell.org/diagrams/
date: 2013-08-13
description: Arrows of a vector field.
tags: arrow, vector field
width: 400
---

> {-# LANGUAGE NoMonomorphismRestriction #-}
> {-# LANGUAGE FlexibleContexts          #-}
> import Diagrams.Prelude

> locs   = [(x, y) | x <- [0.1, 0.3 .. 3.25], y <- [0.1, 0.3 .. 3.25]]

Create a list of points where the vectors will be placed.

> points = map p2 locs

The function to use to create the vector field.

> vectorField (x, y) = r2 (sin (y + 1), sin (x + 1))
>
> arrows = map arrowAtPoint locs
>
> arrowAtPoint (x, y) = arrowAt' opts (p2 (x, y)) (sL *^ vf) # alignTL
>   where
>     vf   = vectorField (x, y)
>     m    = magnitude $ vectorField (x, y)
>
> -- Head size is a function of the length of the vector
> -- as are tail size and shaft length.
>
>     hs   = 0.08 * m
>     sW   = 0.015 * m
>     sL   = 0.01 + 0.1 * m
>     opts = (with & arrowHead .~ spike
>                  & headSize .~ hs
>                  & shaftStyle %~ lwG sW)
>
> field   = position $ zip points arrows
>
> example = ( field # translateY 0.05
>        <> ( square 3.5 # lwG 0 # alignBL))