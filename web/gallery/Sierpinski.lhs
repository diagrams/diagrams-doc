---
title: Sierpinski triangle
author: Brent Yorgey, Jeffrey Rosenbluth
authorurl: http://www.cis.upenn.edu/~byorgey/
date: 2011-05-21
description: Sierpinski's classic fractal.
tags: fractal, Sierpinski, triangle
width: 400
height: 400
---

> {-# LANGUAGE NoMonomorphismRestriction #-}
> import Diagrams.Prelude

#### Brent's Original Code
> -- sierpinski 1 = triangle 1
> -- sierpinski n =     s
> --                  ===
> --               (s ||| s) # centerX
> --   where s = sierpinski (n-1)

#### Code to change color at each level
##### (Technically this is the complement of the Sierpinski triangle)
> import Diagrams.Prelude
> import Data.Colour.Palette.BrewerSet
>
> clrs :: [Colour Double]
> clrs = brewerSet Purples 9
>
> sierpinski :: Int -> [Colour Double] -> Diagram B
> sierpinski n c = go n <> triangle (2^n) # fc (clrs !! 0) # lw none
>   where
>     clrs = if null c then repeat black else cycle c
>     go n
>       | n == 1    = t1 # fc (clrs !! 1)
>       | otherwise = appends tri (zip vecs (replicate 3 sierp))
>       where
>         tri   = scale (2 ^ (n-1)) $ t1 # fc (clrs !! (n+1))
>         vecs  = [unitY, (rotateBy (-1/12) unitX), (rotateBy (1/12) unit_X)]
>         sierp = go (n-1)
>         t1    = triangle 1 # reflectY
>
> example =  sierpinski 7 clrs # lw none # center # frame 2
