---
title: Star
author: Brent Yorgey
authorurl: http://www.cis.upenn.edu/~byorgey/
date: 2011-06-19
description: Star construction using straight lines.
tags: star, arrow
width: 400
height: 400
---

> {-# LANGUAGE NoMonomorphismRestriction, TupleSections #-}
> import Diagrams.Prelude hiding (connect)
> import Data.Colour.SRGB (sRGB24read)

> colors = map sRGB24read ["#5E0042", "#00856A"]

To create one quarter of the diagram, we connect the corresponding
points with headless arrows and alternate the line colors.

> quarter :: Int -> Diagram B
> quarter n = mconcat [ makeLine (getColor i) (p2 (i, 0)) (p2 (0, n - i)) | i <- [0..n] ]
>   where
>     getColor :: Int -> Colour Double
>     getColor i = colors !! (i `mod` 2)
>
>     makeLine :: Colour Double -> P2 Double -> P2 Double -> Diagram B
>     makeLine color p q =
>       p ~~ q
>         # lc color
>         # lw thin

The final diagram is created by assembling four copies of the above.

> dia :: Int -> Diagram B
> dia n = half === rotateBy (1/2) half
>   where
>     half = (rotateBy (1/4) q ||| q) # centerX
>     q = quarter n

> example =
>   pad 1.1
>     (   dia 20    # centerXY
>      <> square 50 # fc whitesmoke)
