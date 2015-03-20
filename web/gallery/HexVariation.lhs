---
title: HexVariation
author: Alexis Praga
authorurl: https://www.projects.haskell.org/diagrams/
date: 2014-11-13
description: W. Kolmyjec's Hex Variation
tags: hexagon, kolmyjec, position
width: 400
---


This is a transcription in Haskell of "Hex Variation" by William Kolmyjec.
The algorithm itself is inspired from the version by Steve Berrick (see the
Recode project: http://recodeproject.com/artwork/v3n4hex-variation).

> {-# LANGUAGE NoMonomorphismRestriction #-}
>
> import Diagrams.Prelude
> import System.Random

We first define the parameters of the tile, which is hexagonal. The side of a
hexagon is the radius of its circumscribed circle, here taken as 1.

The apothem is the distance from the center to the side:

> h = sqrt(3)/2

We define the difference between the radius and the apothem:

> h' = cos(pi/3)

We then define a tile. The hexagon is not actually shown but inside are two
arcs, along with a vertical line. To see the tiling, you can add an hexagon in
the list below:

> hexagon' :: Diagram B
> hexagon' = mconcat [arc1 # translateX (-1)
>                   , vrule (2*h)
>                   , arc1 # rotateBy (1/2) # translateX 1
>                   ]
>     where
>       arc1 = arc' 0.5 (xDir # rotate (-pi/3 @@ rad)) (pi/3 @@ rad)

In the final tiling, the tiles will be rotated randomly with angles in `\{0,
\frac{2 \pi}{3}, \frac{4 \pi}{3} \}`.

> rotateHexagon' :: Int -> Diagram B
> rotateHexagon' n = hexagon' # rotate (n'*2*pi/3 @@ rad)
>   where
>     n' = fromIntegral n

The tiling is created from a list of centers, defined here:

> centerPosition :: Int -> Int -> (N B, N B)
> centerPosition x y
>   | (x `mod` 2 == 0) = ((2-h')*x', 2*y'*h)
>   | otherwise        = ((2-h')*x', (2*y'-1)*h)
>   where
>     x' = fromIntegral x
>     y' = fromIntegral y

The function generating random angles with a fixed seed:

> generateAngles :: [Int]
> generateAngles = randomRs (0, 2) (mkStdGen 31)

Finally, the tiling is created here:

> hexVariation :: Diagram B
> hexVariation = position (zip (map p2 pos) (map rotateHexagon' angles))
>   where
>     pos = [(centerPosition x y) | x <- [0..nb-1], y <- [0..nb-1]]
>     angles = take ((nb+1)*(nb+1)) $ generateAngles

The enveloppe of our tiling is nb*1.5*side + 0.5*side in width and nb*2*h+h in
height. We remove the "corners" to avoid "holes" at the borders of the figure
and define the new width and height:

> width' = nb*1.5 - 0.5
> height' = nb*2*h - h

Which are used to "clip" the figure here:

> nb = 12
> example :: Diagram B
> example = hexVariation # center # rectEnvelope x0 u0
>   where
>     x0 = p2 (-width'/2, -height'/2)
>     u0 = r2 (width', height')
