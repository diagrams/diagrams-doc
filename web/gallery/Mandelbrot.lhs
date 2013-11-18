---
title: The Mandelbrot set
author: Ryan Yates
authorurl: http://www.cs.rochester.edu/u/ryates/
date: 2011-11-18
description: A discretized version of the familiar Mandelbrot set.  Adapted from [code written by MathematicalOrchid](http://warp.povusers.org/MandScripts/haskell.html).
tags: fractal, Mandelbrot
width: 400
---

> {-# LANGUAGE NoMonomorphismRestriction #-}
> -- Addapted from: "The MathematicalOrchid, 24 Feb 2007"
> --  http://warp.povusers.org/MandScripts/haskell.html
>
> import Data.Complex
> import Diagrams.Prelude hiding (magnitude,image)

Code to compute orbits of complex numbers under the Mandelbrot
transformation, and decide on the magnitude of a pixel based on how
slowly its orbit diverges.

> quadratic c z = z*z + c
>
> critical_orbit z = iterate (quadratic z) 0
>
> pixel = length . takeWhile (\z -> magnitude z <= 2) . take max_iter
> max_iter = 32

Generate a grid of points of the desired size.

> side n v0 v1 =
>    let sv = (v1 - v0) / fromIntegral n
>    in  [v0, (v0 + sv) .. v1]
>
> side_x = side 64 (-2) 2
> side_y = side 64 (-2) 2
>
> grid = map (\y -> map (:+ y) side_x) side_y

Generate the Mandelbrot image as a grid of pixel magnitudes.

> image = map (map (to_circle . pixel . critical_orbit)) grid

To lay out the pixels in a grid we have to make them into elements of
the same size, so we construct a "phantom box" out of struts which we
place behind every pixel.

> minBox = centerXY (strutX w <> strutY w)
>   where w = 2 * sqrt max_iter
>
> to_circle = (<> minBox) . centerXY . lw 0 . fc blue . circle . sqrt . fromIntegral
>
> example = vcat . map hcat $ image
