---
title: Pentaflake
author: Brent Yorgey
authorurl: http://www.cis.upenn.edu/~byorgey/
date: 2011-05-22
description: Fractal construction with pentagons.
tags: fractal, pentagon
height: 400
width: 400
---

> {-# LANGUAGE NoMonomorphismRestriction #-}
> module Pentaflake where
>
> import Diagrams.Prelude
> import qualified Data.Colour as C

We can use the [`colour`
library](http://hackage.haskell.org/package/colour) to generate
successively lighter shades of blue:

> colors = iterate (C.blend 0.1 white) blue

An order-0 pentaflake is just a pentagon:

> p = polygon with { sides = 5, orientation = OrientToX }
>     # lw 0
> 
> pentaflake 0 = p

An [order-n pentaflake](http://mathworld.wolfram.com/Pentaflake.html) is an order-(n-1) pentaflake surrounded by five
more.  The `appends` function is useful here for positioning the five
pentaflakes around the central one.

> pentaflake n = appends (p' # fc (colors !! (n-1)))
>                        (zip vs (repeat (rotateBy (1/2) p')))
>   where vs = take 5 . iterate (rotateBy (1/5))
>                     . (if odd n then negateV else id) $ unitY
>         p' = pentaflake (n-1)
> 
> pentaflake' n = pentaflake n # fc (colors !! n)

An order-4 pentaflake looks nice.  Of course there's an exponential
blowup in the number of primitives, so generating higher-order
pentaflakes can take a long time!

> example = pad 1.1 $ pentaflake' 4