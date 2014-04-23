---
title: Pentaflake
author: Brent Yorgey
authorurl: http://www.cis.upenn.edu/~byorgey/
date: 2011-05-22
description: Fractal construction with pentagons.
tags: fractal, pentagon
width: 400
---

> {-# LANGUAGE NoMonomorphismRestriction #-}
> import Diagrams.Prelude

A $0$-pentaflake is just a regular pentagon:

> pentaflake' 0 = regPoly 5 1 # lw none

An [$n$-pentaflake](http://mathworld.wolfram.com/Pentaflake.html)
is an $(n-1)$-pentaflake surrounded by five more.  The `appends`
function is useful here for positioning the five pentaflakes around
the central one.

> pentaflake' n = appends
>                   pCenter
>                   (zip vs (repeat (rotateBy (1/2) pOutside)))
>   where vs = iterateN 5 (rotateBy (1/5))
>            . (if odd n then negateV else id)
>            $ unitY
>         pCenter  = pentaflake' (n-1)
>         pOutside = pCenter # opacity (1.7 / fromIntegral n)
>
> pentaflake n = pentaflake' n # fc navy # bg silver

A $4$-pentaflake looks nice.  Of course there's an exponential
blowup in the number of primitives, so generating higher-order
pentaflakes can take a long time!

> example = pentaflake 4
