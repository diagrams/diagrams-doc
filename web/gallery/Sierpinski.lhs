---
title: Sierpinski triangle
author: Brent Yorgey
authorurl: http://www.cis.upenn.edu/~byorgey/
date: 2011-05-21
description: Sierpinski's classic fractal.
tags: fractal, Sierpinski, triangle
width: 400
---

> {-# LANGUAGE NoMonomorphismRestriction #-}
> import Diagrams.Prelude
>
> sierpinski 1 = eqTriangle 1
> sierpinski n =     s
>                   ===
>                (s ||| s) # centerX
>   where s = sierpinski (n-1)
> 
> example = pad 1.1 $ sierpinski 7 # centerXY # lw 0 # fc black
