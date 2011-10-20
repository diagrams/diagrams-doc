---
title: Sierpinski triangle
author: Brent Yorgey
authorurl: http://www.cis.upenn.edu/~byorgey/
date: 2011-05-21
description: Sierpinski's classic fractal.
tags: fractal, Sierpinski, triangle
height: 400
width: 400
---

> {-# LANGUAGE NoMonomorphismRestriction #-}
> module Sierpinski where
> import Diagrams.Prelude
> 
> sierpinski 1 = t
> sierpinski n =     s
>                   ===
>                (s ||| s)
>   where s = sierpinski (n-1)
> 
> t = regPoly 3 1
>     # lw 0
>     # fc black
> 
> example = pad 1.1 $ sierpinski 7
