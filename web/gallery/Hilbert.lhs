---
title: Hilbert curve
author: Jeffrey Rosenbluth
authorurl: https://www.projects.haskell.org/diagrams/
date: 2015-04-21
description: An order-5 approximation to the space-filling Hilbert curve.
tags: fractal, hilbert, curve
width: 400
height: 400
---

> {-# LANGUAGE NoMonomorphismRestriction #-}
>
> import Diagrams.Prelude
>
> hilbert 0 = mempty
> hilbert n = hilbert (n-1) # rotateBy (1/4) # reflectY <> vrule 1
>          <> hilbert (n-1) <> hrule 1
>          <> hilbert (n-1) <> vrule (-1)
>          <> hilbert (n-1) # rotateBy (1/4) # reflectX
>
>
> example = frame 1 . lw medium . lc darkred
>                   . strokeT $ hilbert 5
