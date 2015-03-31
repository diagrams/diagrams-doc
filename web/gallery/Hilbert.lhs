---
title: Hilbert curve
author: John Tromp
authorurl: http://homepages.cwi.nl/~tromp/
date: 2011-05-25
description: An order-5 approximation to the space-filling Hilbert curve.
tags: fractal, hilbert, curve
width: 400
height: 400
---

> {-# LANGUAGE NoMonomorphismRestriction #-}
> import Diagrams.Prelude
>
> hilbert = iterate expand mempty where
>   expand t = alignBL $ hcat [u, hrule 1, reflectX u] where
>              u = vcat [t, alignT $ vrule 1, rotateBy (3/4) t]
>
> example = pad 1.1 . centerXY $ hilbert!!5
