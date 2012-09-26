---
title: Pythagorean Tree
author: Chris Mears
authorurl: http://www.cmears.id.au/
date: 2012-09-26
description: A fractal made of squares and right-angled triangles that looks like a tree.
tags: fractal
height: 200
width: 400
---

This diagrams was inspired by the one at
http://projecteuler.net/problem=395, which explains the algorithm for
constructing the tree.

> {-# LANGUAGE NoMonomorphismRestriction #-}
> import Diagrams.Prelude
> import Diagrams.TwoD
> import Data.Colour (blend)

An order n tree has a square and a right-angled triangle on top, and
an order n-1 tree on each short side of the triangle.  As the
recursion deepens, the limbs get smaller and more transparent.

For aesthetics, let the leaves have circles instead of squares.

> tree 1 = circle 1   # translate (r2 (0, 1/2)) # colourise green
> tree n =
>   square 1          # translate (r2 (0, 1/2)) # colourise burlywood
>   `atop` triangle   # translate (r2 (0,1))    # colourise brown
>   `atop` tree (n-1) # rotate (-asin 0.8 :: Rad) # scale 0.6 # translate (r2 ( 0.32,1.24)) # fade
>   `atop` tree (n-1) # rotate ( asin 0.6 :: Rad) # scale 0.8 # translate (r2 (-0.18,1.24)) # fade
>   where
>     triangle = translate (r2 (-0.5,0)) . stroke . close
>                  . fromVertices . map p2 $ [(0,0), (1,0), (0.8*0.8,0.8*0.6)]
>     fade = opacity 0.95
>
> colourise c = fc c . lc (blend 0.5 black c)

Draw the order 10 tree.

> example = tree 10
