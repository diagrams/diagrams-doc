---
title: Circular gray code
author: Brent Yorgey
authorurl: http://www.cis.upenn.edu/~byorgey/
date: 2011-05-21
description: Circular gray code, like that used on some rotational sensors.
tags: gray, code
width: 400
---

> {-# LANGUAGE NoMonomorphismRestriction #-}
> import Diagrams.Prelude     hiding (gray)
> import Data.List.Split      (chunk)
> import Data.Maybe           (catMaybes)
> import Control.Applicative
> import Data.Monoid          (mconcat)
> import Data.List            (transpose)

`gray n` recursively generates an n-bit Gray code, where each n-bit
binary number differs from the next in exactly one position.

> gray 0 = [[]]
> gray n = map (False:) g ++ map (True:) (reverse g)
>   where g = gray (n-1)

Construct a circular diagram from the n-bit gray code: each bit
position corresponds to a concentric ring, with black/white indicating
0/1.  `ringOffsets` converts a list of booleans into a list of angular
segments corresponding to consecutive runs of `True`.

> rings n = mkRingsDia . map ringOffsets . transpose . gray $ n
>   where ringOffsets :: [Bool] -> [(CircleFrac, CircleFrac)]
>         ringOffsets = map l2t . chunk 2 . findEdges . zip [0,1/(2^n)..1]
>         l2t [x,y] = (x,y)
>         l2t [x]   = (x,1)
> 
> findEdges :: Eq a => [(CircleFrac, a)] -> [CircleFrac]
> findEdges = catMaybes . (zipWith edge <*> tail)
>   where edge (_,c1) (a,c2) | c1 /= c2  = Just a
>                            | otherwise = Nothing

Generate concentric circular arcs from lists of angular segments.

> mkRingsDia = freeze . mconcat . zipWith mkRingDia [2,3..]
>   where mkRingDia r = lw 1.05 . mconcat . map (stroke . scale r . uncurry arc)
> 
> example = pad 1.1 (rings 10)