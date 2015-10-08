---
title: Heighway dragons
author: Pontus Granström
authorurl: http://github.com/pnutus/
date: 2015-10-08
description: Step-by-step Heighway dragon fractal.
tags: heighway dragon, fractal
width: 800
height: 800
---

> {-# LANGUAGE FlexibleContexts #-}
> import Diagrams.Backend.SVG.CmdLine
> import Diagrams.Prelude
> import Diagrams.TwoD.Layout.Grid

We generate an infinite sequence of increasingly detailed [dragon curves](https://en.wikipedia.org/wiki/Dragon_curve), starting with a simple line. Each curve is two copies of the previous curve rotated, scaled, one of them inverted, and stuck together.

> dragonCurves = map (trailLike . (`at` origin)) (iterate fold initialTrail)
>   where
>     initialTrail = hrule 1
>     fold trail = (trail # rotateBy (-1/8) 
>                     <> trail # rotateBy (5/8) # reverseTrail) 
>                   # scale (1/sqrt 2)

The above is enough to generate a Heighway dragon of arbitrary level of detail, but let's go a little further to show the relation of successive curves in the sequence.

`withPrevious` combines each diagram in a list with a shadow of the previous one.

> withPrevious diagrams = zipWith (<>) diagrams (mempty : diagrams # opacity 0.2)

We remember the order of the diagrams by giving them names, so that we can lay them out and then show the order with arrows.

> rememberOrder :: [Diagram B] -> [Diagram B]
> rememberOrder = zipWith named [0::Int ..]
> 
> showOrder :: Diagram B -> Diagram B
> showOrder diagram 
>   = diagram # applyAll (map addArrow [0 .. length (names diagram)])
>   where
>     addArrow n = connectOutside' opts n (n + 1)
>     opts = with & gaps .~ normalized 0.005 
>                 & headLength .~ tiny

Finally, we put all of the above together, with some layout tricks to make the diagrams and arrows align properly. `gridSnake` lays out the diagrams in a "snaking" grid, so that each diagram is adjacent to the previous one.

> example = dragonCurves # withPrevious 
>                        # take 12
>                        # sameBoundingRect
>                        # rememberOrder 
>                        # map (frame 0.1) 
>                        # gridSnake
>                        # showOrder 
>                        # lw ultraThin
>
> main = mainWith (example :: Diagram B)