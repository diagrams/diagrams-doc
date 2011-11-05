---
title: Apollonius
author: Brent Yorgey
authorurl: http://www.cis.upenn.edu/~byorgey/
date: 2011-MM-DD
description: Circles of Apollonius using Descartes theorem
tags: apollonius, quickcheck
height: 400
width: 400
---

> import Diagrams.Prelude
> import Diagrams.Backend.Cairo.CmdLine
>
> import Test.QuickCheck

Descarte's theorem allows us, given three "kissing" circles, to
calculate the radius of two other circles that "kiss" all three of our
original circles.  The two circles are an "inner" and an "outer" one,
distinguished by the sign in the following expression for the radius.

> descartesR sgn r1 r2 r3 =
>   (r1 * r2 * r3) /
>     (r1*r2 + r1*r3 + r2*r3 + sgn * 2 * sqrt(r1*r2*r3*(r1+r2+r3)))

We can add a QuickCheck property to make sure that we got the
rearrangement of Descartes' theorem we use here right: the theorem is
usually stated in terms of the curvature of circles, rather than their
radii, so requires a little algebra to get it into a usable form.

> prop_descartes (NonZero r1) (NonZero r2) (NonZero r3) = 
>   abs (2*(e1*e1 + e2*e2 + e3*e3 + e4*e4) - (e1+e2+e3+e4)^2) < 1e-10
>   where [e1,e2,e3,e4] = map (1/) [r1,r2,r3,r4]
>         r4 = descartesInR r1 r2 r3

We calculate the radii of the inner and outer kissing circles.

> descartesOutR = descartesR (-1)
> descartesInR  = descartesR 1

Three circles of radius one at the vertices of an equilateral triangle
of side length 2 are all tangential, so satisfy the preconditions for
Descartes' theorem.

> circles = decoratePath (eqTriangle 2) (repeat unitCircle)

We calculate the radii of the inner and outer kissing circles and draw
them along with our three original circles.

> example = pad 1.05 $ scale 100 $
>   circle (descartesInR 1 1 1) <> circle (descartesOutR 1 1 1) <> circles

