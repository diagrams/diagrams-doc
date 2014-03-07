---
title: Semiregular plane tiling
author: Brent Yorgey
authorurl: http://www.cis.upenn.edu/~byorgey/
date: 2011-11-11
description: An example of a semiregular plane tiling.
tags: Tilings, plane, regular, polygon
width: 400
---

> {-# LANGUAGE NoMonomorphismRestriction #-}
>
> import Diagrams.Prelude

To see how this example is implemented, see the source code of the `Diagrams.TwoD.Tilings` module included in the `diagrams-contrib` package. The package contains a wide variety of tools for generating plane tilings made of regular polygons.

> import Diagrams.TwoD.Tilings
>
> example = drawTiling t3464 10 10 # lc white # bg darkorange
>                                  # centerXY # pad 1.1