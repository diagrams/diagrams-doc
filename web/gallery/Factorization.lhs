---
title: Factorization diagrams
author: Brent Yorgey
authorurl: http://www.cis.upenn.edu/~byorgey/
date: 2012-12-29
description: Factorization diagrams, as seen on the [cover of Hacker Monthly](http://hackermonthly.com/issue-31.html).
tags: factorization, primes, recursion
width: 400
---

> {-# LANGUAGE NoMonomorphismRestriction #-}
>
> import Diagrams.Prelude

To see how this example is implemented, see the source code of the
`Diagrams.TwoD.Factorization` module included in the
`diagrams-contrib` package, or read the original blog posts [here](http://mathlesstraveled.com/2012/10/05/factorization-diagrams/) and [here](http://mathlesstraveled.com/2012/11/05/more-factorization-diagrams/).

> import Diagrams.TwoD.Factorization
>
> example = fdGridList 6 # center # pad 1.05