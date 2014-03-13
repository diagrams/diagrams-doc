.. role:: pkg(literal)
.. role:: hs(literal)
.. role:: mod(literal)
.. role:: repo(literal)

.. default-role:: hs

.. contents::

Introduction
============

Arrows come in many shapes and sizes and ``diagrams`` provides a wide
variety of flexible and extensible tools for creating and using
arrows. The diagram below gives a small taste of some of the different
arrows that can be created easily with ``diagrams``. The
`Diagrams.TwoD.Arrow`:mod: module, along with
`Diagrams.TwD.Arrowheads`:mod:, provides a collection of functions and
options used to make arrows.

.. class:: dia

::

> example = d # connect' (with & arrowHead .~ dart & headSize .~ 1 & arrowTail .~ quill
>                              & tailSize .~ 1 & shaftStyle %~ lw 0.1 . lc black & arrowShaft .~ s
>                              & tailGap .~ 0.1 & headGap .~ 0.1
>                              & headColor .~ blue & tailColor .~ orange)
>                              "1" "2"
>             # connect' (with & arrowHead .~ missile & headSize .~ 0.8 & arrowTail .~ missile'
>                              & tailSize .~ 0.8 & shaftStyle %~ lw 0.05 & arrowShaft .~ s1
>                              & headGap .~ 0 & tailGap .~ 0.1)
>                              "4" "3"
>             # connect' (with & arrowHead .~ thorn & headSize .~ 0.8 & arrowShaft .~ a1
>                              & arrowTail .~ noTail & shaftStyle %~ lw 0.03
>                              & tailGap .~ 1 & headGap .~ 1 )
>                              "1" "6"
>             # connect' (with & arrowHead .~ dart & tailSize .~ 1 & arrowTail .~ dart'
>                              & headSize .~ 1 & arrowShaft .~ s2
>                              & headColor .~ green & tailColor .~ green
>                              & shaftStyle %~ lw 0.1 . lc green )
>                              "4" "7"
>             # connect' (with & arrowTail .~ dart' & tailSize .~ 1 & arrowShaft .~ a
>                              & arrowHead .~ spike & headSize .~ 1 & headColor .~ red
>                              & tailGap .~ 0.1 & tailColor .~ red
>                              & shaftStyle %~ lw 0.2 . lc blue )
>                              "9" "5"
>             # connect' (with & arrowHead .~ tri & arrowTail .~ block
>                              & headSize .~ 1 & tailSize .~ 0.6 & headGap .~ 0.5
>                              & headStyle %~ fc black . opacity 0.5
>                              & tailStyle %~ fc black . opacity 0.5
>                              & shaftStyle %~ lw 0.06 . dashing [0.1,0.2,0.3,0.1] 0)
>                              "8" "9"
>             # scale 0.75
>   where
>     c = circle 1 # showOrigin # lw 0.04
>     a = arc (5/12 @@ turn) (11/12 @@ turn)
>     a1 = arc (1/2 @@ turn) (3/4 @@ turn)
>     t = bezier3 (r2 (1,1)) (r2 (1,1)) (r2 (0,2))
>     t' = reflectX t
>     l = straight unitX
>     l' = straight (unitX # rotateBy (1/6))
>     s = trailFromSegments [t, l, t', l, t]
>     s1 = cubicSpline False (trailVertices (s `at` origin))
>     s2 = cubicSpline False (map p2 [(0,0), (1,0), (0.8, 0.2),(2,0.2)])
>     x |-| y = x ||| strutX 12 ||| y
>     row1 = (c # named "1") |-| (c # named "2") |-| (c # named "3")
>     row2 = (c # named "4") |-| (c # named "5") |-| (c # named "6")
>     row3 = (c # named "7") |-| (c # named "8") |-| (c # named "9")
>     d = row1
>         ===
>         strutY 4
>         ===
>         row2
>         ===
>         strutY 4
>         ===
>         row3

Optional and named parameters
-----------------------------

Most of the arrow functions take an `opts` parameter (see `Faking
optional named parameters`__) of type `ArrowOpts`, these functions typically
have companion functions that use a default set of `ArrowOpts`. For example
the functions `arrow'` and `arrow`. The former takes an `opts` parameter and
the latter does not. In this tutorial whenever we mention a function with
a single quote (`'`) at the end, there is a sister function without the quote that
uses a default set of options.

__ http://projects.haskell.org/diagrams/doc/manual.html#faking-optional-named-arguments

Scale invariance
----------------

Arrowheads and -tails are the canonical example of *scale invariant*
objects: they are not affected by scaling (though they are affected by
other transformations such as rotation and translation). The
`scale-invariance section of the user manual`__ has a good example
showing why scale-invariance is necessary for the creation of
arrowheads; detailed documentation explaining scale invariant objects
is in `Diagrams.TwoD.Transform.ScaleInv`:mod:.  The most important
consequence for day-to-day diagramming with arrows is that arrowheads
and -tails do not contribute to the envelope of an arrow (arrow
shafts, on the other hand, do).

__ http://projects.haskell.org/diagrams/doc/manual.html#scale-invariance

.. container:: warning

  Arrowheads and tails do not contribute to the envelope of an arrow!

Connecting Points
=================

A typical use case for an arrow is to connect two points, having an
arrow pointing from one to the other. The function `arrowBetween` (and
its cousin `arrowBetween'`) connects two points.

.. class:: dia-lhs

::

> sPt = p2 (0.20, 0.20)
> ePt = p2 (2.85, 0.85)
>
> -- We use small blue and red circles to mark the start and end points.
> dot  = circle 0.02 # lw 0
> sDot = dot # fc blue # moveTo sPt
> eDot = dot # fc red  # moveTo ePt
>
> example = ( sDot <> eDot <> arrowBetween sPt ePt)
>           # centerXY # pad 1.1

.. container:: exercises

  1. Create a diagram which contains a circle of radius 1 with an arrow connecting
  the points on the circumference at 45 degrees and 180 degrees.

ArrowOpts
=========

All of the arrow creation functions have a primed variant (*e.g.*
`arrowBetween` and `arrowBetween'`) which takes an additional `opts`
parameter of type `ArrowOpts`. The `opts` record is the primary means
of customizing the look of the arrow. It contains a substantial
collection of options to control all of the aspects of an arrow. Here
is the definition for reference:

.. class:: lhs

::

  data ArrowOpts = ArrowOpts
    { _arrowHead  :: ArrowHT
    , _arrowTail  :: ArrowHT
    , _arrowShaft :: Trail R2
    , _headSize   :: Double
    , _tailSize   :: Double
    , _headGap    :: Double
    , _tailGap    :: Double
    , _headStyle  :: Style R2
    , _tailStyle  :: Style R2
    , _shaftStyle :: Style R2
    }

Don't worry if some of the field types in this record are not yet clear,
we will walk through each field
and occasionally point to the API reference for material that we don't
cover in this tutorial.

The head and tail shape
-----------------------

The `arrowHead` and `arrowTail` fields contain information needed to
construct the head and tail of the arrow, the most important aspect
being the shape. So, for example, if we set `arrowHead=spike` and
`arrowTail=quill`,

.. class:: lhs

::

> arrowBetween' (with & arrowHead .~ spike & arrowTail .~ quill) sPt ePt

then the arrow from the previous example looks like this:

.. class:: dia

::

> sPt = p2 (0.20, 0.20)
> ePt = p2 (2.85,  0.85)
>
> dot = circle 0.02 # lw 0
> sDot = dot # fc blue # moveTo sPt
> eDot = dot # fc red # moveTo ePt
>
> example = (sDot <> eDot <> arrowBetween' (with & arrowHead .~ spike
>                                                & arrowTail .~ quill) sPt ePt)
>          # centerXY # pad 1.1

The `Arrowheads` package exports a number of standard arrowheads
including, `tri`, `dart`, `spike`, `thorn`, `missile`, `lineHead`, and `noHead`,
with `dart` being
the default. Also available are companion functions like `arrowheadDart`
that allow finer control over the shape of a dart style head. For tails,
in addition to `quill` are `block`, `lineTail`, and `noTail`. Again for more control
are functions like, `arrowtailQuill`. Finally, any of the standard arrowheads
can be used as tails by appending a single quote, so for example:

.. class:: lhs

::

> arrowBetween' (with & arrowHead .~ thorn & arrowTail .~ thorn') sPt ePt

yields:

.. class:: dia

::

> sPt = p2 (0.20, 0.20)
> ePt = p2 (2.85, 0.85)
>
> dot = circle 0.02 # lw 0
> sDot = dot # fc blue # moveTo sPt
> eDot = dot # fc red # moveTo ePt
>
> example = ( sDot <> eDot <>arrowBetween' (with & arrowHead .~ thorn
>                                                & arrowTail .~ thorn') sPt ePt)
>           # centerXY # pad 1.1


The shaft
----------

The shaft of an arrow can be any arbitrary `Trail R2` in addition to a
simple straight line. For example, an arc makes a perfectly good
shaft. The length of the trail is irrelevant, as the arrow is scaled
to connect the starting point and ending point regardless of the
length of the shaft.  Modifying our example with the following code
will make the arrow shaft into an arc:

.. class:: lhs

::

> shaft = arc (0 @@ turn) (1/2 @@ turn)
>
> example = ( sDot <> eDot
>          <> arrowBetween' (with & arrowHead .~ spike & arrowTail .~ spike'
>                                 & arrowShaft .~shaft) sPt ePt)
>           # centerXY # pad 1.1

.. class:: dia

::

> sPt = p2 (0.20, 0.40)
> ePt = p2 (2.80, 0.40)
>
> dot = circle 0.02 # lw 0
> sDot = dot # fc blue # moveTo sPt
> eDot = dot # fc red # moveTo ePt
>
> shaft = arc (0 @@ turn) (1/2 @@ turn)
>
> example = ( sDot <> eDot
>          <> arrowBetween' (with & arrowHead .~ spike & arrowTail .~ spike'
>                                 & arrowShaft .~ shaft) sPt ePt)
>           # centerXY # pad 1.1

Arrows with curved shafts don't always render the way our intuition
may lead us to expect. One could reasonably expect that the arc in the
above example would produce an arrow curving upwards, not the
downwards-curving one we see.  To understand what's going on, imagine
that the arc is `Located`. Suppose the arc goes from the point
`(0,0)`:math: to `(-1,0)`:math:. This is indeed an upwards curving arc
with origin at `(0,0)`:math:. Now suppose we want to connect points
`(0,0)`:math: and `(1,0)`:math:. We attach the arrow head and tail and
rotate the arrow about its origin at `(0,0)`:math: until the tip of
the head is touching `(1,0)`:math:.  This rotation flips the arrow
vertically.

In order to get the arrow to curve upwards we might initially think we
could create the shaft reversing the order of the angles, using `arc
(1/2 @@ turn) 0`, but this won't work either, as it creates a
downwards curving arc from, say, `(0,0)`:math: to `(1,0)`:math: that
does not need to be rotated. The only way to achieve the desired
result of making the arrow pointing from `(0,0)`:math: to
`(1,0)`:math: curve upwards is to reverse the trail:

.. class:: lhs

::

> shaft = arc (0 @@ turn) (1/2 @@ turn) # reverseTrail

.. class:: dia

::

> sPt = p2 (0.20, 0.40)
> ePt = p2 (2.80, 0.40)
> dot = circle 0.02 # lw 0
> sDot = dot # fc blue # moveTo sPt
> eDot = dot # fc red # moveTo ePt
> shaft = arc (0 @@ turn) (1/2 @@ turn) # reverseTrail
> example = ( sDot <> eDot
>          <> arrowBetween' (with & arrowHead .~ spike & arrowTail .~ spike'
>                                 & arrowShaft .~ shaft) sPt ePt)
>           # centerXY # pad 1.1

.. container:: warning

  If an arrow shaft does not appear as you expect, then try using `reverseTrail`.

Here are some exercises to try.

.. container:: exercises

  Construct each of the following arrows pointing from `(1,1)`:math: to
  `(3,3)`:math: inside a square with side `4`:math:.

  1. A straight arrow with no head and a spike shaped tail.

  #. An arrow with a `45`:math: degree arc for a shaft, triangles for both head
     and tail, curving downwards.

  #. The same as above, only now make it curve upwards.

Size, and gaps
--------------

The fields `headSize` and `tailSize` are for setting the size of the
head and tail. The head and tail size are specified as the diameter of
an imaginary circle that would circumscribe the head or tail. The
default value is 0.3. The `headGap` and
`tailGap` options are also fairly self explanatory: they leave space
at the end or beginning of the arrow. Take a look at their effect in
the following example. The default gaps are 0.

.. class:: dia-lhs

::

> sPt = p2 (0.20, 0.50)
> mPt = p2 (1.50, 0.50)
> ePt = p2 (2.80, 0.50)
>
> dot  = circle 0.02 # lw 0
> sDot = dot # fc blue  # moveTo sPt
> mDot = dot # fc green # moveTo mPt
> eDot = dot # fc red   # moveTo ePt
>
>
> leftArrow  = arrowBetween' (with & arrowHead .~ missile & arrowTail .~ spike'
>                                  & headSize .~ 0.15 & tailSize .~ 0.1
>                                  & shaftStyle %~ lw 0.02
>                                  & headGap .~ 0.05) sPt mPt
> rightArrow = arrowBetween' (with & arrowHead .~ tri & arrowTail .~ dart'
>                                  & headSize .~ 0.25 & tailSize .~ 0.2
>                                  & shaftStyle %~ lw 0.015
>                                  & tailGap .~ 0.1) mPt ePt
>
> example = ( sDot <> mDot <> eDot <> leftArrow <> rightArrow)
>           # centerXY # pad 1.1

Our use of the `lens`:pkg: package allows us to create other lenses to
modify `ArrowOpts` using the same syntax as the record field
lenses. For example, the functions `headWidth` and `tailWidth` can be
used to set the `headSize` and `tailSize` to a specified width. This
is useful, for example, for making the arrowhead and tail take up the
same length of shaft. In fact `widths` can be used to simultaneously
set the size of the head and tail so that they have the specified
width. Similarly `gap` and `sizes` can be used to simultaneously set
the `headGap` / `tailGap` and the `headSize` / `tailSize`
respectively.

A useful pattern is to use `lineTail` together with `widths` as in the
following example:

.. class:: dia-lhs

::

> dia = (rect 5 2 # fc lavender # alignX (-1) # showOrigin # named "A")
>        === strutY 2 ===
>       (rect 5 2 # fc pink # alignX (-1) # showOrigin # named "B")
>
> ushaft = trailFromVertices (map p2 [(0, 0), (-0.5, 0), (-0.5, 1), (0, 1)])
>
> uconnect tl setWd =
>   connect' (with
>           & arrowHead .~ spike
>           & arrowShaft .~ ushaft
>           & shaftStyle %~ lw 0.1 . lc black
>           & arrowTail .~ tl
>           & setWd)
>
> example =
>   hcat' (with & sep .~ 1.5)
>   [ dia # uconnect noTail   (headWidth .~ 0.5) "B" "A"  -- looks bad
>   , dia # uconnect lineTail (widths    .~ 0.5) "B" "A"  -- looks good!
>   ]
>   # frame 1.1

The style options
-----------------

By default, arrows are drawn using the current line color (including
the head and tail).  In addition, the shaft styling is taken from the
current line styling attributes.  For example:

.. class:: dia-lhs

::

> example = mconcat
>   [ square 2
>   , arrowAt origin unitX
>     # lc blue
>   ]
>   # dashing [0.05, 0.05] 0
>   # lw 0.03

The colors of the head, tail, and shaft may be individually overridden
using `headColor`, `tailColor`, and `shaftColor`.  More generally, the
styles are controlled using `headStyle`, `tailStyle`, and
`shaftStyle`. For example:

.. class:: lhs

::

> dashedArrow = arrowBetween' (with & arrowHead .~ dart & arrowTail .~ spike'
>                                   & headColor .~ blue & tailColor .~ orange
>                                   & shaftStyle %~ dashing [0.04, 0.02] 0
>                                   . lw 0.01) sPt ePt
>

.. class:: dia

::

> sPt = p2 (0.20, 0.20)
> ePt = p2 (2.95, 0.85)
>
> dot = circle 0.025 # lw 0
> sDot = dot # fc blue # moveTo sPt
> eDot = dot # fc red # moveTo ePt
>
> arrow1 = arrowBetween' (with & arrowHead .~ dart & arrowTail .~ spike'
>                              & headColor .~ blue & tailColor .~ orange
>                              & shaftStyle %~ dashing [0.04, 0.02] 0 . lw 0.01
>                              ) sPt ePt
>
> example = (sDot <> eDot <> arrow1) # centerXY # pad 1.1

Note that when setting a style, one must generally use the `%~`
operator in order to apply something like `dashing [0.04, 0.02] 0`
which is a *function* that changes the style.

.. container:: warning

  By default, the ambient line color is used for the head, tail, and
  shaft of an arrow.  However, when setting the styles individually,
  the fill color should be used for the head and tail, and line color
  for the shaft.  This issue can be avoided entirely by using, for
  example, `headColor .~ blue` to set the color instead of `headStyle
  %~ fc blue`.

Placing an arrow at a point
===========================

Sometimes we prefer to specify a starting point and vector from which the arrow
takes its magnitude and direction. The `arrowAt'` and
`arrowAt` functions are useful in this regard. The example below demonstrates
how we might create a vector field using the `arrowAt'` function.

.. class:: dia-lhs

::

> locs   = [(x, y) | x <- [0.1, 0.3 .. 3.25], y <- [0.1, 0.3 .. 3.25]]
>
> -- create a list of points where the vectors will be place.
> points = map p2 locs
>
> -- The function to use to create the vector field.
> vectorField (x, y) = r2 (sin (y + 1), sin (x + 1))
>
> arrows = map arrowAtPoint locs
>
> arrowAtPoint (x, y) = arrowAt' opts (p2 (x, y)) (sL *^ vf) # alignTL
>   where
>     vf   = vectorField (x, y)
>     m    = magnitude $ vectorField (x, y)
>
>     -- Head size is a function of the length of the vector
>     -- as are tail size and shaft length.
>     hs   = 0.08 * m
>     sW   = 0.015 * m
>     sL   = 0.01 + 0.1 * m
>     opts = (with & arrowHead .~ spike & headSize .~ hs & shaftStyle %~ lw sW)
>
> field   = position $ zip points arrows
> example = ( field # translateY 0.05
>        <> ( square 3.5 # fc whitesmoke # lw 0.02 # alignBL))
>         # scaleX 2

Your turn:

.. container:: exercises

  Try using the above code to plot some other interesting vector fields.

Connecting diagrams with arrows
===============================

The workhorse of the Arrow package is the `connect'`
function. `connect'` takes an opts record and the names of two
diagrams, and places an arrow starting at the origin of the first
diagram and ending at the origin of the second (unless gaps are
specified).

.. class:: dia-lhs

::

> s  = square 2 # showOrigin # lw 0.02
> ds = (s # named "1") ||| strutX 3 ||| (s # named "2")
> t  = cubicSpline False (map p2 [(0, 0), (1, 0), (1, 0.2), (2, 0.2)])
>
> example = ds # connect' (with & arrowHead .~ dart & headSize .~ 0.6
>                               & tailSize .~ 0.6 & arrowTail .~ dart'
>                               & shaftStyle %~ lw 0.03 & arrowShaft .~ t) "1" "2"

Connecting points on the trace of diagrams
==========================================

It is often convenient to be able to connect the points on the `Trace`
of diagrams with arrows. The `connectPerim'` and `connectOutside'`
functions are used for this purpose. We pass `connectPerim` two names
and two angles. The angles are used to determine points on the traces
of the two diagrams, determined by shooting a ray from the local
origin of each diagram in the direction of the given angle.  The
generated arrow stretches between these two points. Note that if the
names are the same then the arrow connects two points on the same
diagram.

In the case of `connectOutside` The arrow lies on the line between the centers of the diagrams, but is drawn so that it stops at the boundaries of the diagrams, using traces to find the intersection points.

.. class:: lhs

::

> connectOutside "diagram1" "diagram2"
> connectPerim "diagram" "diagram" (2/12 @@ turn) (4/12 @@ turn)

Here is an example of a finite state automata that accepts real numbers.
The code is a bit longer than what we have seen so far, but still very
straightforward.

.. class:: dia-lhs

::

> import Data.Maybe (fromMaybe)
>
> state = circle 1 # lw 0.05 # fc silver
> fState = circle 0.85 # lw 0.05 # fc lightblue <> state
>
> points = map p2 [ (0, 3), (3, 4), (6, 3), (6, 6), (9, 4), (12, 3)
>                 , (12, 6), (3, 0), (1.75, 1.75), (6, 1), (9, 0), (12.25, 0)]
>
> ds = [ (text "1" <> state)  # named "1"
>        , label "0-9" 0.5
>        , (text "2" <> state)  # named "2"
>        , label "0-9" 0.5
>        , label "." 1
>        , (text "3" <> fState) # named "3"
>        , label "0-9" 0.5
>        , (text "4" <> state)  # named "4"
>        , label "." 1
>        , label "0-9" 0.5
>        , (text "5" <> fState) # named "5"
>        , label "0-9" 0.5]
>
> label txt size = text txt # fontSize size
>
> states = position (zip points ds)
>
> shaft = reverseTrail $ arc (0 @@ turn) (1/6 @@ turn)
> shaft' = reverseTrail $ arc (1/2 @@ turn) (0 @@ turn) # scaleX 0.33
> line = trailFromOffsets [unitX]
>
> arrowStyle1 = (with  & arrowHead  .~ spike  & headSize .~ 0.3
>                      & arrowShaft .~ shaft & shaftStyle %~ lw 0.02)
> arrowStyle2  = (with  & arrowHead  .~ spike & shaftStyle %~ lw 0.02
>                       & arrowShaft .~ shaft' & arrowTail .~ lineTail
>                       & tailColor  .~ black & widths .~ 0.2)
> arrowStyle3  = (with  & arrowHead  .~ spike  & headSize .~ 0.3
>                       & arrowShaft .~ line & shaftStyle %~ lw 0.02)
>
> example = states # connectOutside' arrowStyle1 "1" "2"
>                  # connectOutside' arrowStyle3 "1" "4"
>                  # connectPerim' arrowStyle2 "2" "2"
>                     (4/12 @@ turn) (2/12 @@ turn)
>                  # connectOutside' arrowStyle1 "2" "3"
>                  # connectPerim' arrowStyle2 "3" "3"
>                     (4/12 @@ turn) (2/12 @@ turn)
>                  # connectOutside' arrowStyle1 "4" "5"
>                  # connectPerim' arrowStyle2 "5" "5"
>                     (1/12 @@ turn) (-1/12 @@ turn)

In the following exercise you can try `connectPerim'` for yourself.

.. container:: exercises

  Create a torus (donut) with `16`:math: curved arrows pointing from the
  outer ring to the inner ring at the same angle every `1/16 @@ turn`.

    .. class:: dia

    ::

    > {-# LANGUAGE MultiParamTypeClasses          #-}
    > {-# LANGUAGE FlexibleContexts               #-}
    >
    > bullseye = circle 0.2 # fc orangered
    >                       # lw 0
    >                       # named "bullseye"
    >
    > target = circle 1 # fc gold # named "target"
    >
    > d = bullseye <> target
    >
    > shaft = arc (0 @@ turn) (1/6 @@ turn)
    >
    > connectTarget :: (Renderable (Path R2) b)
    >               =>  Angle -> (Diagram b R2 -> Diagram b R2)
    > connectTarget a = connectPerim' (with & arrowHead .~ thorn & shaftStyle %~  lw 0.01
    >                                       & arrowShaft .~ shaft & headSize .~ 0.18
    >                                       & arrowTail .~ thorn'
    >                                      ) "target" "bullseye" a a
    >
    > angles :: [Angle]
    > angles = map (@@ turn) [0, 1/16 .. 15/16]
    >
    > example = foldr connectTarget d angles
