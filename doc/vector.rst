.. role:: pkg(literal)
.. role:: hs(literal)
.. role:: mod(literal)
.. role:: repo(literal)

.. default-role:: hs

==================
Vectors and points
==================

.. contents::

Introduction
============

After working with ``diagrams`` for a while, you very quickly end up
needing to manipulate points and vectors in order to position and
describe your diagrams.  For example, `fromOffsets` and `fromVertices`
take lists of vectors and lists of points, respectively;

.. container:: todo

  Other examples

.. container:: todo

  Add reference(s) to this tutorial from the user manual

Vectors
=======

Vectors in ``diagrams`` are based on the `vector-space`:pkg: package.
In two dimensions, you can think of a vector as a pair of coordinates,
representing *displacements* in the `x`:math: and `y`:math: directions.

.. class:: dia

::

> import Diagrams.Coordinates
>
> drawV v = (    hrule (magnitude v) # alignR
>             <> triangle 0.3 # rotateBy (-1/4) # scaleY (1/2)
>                # fc black # alignR
>           )
>           # alignL
>           # rotateBy (direction v)
>
> vPic v = drawV v <> xComponent <> yComponent
>   where
>     component u = fromOffsets [project u v]
>                 # dashing [0.05,0.05] 0
>     xComponent = component unitX
>     yComponent = component unitY # translate (project unitX v)
>
> text' d s = (stroke $ textSVG' (TextOpts s lin2 INSIDE_H KERN False d d))
>           # lw 0 # fc black
>
> example = (vPic ((4 & 0) # rotateBy (1/12)) # centerXY)
>       ||| text' 0.5 "y"

Alternatively, you can think of a vector as consisting of a
*magnitude* (length) and a *direction* (angle).  One of the most
important things to understand about vectors is that they are
*translation-invariant*: that is, they have no specific location in
space, and are unaffected by translations (though they are affected by
other sorts of transformation such as scaling and rotation).  You can
see this for yourself at a ``ghci`` prompt:

::

  >>> (3 & 6) :: R2
  3.0 & 6.0
  >>> translateX 19 (3 & 6) :: R2
  3.0 & 6.0
  >>> rotateBy (1/4) (3 & 6) :: R2
  (-6.0) & 3.0000000000000004

.. container:: todo

  Where to put this?

  (In particular,
  `vector-space`:pkg: provides sophisticated representation of linear
  transformations---used in the internal representation of
  `Transformation` values---but you don't need to know anything about
  that to work with vectors and points.)

Constructing vectors
--------------------

see `Diagrams.TwoD.Vector`:mod:

Vectors in two dimensions have the type `R2`.  (One can also work with
other vector spaces with any number of dimensions; in this tutorial
we'll stick to the 2D case.)

The first thing to learn is how to *create* values of type
`R2`. Towards this end there are quite a few options:

* `zeroV` is the zero vector, that is, the vector with zero magnitude
  (and no direction, or perhaps every direction).  `zeroV` is rarely
  useful on its own, but can come in handy *e.g.* as an argument to a
  function expecting a vector input.  (`zeroV` is actually quite a bit
  more general; see the discussion of the `AdditiveGroup` class in the
  `Vector operations`_ section below.)

* `unitX` and `unitY` are the length-one vectors in the positive
  `x`:math: and `y`:math: directions, respectively.

.. container:: todo

  * `zeroV`
  * `unitX`, `unitY`, etc.
  * Using `&`  (looks nice for writing literal values)
  * Using `r2` (useful if you already have some pairs)
  * `fromDirection`, `e` (note, useful with `*^` operator -- see
    below.  Name is sort of a pun.)

Destructing vectors
-------------------

.. container:: todo

  * `unr2`, `coords`
  * `direction`
  * `magnitude`
  * `magnitudeSq`

Vector operations
-----------------

.. container:: todo

  * Apply transformations etc.

  * `AdditiveGroup` (note this is where `zeroV` comes from)
      * adding and subtracting vectors

  * `VectorSpace`
      * scalars & scaling

  * `InnerSpace`

  * `normalized`
  * `lerp`
  * `project`
  * `perp`
  * `leftTurn`

.. container:: exercises

  1. Write a function `vTriangle :: R2 -> R2 -> Diagram SVG R2`
     (substituting your favorite backend in place of `SVG`) which
     takes as arguments two vectors representing two sides of a
     triangle and draws the corresponding triangle.  For example,
     `vTriangle unitX (unitX # rotateBy (1/8))` should produce

     .. class:: dia

     ::

     > vTriangle v1 v2 = fromOffsets [v1, v2 ^-^ v1, (-1) *^ v2]
     >                 # glueLine # strokeLoop
     >
     > example = vTriangle unitX (unitX # rotateBy (1/8))
     >         # centerXY # pad 1.1

  #. Bar

Points
======

.. container:: todo

  General remarks about points.  `vector-space-points`:pkg: package.
  They *are* affected by translation.

Constructing points
-------------------

.. container:: todo

  * `&` as before
  * `p2`
  * (Intentionally) no way to directly convert a vector into a point.
    You probably don't want to do that anyway. (If you want, see
    below; also note `unsafe` function(s) from vsp package??)

  * Advanced: use any function returning `TrailLike` to get a list of
    vertices!  Turn this into an exercise...?

Destructing points
------------------

.. container::

  * `unp2`, `coords`
  * Do we have a `distance` function?

Point operations
----------------

  * `AffineSpace`.
      * `Diff` type function
      * subtract two points to get a vector
      * point + vector.
  * Apply transformations etc.
