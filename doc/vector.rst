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
> example = ( (vPic ((4 & 0) # rotateBy (1/12)) # centerXY)
>             ||| strutX 0.2 ||| text' 0.5 "y"
>           )
>           === strutY 0.2 === text' 0.5 "x"

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

.. container:: todo

  see `Diagrams.TwoD.Vector`:mod:

Vectors in two dimensions have the type `R2`.  (One can also work with
other vector spaces with any number of dimensions; in this tutorial
we'll stick to the 2D case.)

The first thing to learn is how to *create* values of type
`R2`. There are many options:

* `zeroV` is the zero vector, that is, the vector with zero magnitude
  (and no direction, or perhaps every direction).  `zeroV` is rarely
  useful on its own, but can come in handy *e.g.* as an argument to a
  function expecting a vector input.  (`zeroV` is actually quite a bit
  more general; see the discussion of the `AdditiveGroup` class in the
  `Vector operations`_ section below.)

* `unitX` and `unitY` are the length-one vectors in the positive
  `x`:math: and `y`:math: directions, respectively.  To create a
  length-`l`:math: vector you can apply scaling to `unitX` or `unitY`,
  like `unitX # scale 3` or `3 *^ unitX` (see `Vector operations`_).

  Also, `unit_X` and `unit_Y` are like `unitX` and `unitY` but point
  in the corresponding negative direction.

  .. class:: dia-lhs

  ::

  > example = fromOffsets [unitX, unitY, 2 *^ unit_X, unit_Y] # centerXY

* To create a vector with given :math:`x`- and :math:`y`- components,
  you can use the function `r2 :: (Double,Double) -> R2`:

  .. class:: dia-lhs

  ::

  > example = fromOffsets . map r2 $ [(1,1), (0,3), (-2,1), (-1,-4)]

  As you can see, `r2` is especially useful if you already have pairs
  representing vector components (which is not uncommon if the
  components are coming from some other data source).

* You can also use `(&)` to construct vector literals, like so:

  .. class:: dia-lhs

  ::

  > import Diagrams.Coordinates
  > example = fromOffsets [1 & 1, 0 & 3, (-2) & 1, (-1) & (-4)]

  This can make for convenient and pleasant notation. However, it does
  have some drawbacks, namely:

  * You must explicitly import `Diagrams.Coordinates`:mod: to use
    `(&)`.
  * `(&)` is extremely general so its type is unhelpful.
  * Related to the above, literal vector expressions like `1 & 2` must
    be used in a context where the type can be inferred (or else a
    type annotation must be added).  This is because (as we will see
    later) `(&)` can also be used to construct points as well as
    higher-dimensional vectors.

  Only you can decide whether the tradeoffs are worth it in a given
  situation.

* One final way to construct vectors is using the `fromDirection`
  function (or its synonym `e`).  `fromDirection` takes an angle and
  constructs a unit (*i.e.* magnitude 1) vector pointing in the given
  direction.  This can also be accomplished using `unitX` and `rotate`
  (in particular, `fromDirection a == unitX # rotate a`), but
  sometimes calling `fromDirection` can be more convenient.
  Additionally, the synonym `e` is available as a sort of convenient
  pun: in the same way that a complex number with magnitude `r`:math:
  and angle `\theta`:math: can be constructed as `r
  e^{i\theta}`:math:, a vector with given magnitude and direction can
  be constructed as `r *^ e theta`.

  .. class:: dia-lhs

  ::

  > example = lw 0.05 . mconcat . map (fromOffsets . (:[]))
  >         $ [ r *^ e (Rad r) | r <- [33 * tau/32, 34 * tau/32 .. 2 * tau] ]

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
