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

Most of the functionality discussed in this tutorial can be found in
the `vector-space`:pkg: package and the `Diagrams.TwoD.Vector`:mod:
module.

Vectors
=======

Vectors in ``diagrams`` are based on the `vector-space`:pkg: package.
In two dimensions, you can think of a vector as a pair of coordinates,
representing *displacements* in the `x`:math: and `y`:math:
directions. Alternatively, you can think of a vector as consisting of
a *magnitude* (length) and a *direction* (angle).

.. class:: dia

::

> import Diagrams.Coordinates
>
> drawV v = (    beside unitY (hrule (magnitude v))
>                             (text' 0.5 "r" === strutY 0.2)
>                # alignR
>             <> triangle 0.3 # rotateBy (-1/4) # scaleY (1/2)
>                # fc black # alignR
>           )
>           # alignL
>           # rotateBy (direction v)
>
> vPic v = drawV v <> xComponent <> yComponent <> theta
>   where
>     component u = fromOffsets [project u v]
>                 # dashing [0.05,0.05] 0
>     xComponent = component unitX
>     yComponent = component unitY # translate (project unitX v)
>     theta = text' 0.5 "θ" # translate (0.7 & 0.2)
>
> text' d s = (stroke $ textSVG' (TextOpts s lin INSIDE_H KERN False d d))
>           # lw 0 # fc black
>
> example = ( (vPic ((4 & 0) # rotateBy (1/12)) # centerXY)
>             ||| strutX 0.2 ||| text' 0.5 "y"
>           )
>           === strutY 0.2 === text' 0.5 "x"

One of the most
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

Constructing vectors
--------------------

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
  in the corresponding negative directions.

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

.. container:: exercises

  Construct each of the following images.

  1. .. class:: dia

     ::

     > vs = take 10 $ cycle [unitX # rotateBy (1/8), unitX # rotateBy (-1/8)]
     > example = fromOffsets vs # centerXY

  #. .. class:: dia

     ::

     > vs = [ e (r :: Turn) | r <- [-1/4, -1/4 + 1/12 .. 1/4] ]
     > example = mconcat (map (\v -> circle 0.2 # translate v) vs)
     >         # fc blue
     >         # centerXY

Destructing vectors
-------------------

To take apart a vector into its `x`:math: and `y`:math: components,
use `unr2 :: R2 -> (Double, Double)`, or more generally you can use
`coords` (from `Diagrams.Coordinates`:mod:) and pattern-match on
`(:&)`.  Both these methods work well in conjunction with the
``ViewPatterns`` `GHC extension`__, as in

__ http://ghc.haskell.org/trac/ghc/wiki/ViewPatterns

.. class:: lhs

::

> foo :: R2 -> ...
> foo (unr2 -> (x,y)) = ... x ... y ...

Note, however, that you will probably need this less often than you
think.  Using the vector operations presented in the next section, you
should strive to work on the level of vectors, and only "stoop" to the
level of working with explicit coordinates when absolutely necessary.

To get the magnitude and direction of a vector, you can use the
`magnitude` and `direction` functions.  Additionally, `magnitudeSq`
gives the *squared* magnitude of a vector, and is more efficient than
squaring the result of `magnitude`, since it avoids a `sqrt` call.

Vector operations
-----------------

There is a rich set of combinators for operating on vectors (and we
are open to adding more!).

* Vectors can be transformed with all the usual transformation
  functions like `rotate`, `scale`, and so on.  However, recall that
  although it is possible to apply `translate` to a vector, it has no
  effect.

  .. class:: dia-lhs

  ::

  > example = mconcat $ map (fromOffsets . (:[])) vs
  >   where
  >     vs = take 33 . iterate (scale (2**(1/32)) . rotateBy (1/32)) $ unitX

* `R2` is an instance of the `AdditiveGroup` class (see
  `Data.AdditiveGroup`:mod: from the `vector-space`:pkg: package),
  which is for types with an (additive) group structure.  This means:

  * Vectors can be added with `(^+^)`.

    .. container:: todo

       Explain and illustrate

  * There is a zero vector `zeroV` (mentioned previously), which is
    the identity for `(^+^)`.
  * Vectors can be negated with `negateV`.  The negation of a vector
    ``v`` is the vector with the same magnitude which points in the
    opposite direction, and is the additive inverse of ``v``: that is,
    `v ^+^ negateV v == zeroV`.

  `Data.AdditiveGroup`:mod: also defines a few other methods which can
  be used on vectors, including `(^-^)` (vector subtraction) and
  `sumV` (summing an entire list or other `Foldable` container of
  vectors).

* `R2` is also an instance of the `VectorSpace` class (see
  `Data.VectorSpace`:mod: from the `vector-space`:pkg: package).
  Significantly, this class defines an associated type family called
  `Scalar`; the `Scalar` type associated to a `VectorSpace` can be
  thought of as representing *distances* or *scaling
  factors*. In particular `Scalar R2` is defined to be `Double`.

  You can multiply (scale) a vector by a `Scalar` using `(*^)` (which
  takes a `Scalar` on the left and a vector on the right) or `(^*)`
  (which is `(*^)` with the arguments reversed).  (Note that
  `vector-space`:pkg: operators always use ``^`` in their names to
  indicate a vector argument, as in `(*^)` (scalar times vector) and
  `(^+^)` (vector plus vector) and `(.+^)` (point plus vector, as we
  will see later.)

  Note that using `(*^)` is equivalent to using `scale`; that is, `s
  *^ v == v # scale s`.  There is also a `(^/)` operator provided for
  convenience which divides a vector by a scalar; of course `v ^/ s ==
  v ^* (1/s)`.

* Finally, `R2` is an instance of the `InnerSpace` class (also in
  `Data.VectorSpace`:mod:).

.. container:: todo

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

.. container:: todo

  * `unp2`, `coords`
  * Do we have a `distance` function?

Point operations
----------------

.. container:: todo

  * `AffineSpace`.
      * `Diff` type function
      * subtract two points to get a vector
      * point + vector.
  * Apply transformations etc.
