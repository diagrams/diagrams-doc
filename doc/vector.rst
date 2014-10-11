.. role:: pkg(literal)
.. role:: hs(literal)
.. role:: mod(literal)
.. role:: repo(literal)

.. default-role:: hs

.. contents::

Introduction
============

After working with ``diagrams`` for a while, you very quickly end up
needing to manipulate points and vectors in order to position and
describe your diagrams.  For example, `fromOffsets` and `fromVertices`
take lists of vectors and lists of points, respectively; `beside` and
`translate` each take a vector as an argument; `position` expects
objects paired with points; and so on.

This tutorial will walk you through everything you need to know about
creating and working with vectors and points, with examples and
exercises to deepen your understanding.  If you notice any typos or
bugs, are confused, or have an idea for extending or enhancing this
tutorial, please `open a ticket`__!

__ https://github.com/diagrams/diagrams-doc/issues

Solutions to the exercises can be found in the source code for this
tutorial, in the `diagrams-doc`:repo: repository.  Note, however, that
many of the exercises have multiple good solutions.

Vectors
=======

Vectors in ``diagrams`` are based on the `linear`:pkg: package.
In two dimensions, you can think of a vector as a pair of coordinates,
representing *displacements* in the `x`:math: and `y`:math:
directions. Alternatively, you can think of a vector as consisting of
a *magnitude* (length) and a *direction* (angle).

.. class:: dia

::

> drawV v = (    beside unitY (hrule (norm v))
>                             (text' 0.5 "r" === strutY 0.2)
>                # alignR
>             <> triangle 0.3 # rotateBy (-1/4) # scaleY (1/2)
>                # fc black # alignR
>           )
>           # alignL
>           # rotate (angleBetween v unitX)
>
> vPic v = drawV v <> xComponent <> yComponent <> theta
>   where
>     component u = fromOffsets [project u v]
>                 # dashingG [0.05,0.05] 0
>     xComponent = component unitX
>     yComponent = component unitY # translate (project unitX v)
>     theta = text' 0.5 "θ" # translate (0.7 ^& 0.2)
>
> text' d s = (stroke $ textSVG' (TextOpts s lin INSIDE_H KERN False d d))
>           # lwG 0 # fc black
>
> example = ( (vPic ((4 ^& 0) # rotateBy (1/12)) # centerXY)
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

  >>> (3 ^& 6) :: V2 Double
  3.0 ^& 6.0
  >>> translateX 19 (3 ^& 6) :: V2 Double
  3.0 ^& 6.0
  >>> rotateBy (1/4) (3 ^& 6) :: V2 Double
  (-6.0) ^& 3.0000000000000004

Constructing vectors
--------------------

Vectors in two dimensions have the type `V2 n`.  (One can also work with
other vector spaces with any number of dimensions; in this tutorial
we'll stick to the 2D case.)

The first thing to learn is how to *create* values of type `V2 n`.
There are many options:

* `zero` is the zero vector, that is, the vector with zero magnitude
  (and no direction, or perhaps every direction).  `zero` is rarely
  useful on its own, but can come in handy *e.g.* as an argument to a
  function expecting a vector input.

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
  you can use the function `r2 :: (n, n) -> V2 n`:

  .. class:: dia-lhs

  ::

  > example = fromOffsets . map r2 $ [(1,1), (0,3), (-2,1), (-1,-4)]

  As you can see, `r2` is especially useful if you already have pairs
  representing vector components (which is not uncommon if the
  components are coming from some other data source).

* You can also use the data constructor `V2`:

  .. class:: dia-lhs

  ::

  > example = fromOffsets [V2 1 1, V2 0 3, V2 (-2) 1, V2 (-1) (-4)]

* You can also use `(^&)` to construct vector literals, like so:

  .. class:: dia-lhs

  ::

  > example = fromOffsets [1 ^& 1, 0 ^& 3, (-2) ^& 1, (-1) ^& (-4)]

  This can make for convenient and pleasant notation. However, it does
  have some drawbacks, namely:

  * `(^&)` is extremely general so its type is unhelpful.
  * Related to the above, literal vector expressions like `1 ^& 2` must
    be used in a context where the type can be inferred (or else a
    type annotation must be added).  This is because (as we will see
    later) `(^&)` can also be used to construct points as well as
    higher-dimensional vectors.

  Only you can decide whether the tradeoffs are worth it in a given
  situation.

* You can construct vectors from `Direction`s using the
  `fromDirection` function.  `fromDirection` takes a `Direction` and
  constructs a unit (*i.e.* magnitude 1) vector pointing in the given
  direction.

* One final way to construct vectors is using the function `e`.  By
  definition, `e a == unitX # rotate a`, but sometimes calling `e`
  can be more convenient.  The name `e` is a sort of pun: in the same
  way that a complex number with magnitude `r`:math: and angle
  `\theta`:math: can be constructed as `r e^{i\theta}`:math:, a vector
  with given magnitude and direction can be constructed as `r *^ e
  theta`. (Note that `e` is not exported from `Diagrams.Prelude`:mod:;
  if you wish to use it you must import it from
  `Diagrams.TwoD.Vector`:mod:.)

  .. class:: dia-lhs

  ::

  > example = lwG 0.05 . mconcat . map fromOffsets
  >         $ [ [r *^ rotate (r @@ rad) unitX]
  >           | r <- [33 * tau/32, 34 * tau/32 .. 2 * tau]
  >           ]

.. container:: exercises

  Construct each of the following images.

  1. .. class:: dia

     ::

     > vs = take 10 $ cycle [unitX # rotateBy (1/8), unitX # rotateBy (-1/8)]
     > example = fromOffsets vs # centerXY

  #. The circles have radius 1, and are arranged in the shape of a
     radius-5 semicircle.

     .. class:: dia

     ::

     > vs = [ 5 *^ rotate (r @@ turn) unitX | r <- [-1/4, -1/4 + 1/12 .. 1/4] ]
     > example = mconcat (map (\v -> unitCircle # translate v) vs)
     >         # fc blue
     >         # centerXY

  #. 30 spokes with lengths 1, 2, and 3.

     .. class:: dia

     ::

     > vs = zipWith mkV (cycle [1,2,3]) [ 1/30 @@ turn, 2/30 @@ turn .. 1 @@ turn ]
     >   where mkV r th = r *^ rotate th unitX
     > example = lwG 0.02 . mconcat . map (fromOffsets . (:[])) $ vs

Destructing vectors
-------------------

To take apart a vector into its `x`:math: and `y`:math: components,
use `unr2 :: V2 n -> (n, n)`, or more generally you can use
`coords` (from `Diagrams.Coordinates`:mod:) and pattern-match on
`(:^&)`.  Both these methods work well in conjunction with the
``ViewPatterns`` `GHC extension`__, as in

__ http://ghc.haskell.org/trac/ghc/wiki/ViewPatterns

.. class:: lhs

::

> foo :: V2 n -> ...
> foo (unr2 -> (x,y)) = ... x ... y ...

Note, however, that you will probably need this less often than you
think.  Using the vector operations presented in the next section, you
should strive to work on the level of vectors, and only "stoop" to the
level of working with explicit coordinates when absolutely necessary.

To get the magnitude and direction of a vector, you can use the
`magnitude` and `direction` functions.  To get the angle between two
given vectors, use `angleBetween`.  Additionally, `quadrance` gives
the *squared* magnitude of a vector, and is more efficient than
squaring the result of `norm`, since it avoids a `sqrt` call.
For example, if you want to test which of two vectors is longer, you
can compare the results of `quadrance` instead of `norm` (since
`a < b \iff a^2 < b^2`:math: as long as `a`:math: and `b`:math: are
nonnegative).

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

  > example = mconcat $ map fromOffsets ls
  >   where
  >     vs = take 33 . iterate (scale (2**(1/32)) . rotateBy (1/32))
  >        $ unitX
  >     ls = [[x] | x <- vs]

* `V2` is an instance of the `Additive` class (see
  `Linear.Additive`:mod: from the `linear`:pkg: package). This means:

  * Vectors can be added with `(^+^)`.  To add two vectors, think of
    placing them head-to-tail; the result of the addition is the
    vector from the tail of the first vector to the head of the
    second.
  * There is a zero vector `zero` (mentioned previously), which is
    the identity for `(^+^)`.
  * Vectors can be negated with `negated`.  The negation of a vector
    ``v`` is the vector with the same magnitude which points in the
    opposite direction, and is the additive inverse of ``v``: that is,
    `v ^+^ negated v == zero`.

  `Linear.Additive`:mod: also defines a few other methods which can
  be used on vectors, including `(^-^)` (vector subtraction) and
  `sumV` (summing an entire list or other `Foldable` container of
  vectors).

* `V2` is also an instance of the `Functor` class (see
  `Data.Functor`:mod: from the `base`:pkg:). The `(*^)` operator uses
  this class to multiply all components of a vector by a scalar. In
  particular for `Num n => V2 n` we have `(*^) :: n -> V2 n -> V2 n`.
  (Note that `linear`:pkg: operators always use ``^`` in their
  names to indicate a vector argument, as in `(*^)` (scalar times
  vector) and `(^+^)` (vector plus vector) and `(.+^)` (point plus
  vector, as we will see later.)

  Note that using `(*^)` is equivalent to using `scale`; that is, `s
  *^ v == v # scale s`.  There is also a `(^/)` operator provided for
  convenience which divides a vector by a scalar; of course `v ^/ s ==
  v ^* (1/s)`.

* Finally, `R2` is an instance of the `Metric` class (also in
  `linear`:mod:), which provides the *inner product* (also
  called *dot product*) function, `dot`.  The definition and
  properties of the dot product are beyond the scope of this tutorial;
  you can `read about it on Wikipedia`__.  However, note that several
  common uses of the dot product are already encapsulated in other
  functions, such as `project` and `leftTurn`.

__ http://en.wikipedia.org/wiki/Dot_product

* The `normalized` function changes the magnitude of a vector to
  `1`:math:, while keeping the direction fixed.

* `perp` yields a vector perpendicular to (and of the same magnitude
  as) its input.

* `lerp` linearly interpolates between two vectors as the given
  parameter varies from `0`:math: to `1`:math:.

* `leftTurn v1 v2` tests whether the direction of `v2` is a "left
  turn" from `v1` (that is, if the direction of `v2` can be obtained
  from that of `v1` by rotating up to one-half turn in the positive
  direction).

* `project u v` computes the *projection* of `v` onto `u`.  In the
  illustration below, the green line shows the projection of the red
  vector onto the blue vector.

  .. class:: dia-lhs

  ::

  > u = r2 (1,2)
  > v = 2 *^ (unitY # rotateBy (1/19))
  > p = project u v
  >
  > drawV v = fromOffsets [v]
  >
  > example = mconcat
  >   [ drawV p # lc green # lwG 0.03
  >   , drawV u # lc blue
  >   , drawV v # lc red
  >   , drawV (p ^-^ v) # translate v # dashingG [0.1,0.1] 0
  >   ]

.. container:: exercises

  1. Write a function `vTriangle :: V2 Double -> V2 Double -> Diagram SVG V2 Double`
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

  #. Write a function which takes two vectors as input and constructs
     a classic illustration of vector addition using a parallelogram,
     as in the following example:

     .. class:: dia

     ::

     > drawV = fromOffsets . (:[])
     > vAddVis v1 v2
     >   = mconcat
     >     [ drawV v1 # lc red
     >     , drawV v2 # lc blue
     >	   , drawV v1 # lc red  # dashingG [0.1,0.1] 0 # translate v2
     >     , drawV v2 # lc blue # dashingG [0.1,0.1] 0 # translate v1
     >     , drawV (v1 ^+^ v2) # lc purple
     >     ]
     >
     > example = vAddVis (r2 (0.5,1)) (r2 (2,0.5)) # lwG 0.02

Using vectors
-------------

Once you have a vector, what can you do with it?  A few of the things
have already been seen in the examples above, but it's worth
collecting a list here in one place.

* You can create a trail, path, or diagram (in fact, any `TrailLike`
  thing---see the `trails and paths tutorial`__) from a list of
  vectors using `fromOffsets`.

  __ paths.html

* You can translate things by a vector using `translate` or
  `moveOriginBy`.

Points
======

A *point* is a location in space.  In ``diagrams``, points are based
on the `linear`:pkg: package, and in the case of 2D are represented by
the type alias `P2 = Point V2`. In 2D, points are usually thought of
as a pair of `x`:math: and `y`:math: coordinates (though other
coordinate systems could be used as well, *e.g.* polar coordinates).

Points and vectors are closely related, and are sometimes conflated
since both can be concretely represented by tuples of coordinates.
However, they are distinct concepts which support different sets of
operations. For example, points are affected by translation whereas
vectors are not and so on.  Hence, they are represented by distinct
types in ``diagrams``.

Constructing points
-------------------

There are several ways to construct points.

* `origin` is the name of the distinguished point at the origin of
   the vector space (note this works in any dimension).

* To create a point with given :math:`x`- and :math:`y`- components,
  you can use the function `p2 :: (n,n) -> Point V2 n`:

  .. class:: dia-lhs

  ::

  > example
  >   = position . flip zip (repeat (circle 0.2 # fc green))
  >   . map p2 $ [(1,1), (0,3), (-2,1), (-1,-4), (2,0)]

  As with `r2`, `p2` is especially useful if you already have pairs
  representing point coordinates.

* The `^&` operator can be used to construct literal points (`P2 n`
  values) as well as vectors (`V2 n` values).  The proper type is chosen
  via type inference: if the expression `(3 ^& 5)` is used in a context
  where its type is inferred to be `P2 n`, it is the point at
  `(3,5)`:math:; if its type is inferred to be `V2 n`, it is the vector
  with `x`:math:-component `3`:math: and `y`:math:-component
  `5`:math:.

* There is no way to directly convert a vector into a point (unless
  you use the `P` type constructor from `Linear.Affine`)---this is
  intentional!  If you have a vector `v` and you want to refer to the
  point located at the vector's head (when the vector tail is placed
  at, say, the origin) you can write `origin .+^ v` (see below for a
  discussion of `.+^`).

* An advanced method of generating points is to use any function
  returning a `TrailLike` result, since `[Point V2 Double]` is an instace of
  `TrailLike`. Using a function returning any `TrailLike` at the
  result type `[Point V2 Double]` will result in the list of vertices of the trail.
  For example, here we obtain the list of vertices of a regular
  nonagon:

  .. class:: dia-lhs

  ::

  > pts :: [P2 Double]
  > pts = nonagon 1
  > example = position . map (\p -> (p, circle 0.2 # fc green)) $ pts

  Note that we could also inline `pts` in the above example to obtain

  .. class:: lhs

  ::

  > example = position . map (\p -> (p, circle 0.2 # fc green)) $ nonagon 1

  In this case, the type of `nonagon 1` would be inferred as `[P2 Double]`
  (since `position` expects a list of paired points and diagrams), causing the
  appropriate `TrailLike` instance to be
  chosen.

Destructing points
------------------

For taking a point apart into its components you can use the `unp2`
function, or, more generally, `coords` (just as with vectors).  There
is currently no way to get a polar representation of a point, but it
would be easy to add: if you want it, holler (or `submit a pull
request`__!).

__ http://www.haskell.org/haskellwiki/Diagrams/Contributing

You can compute the distance between two points with the `distance`
function (or `quadrance` to get the square of the distance, which
avoids a square root).

.. container:: exercises

  Construct each of the following images.

  1. A `31 \times 31`:math: grid of circles, each colored according to
     the distance of its center from the origin.

     .. class:: dia

     ::

     > example
     >   = pts
     >   # map (hcat . map mkSquare)
     >   # vcat
     >   # centerXY
     >
     > r = 15
     >
     > pts = [ [p2 (x,y) | x <- [-r .. r]] | y <- [-r .. r]]
     > mkSquare p = circle 0.5 # fc c # moveTo p
     >   where
     >     c | distance p origin <= r = yellow
     >       | otherwise              = purple

Point operations
----------------

You can transform points arbitrarily: unlike vectors, points are
affected by translation.  Rotation and scaling act on points with
respect to the origin (for example, scaling the point `(1,1)`:math: by
`2`:math: results in the point `(2,2)`:math:).

.. class:: dia-lhs

::

> sqPts = square 1
>
> drawPts pts c = pts # map (\p -> (p,dot' c)) # position
> dot' c = circle 0.2 # fc c
>
> example = drawPts sqPts blue
>        <> drawPts (sqPts # scale 2 # rotateBy(1/10)) red

Abstractly, points and vectors together form what is termed an "affine
space". Here is a nice intuitive description of affine spaces, stolen
from `the wikipedia page`__:

__ http://en.wikipedia.org/wiki/Affine_space

    An affine space is what is left of a `vector space`_ after you've
    forgotten which point is the origin (or, in the words of the
    French mathematician `Marcel Berger`_, "An affine space is nothing
    more than a vector space whose origin we try to forget about, by
    adding translations to the linear maps").

.. _`vector space`: http://en.wikipedia.org/wiki/Vector_space
.. _`Marcel Berger`: http://en.wikipedia.org/wiki/Marcel_Berger

It's not important to understand the formal mathematical
definition of an affine space; it's enough to understand the sorts of
operations which this enables on points and vectors.

In particular, `P2` is an instance of the `Affine` type class
(defined in `Linear.Affine`:mod: from the `linear`:pkg: package).
This class also has an associated type family called `Diff`, which for
`P2` is defined to be `V2`: roughly, this says that the *difference*
or "offset" between two points is given by a vector.

Note how the operators below are named: a period indicates a point
argument, and a carat (`^`) indicates a vector argument.  So, for
example, `(.+^)` takes a point as its first argument and a vector as
its second.

* You can "subtract" one point from another to get the vector between
  them, using `(.-.)`.  In particular `b .-. a` is the vector
  pointing from `a` to `b`.

* Using `(.+^)`, you can add a vector to a point, resulting in another
  point which is offset from the first point by the given vector.  If
  `p .+^ v == p'`, then `p' .-. p == v`.  You can also use `(.-^)` to
  subtract a vector from a point.

* Although it is not semanticly correct, `Point` is an instance of
  `Additive`. This means you can *linearly interpolate* between two
  points using `lerp`, which does make sense. For example, to find the
  point which is 25% of the way from the first point to the second. 

  .. class:: dia-lhs

  ::

  > pt1 = origin
  > pt2 = p2 (5,3)
  >
  > example = position $
  >   [ (p, circle 0.2 # fc c)
  >   | a <- [0, 0.1 .. 1]
  >   , let p = lerp a pt2 pt1
  >   , let c = blend a blue green
  >   ]

* You can find the *centroid* (the "average" or "center of mass") of a
  list of points using the `centroid` function (defined in
  `Diagrams.Points`:mod:).

* Finally, you can scale a point using the `(*^)` operator (though, as
  mentioned earlier, you can also use `scale`).

.. container:: exercises

  1. Implement the `Graham scan algorithm`__ and generate diagrams
     illustrating the intermediate steps.

  __ http://en.wikipedia.org/wiki/Graham_scan

Using points
------------

Here are some things you can do with points, once you have constructed
or computed them:

* You can create a straight line between two points with `(~~)`.

* You can construct any `TrailLike` instance (like trails, paths, or
  diagrams) from a list of points using `fromVertices`.

* You can translate objects to a given point using `moveTo`, `place`,
  or `moveOriginTo`.

* You can position an entire collection of objects using `position`.
