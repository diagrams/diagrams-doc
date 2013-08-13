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

XXX very quickly want to manipulate points and vectors, to position
and describe your diagrams.  For example `fromOffsets` and
`fromVertices` take lists of vectors and lists of points,
respectively;

.. container:: todo

  Other examples

.. container:: todo

  Add reference(s) to this tutorial from the user manual

Vectors
=======

Vectors are based on the `vector-space`:pkg: package.  In two
dimensions, you can think of a vector as a pair of coordinates,
representing *displacements* in the $x$ and $y$ directions.

.. container:: todo

  Picture of a vector

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

.. container:: todo

  Where to put this?

  (In particular,
  `vector-space`:pkg: provides sophisticated representation of linear
  transformations---used in the internal representation of
  `Transformation` values---but you don't need to know anything about
  that to work with vectors and points.)

.. container:: todo

  How to add some fun(ish) exercises instead of just imparting a bunch
  of information?  Maybe add some special formatting for exercises!

Constructing vectors
--------------------

.. container:: todo

  * `zeroV`
  * `unitX`, `unitY`, etc.
  * Using `&`  (looks nice for writing literal values)
  * Using `r2` (useful if you already have some pairs)
  * `e` (note, useful with `*^` operator -- see below.  Name is sort
    of a pun.)

Destructing vectors
-------------------

.. container:: todo

  * `unr2`, `coords`
  * `direction`
  * `magnitude`

Vector operations
-----------------

.. container:: todo

  * `AdditiveGroup` (note this is where `zeroV` comes from)
      * adding and subtracting vectors

  * `VectorSpace`
      * scalars & scaling

  * Apply transformations etc.

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
