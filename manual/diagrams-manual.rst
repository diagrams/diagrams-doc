.. role:: pkg(literal)
.. role:: hs(literal)
.. role:: mod(literal)

.. default-role:: hs

====================
Diagrams User Manual
====================

Preliminaries
=============

Introduction
------------

Installation
------------

Getting started
---------------

Tutorial
========

Important concepts
==================

Local vector spaces
-------------------

Bounding functions
------------------

Monoids
-------

Creating 2D diagrams
====================

The main purpose of ``diagrams`` is to construct two-dimensional
vector graphics, although it can be used for more general purposes as
well.  This section explains the building blocks provided by
`diagrams-core`:pkg: and `diagrams-lib`:pkg: for constructing
two-dimensional diagrams:

* `Basic 2D types`_
* `Primitive shapes`_
* Methods for `Combining`_ and `Modifying diagrams`_
* `Working with paths`_
* Working with `Text`_
* Working with `Named subdiagrams`_

.. _Combining: `Combining diagrams`_

All 2D-specific things can be found in `Diagrams.TwoD`:mod:, which
re-exports most of the contents of ``Diagrams.TwoD.*`` modules.  This
section also covers many things which are not specific to two
dimensions; later sections will make clear which are which.

Basic 2D types
--------------

`Diagrams.TwoD.Types`:mod: defines types for working with
two-dimensional Euclidean space and with angles.

Euclidean 2-space
~~~~~~~~~~~~~~~~~

There are three main types synonyms defined for referring to
two-dimensional space:

* `R2` is the type of the two-dimensional Euclidean vector space.  It
  is a synonym for `(Double, Double)`.
* `P2` is the type of points in two-dimensional space. It is a synonym
  for `Point R2`.
* `T2` is the type of two-dimensional affine transformations.  It is a
  synonym for `Transformation R2`.

XXX note re: vectors vs points.

Angles
~~~~~~

The `Angle` type class classifies types which measure two-dimensional
angles.  Three instances are provided by default (you can, of course,
also make your own):

* `CircleFrac` represents fractions of a circle.  A value of `1`
  represents a full turn.
* `Rad` represents angles measured in radians.  A value of `tau` (that
  is, `2 * pi`) represents a full turn. (If you don't know what `tau`
  is, see `The Tau Manifesto`__.)
* `Deg` represents angles measured in degrees.  A value of `360`
  represents a full turn.

__ http://tauday.com

The intention is that to pass an argument to a function that expects a
value of some `Angle` type, you can write something like `(3 :: Deg)`
or `(3 :: Rad)`.  The `convertAngle` function is also provided for
converting between different angle representations.

Primitive shapes
----------------

`diagrams-lib`:pkg: provides many standard two-dimensional shapes for
use in constructing diagrams.

Circles and ellipses
~~~~~~~~~~~~~~~~~~~~

Circles can be created with the `unitCircle` and `circle`
functions, defined in `Diagrams.TwoD.Ellipse`:mod:.

For example,

.. codeblock:: dia-lhs

   > example = circle 0.5 <> unitCircle

`unitCircle` creates a circle of radius 1 centered at the
origin; `circle` takes the desired radius as an argument.

Every ellipse is the image of the unit circle under some affine
transformation, so ellipses can be created by appropriately `scaling
and rotating`__ circles.

.. codeblock:: dia-lhs

   > example = unitCircle # scaleX 0.5 # rotateBy (1/6)

For convenience the standard library also provides `ellipse`, for
creating an ellipse with a given eccentricity, and `ellipseXY`, for
creating an axis-aligned ellipse with specified radii in the x and y
directions.

__ `2D Transformations`_

Arcs
~~~~

`Diagrams.TwoD.Arc`:mod: provides a function `arc` for constructing
circular arcs.

XXX write me

Squares and rectangles
~~~~~~~~~~~~~~~~~~~~~~

`Diagrams.TwoD.Shapes`:mod:

XXX write me

Polygons
~~~~~~~~

Combining diagrams
------------------

The ``diagrams`` framework is fundamentally *compositional*: complex
diagrams are created by combining simpler diagrams in various ways.

Superimposing diagrams with `atop`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

[XXX should this actually go somewhere more general?  `atop` is not
just for 2D diagrams.  Or maybe the general section should talk about
the diagrams monoid instance, and atop should just be for 2D?  Or
maybe we should just start by talking about 2D, and later show how it
generalizes?]

The most fundamental way to combine two diagrams is to place one on
top of the other with `atop`.  The diagram `d1 \`atop\` d2` is formed
by placing `d1`'s local origin on top of `d2`'s local origin; that is,
by identifying their local vector spaces.  

As noted before, diagrams form a monoid_
with composition given by identification of vector spaces.  `atop` is
simply a synonym for `mappend` (or `(<>)`) , specialized to two
dimensions.  (XXX more here about how it is commutative in higher
dimensions?)

.. _monoid: Monoids_

This also means that a list of diagrams can be stacked with `mconcat`;
that is, `mconcat [d1, d2, d3, ...]` is the diagram with `d1` on top
of `d2` on top of `d3` on top of...

Juxtaposing diagrams
~~~~~~~~~~~~~~~~~~~~

Fundamentally, `atop` is actually the *only* way to compose diagrams;
however, there are a number of other combining methods (all ultimately
implemented in terms of `atop`) provided for convenience.

Two diagrams can be placed *next to* each other in a certain direction
using `beside`. 

Concatenating diagrams
~~~~~~~~~~~~~~~~~~~~~~

Modifying diagrams
------------------

2D Transformations
~~~~~~~~~~~~~~~~~~

Alignment
~~~~~~~~~

Attributes and styles
~~~~~~~~~~~~~~~~~~~~~

Working with paths
------------------

Segments
~~~~~~~~

Trails
~~~~~~

Paths
~~~~~

The `PathLike` class
~~~~~~~~~~~~~~~~~~~~~~

Text
----

Named subdiagrams
-----------------

Bounding boxes
--------------

Tools for backends
------------------

Core library
============

Vector spaces
-------------

The V type function
~~~~~~~~~~~~~~~~~~~

Points and vectors
~~~~~~~~~~~~~~~~~~

Transformations
---------------

Bounding functions
------------------

Queries
-------

Styles
------

Names
-----

UD-Trees
--------

Backends
--------

The `Backend` class
~~~~~~~~~~~~~~~~~~~~~

The `Renderable` class
~~~~~~~~~~~~~~~~~~~~~~~~

Cairo backend
=============
