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

2D standard library
===================

The `diagrams-lib`:pkg: package provides a standard library XXX

Primitive shapes
----------------

Circles and ellipses
~~~~~~~~~~~~~~~~~~~~

Circles can be created with the `unitCircle` and `circle`
functions, defined in `Diagrams.TwoD.Ellipse`:mod:.

XXX insert type signatures of unitCircle and circle?

`unitCircle` creates a circle of radius 1 centered at the
origin; `circle` takes the desired radius as an argument.

Every ellipse is the image of the unit circle under some affine
transformation, so ellipses can be created by appropriately `scaling
and rotating`__ circles.  For convenience the standard library also
provides `ellipse`, for creating an ellipse with a given eccentricity,
and `ellipseXY`, for creating an axis-aligned ellipse with specified
radii in the x and y directions.

__ `2D Transformations`_

Arcs
~~~~

`Diagrams.TwoD.Arc`:mod:

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

Paths
-----

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

Names
-----

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
