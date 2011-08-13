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

Essential concepts
==================

Monoids
-------

Faking optional named arguments
-------------------------------

Bounding functions and local vector spaces
------------------------------------------

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
  is a synonym for `(Double, Double)`.  The positive x-axis extends to
  the right, and the positive y-axis extends *upwards*.  This is
  consistent with standard mathematical practice, but upside-down with
  respect to many common graphics systems.  This is intentional: the
  goal is to provide an elegant interface which is abstracted as much
  as possible from implementation details.
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

__ `2D Transformations`_

.. codeblock:: dia-lhs

   > example = unitCircle # scaleX 0.5 # rotateBy (1/6)

For convenience the standard library also provides `ellipse`, for
creating an ellipse with a given eccentricity, and `ellipseXY`, for
creating an axis-aligned ellipse with specified radii in the x and y
directions.

Arcs
~~~~

`Diagrams.TwoD.Arc`:mod: provides a function `arc`, which constructs a
radius-one circular arc starting at a first angle and extending
counterclockwise to the second.

.. codeblock:: dia-lhs

   > example = arc (tau/4 :: Rad) (4 * tau / 7 :: Rad)

Polygons
~~~~~~~~

The `polygon` function from `Diagrams.TwoD.Shapes`:mod: constructs
regular radius-one polygons centered at the origin.  Its argument is a
record of optional arguments that control the generated polygon:

* `sides` determines the number of sides (default: `5`).
* `edgeSkip` allows for the creation of star polygons by specifying
  that edges should connect every nth vertex.  The default is `1`.
* `orientation` specifies the `PolygonOrientation`.

.. codeblock:: dia-lhs

   > poly1 = polygon with { sides = 6, orientation = OrientToX }
   > poly2 = polygon with { sides = 7, edgeSkip = 2 }
   > poly3 = polygon with { sides = 5 }
   > example = poly1 ||| poly2 ||| poly3

Notice the idiom of using `with` to construct a record of default
options and selectively overriding particular options by name. `with`
is a synonym for `def` from the type class `Default`, which specifies
a default value for types which are instances.  You can read more
about this idiom in the section `Faking optional named arguments`_.

A future version of the library will likely expand the `polygon` function
with additional options; if there are particular options you would
like to see, record your request in the `bug tracker`_.

.. _`bug tracker` : http://code.google.com/p/diagrams/issues/list

Squares, rectangles, and other polygons
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`Diagrams.TwoD.Shapes`:mod: also provides a number of other
specialized path-based shapes.  In principle you could construct all
of these shapes explicitly using various polygons.

* `square` constructs a square with a given side length; `unitSquare`
  constructs a square with sides of length `1`.
* `rect` constructs a rectangle of a given width and height.
* `eqTriangle` constructs an equilateral triangle with radius `1`.
* `roundedRect` constructs a rectangle with circular rounded corners.

.. codeblock:: dia-lhs

  > example = square 1 ||| rect 0.3 0.5 ||| eqTriangle ||| roundedRect (0.7,0.4) 0.1

More special polygons will likely be added in future versions of the
library.

Other
~~~~~

Completing the hodgepodge in `Diagrams.TwoD.Shapes`:mod: for now, the
functions `hrule` and `vrule` create horizontal and vertical lines,
respectively.

.. codeblock:: dia-lhs

   > example = circle 1 ||| hrule 2 ||| circle 1

Combining diagrams
------------------

The ``diagrams`` framework is fundamentally *compositional*: complex
diagrams are created by combining simpler diagrams in various ways.
Many of the combination methods discussed in this section are defined
in `Diagrams.Combinators`:mod:.

Superimposing diagrams with `atop`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The most fundamental way to combine two diagrams is to place one on
top of the other with `atop`.  The diagram `d1 \`atop\` d2` is formed
by placing `d1`'s local origin on top of `d2`'s local origin; that is,
by identifying their local vector spaces.  

.. codeblock:: dia-lhs

  > example = circle 1 `atop` square (sqrt 2)

As noted before, diagrams form a monoid_
with composition given by identification of vector spaces.  `atop` is
simply a synonym for `mappend` (or `(<>)`), specialized to two
dimensions.

.. _monoid: Monoids_

This also means that a list of diagrams can be stacked with `mconcat`;
that is, `mconcat [d1, d2, d3, ...]` is the diagram with `d1` on top
of `d2` on top of `d3` on top of...

.. codeblock:: dia-lhs

  > example = mconcat [ circle 0.1 # fc green
  >                   , eqTriangle # scale 0.4 # fc yellow
  >                   , square 1 # fc blue
  >                   , circle 1 # fc red
  >                   ]

Juxtaposing diagrams
~~~~~~~~~~~~~~~~~~~~

Fundamentally, `atop` is actually the *only* way to compose diagrams;
however, there are a number of other combining methods (all ultimately
implemented in terms of `atop`) provided for convenience.

Two diagrams can be placed *next to* each other using `beside`.  The
first argument to `beside` is a vector specifying a direction.  The
second and third arguments are diagrams, which are placed next to each
other so that the vector points from the first diagram to the second.

.. codeblock:: dia-lhs

  > example = beside (20,30) (circle 1 # fc orange) (circle 1.5 # fc purple)

As can be seen from the above example, the *length* of the vector
makes no difference, only its *direction* is taken into account. (To
place diagrams at a certain fixed distance from each other, see `cat'`.)

Concatenating diagrams
~~~~~~~~~~~~~~~~~~~~~~

Modifying diagrams
------------------

Attributes and styles
~~~~~~~~~~~~~~~~~~~~~

2D Transformations
~~~~~~~~~~~~~~~~~~

Alignment
~~~~~~~~~

Working with paths
------------------

Segments
~~~~~~~~

Trails
~~~~~~

Paths
~~~~~

The `PathLike` class
~~~~~~~~~~~~~~~~~~~~

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
