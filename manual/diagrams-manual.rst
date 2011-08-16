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

Vectors and points
------------------

Bounding functions and local vector spaces
------------------------------------------

[TODO: write about the basics/intuition of bounding functions]
[TODO: write about local origin, note 'showOrigin' function]

[TODO: write about strut, pad, withBounds, phantom somewhere]

Postfix transformation
----------------------

[TODO: write about # operator]

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

[TODO: note re: vectors vs points.]

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
radius-one circular arc starting at a first angle__ and extending
counterclockwise to the second.

__ `Angles`_

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
specialized path-based shapes.   For constructing more general shapes,
see `Working with paths`_.

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
  >           # showOrigin

As can be seen from the above example, the *length* of the vector
makes no difference, only its *direction* is taken into account. (To
place diagrams at a certain fixed distance from each other, see
`cat'`.)  As can also be seen, the local origin of the new, combined
diagram is at the point of tangency between the two subdiagrams.

To place diagrams next to each other while leaving the local origin of
the combined diagram in the same place as the local origin of the
first subdiagram, use `append` instead of `beside`:

.. codeblock:: dia-lhs

  > example = append (20,30) (circle 1 # fc orange) (circle 1.5 # fc purple)
  >           # showOrigin

Since placing diagrams next to one another horizontally and vertically
is quite common, special combinators are provided for convenience.
`(|||)` and `(===)` are specializations of `beside` which juxtapose
diagrams in the x and y-directions, respectively.

.. codeblock:: dia-lhs

  > d1 = circle 1 # fc red
  > d2 = square 1 # fc blue
  > example = (d1 ||| d2) ||| strutX 3 ||| ( d1
  >                                          ===
  >                                          d2  )

See `Bounding functions and local vector spaces`_ for more information
on what "next to" means, or see `Bounding functions`_ for precise
details.

Concatenating diagrams
~~~~~~~~~~~~~~~~~~~~~~

We have already seen one way to combine a list of diagrams, using
`mconcat` to stack them.  Several other methods for combining lists of
diagrams are also provided in `Diagrams.Combinators`:mod:.

The simplest method of combining multiple diagrams is `position`,
which takes a list of diagrams paired with points, and places the
local origin of each diagram at the indicated point.

.. codeblock:: dia-lhs

  > example = position (zip (map mkPoint [-3, -2.8 .. 3]) (repeat dot))
  >   where dot       = circle 0.2 # fc black
  >         mkPoint x = P (x,x^2)

`cat` is like an iterated version of `beside`, which takes a direction
vector and a list of diagrams, laying out the diagrams beside one
another in a row.  The local origins of the subdiagrams will be placed
along a straight line in the direction of the given vector.

.. codeblock:: dia-lhs

  > example = cat (2,-1) (map p [3..8]) # showOrigin
  >   where p n = polygon with {sides = n} # lw 0.03

Note, however, that the local origin of the final diagram is placed at
the local origin of the first diagram in the list.

For more control over the way in which the diagrams are laid out, use
`cat'`, a variant of `cat` which also takes a `CatOpts` record.  See
the documentation for `cat'` and `CatOpts` to learn about the various
possibilities.

.. codeblock:: dia-lhs

  > example = cat' (2,-1) with { catMethod = Distrib, sep = 2 } (map p [3..8])
  >   where p n = polygon with {sides = n} # lw 0.03
  >                                        # scale (1 + fromIntegral n/4)
  >                                        # showOrigin

For convenience, `Diagrams.TwoD.Combinators`:mod: also provides `hcat`, `hcat'`,
`vcat`, and `vcat'`, variants of `cat` and `cat'` which concatenate
diagrams horizontally and vertically.

Finally, `appends` is like an iterated variant of `append`, with the
important difference that multiple diagrams are placed next to a
single central diagram without reference to one another; simply
iterating `append` causes each of the previously appended diagrams to
be taken into account when deciding where to place the next one.

.. codeblock:: dia-lhs

  > c        = circle 1 # lw 0.03
  > dirs     = iterate (rotateBy (1/7)) unitX
  > cdirs    = zip dirs (replicate 7 c)
  > example1 = appends c cdirs
  > example2 = foldl (\a (v,b) -> append v a b) c cdirs
  > example  = example1 ||| strutX 3 ||| example2

`Diagrams.Combinators`:mod: also provides `decoratePath` and
`decorateTrail`, which are described in `Stroking and decorating
paths`_.

Modifying diagrams
------------------

[TODO: some sort of general statement about modifying diagrams]

Attributes and styles
~~~~~~~~~~~~~~~~~~~~~

Every diagram has a *style* which is an arbitrary collection of
*attributes*.  This section will describe some of the default
attributes which are provided by the ``diagrams`` library and
recognized by most backends.  However, you can easily create your own
attributes as well; for details, see `Style and attribute internals`_.

In many examples, you will see attributes applied to diagrams using
the `(#)` operator.  However, keep in mind that there is nothing
special about this operator as far as attributes are concerned. It is
merely backwards function application, which is used for attributes
since it often reads better to have the main diagram come first,
followed by modifications to its attributes.

In general, inner attributes (that is, attributes applied earlier)
override outer ones.  Note, however, that this is not a requirement.
Each attribute may define its own specific method for combining
multiple instances.  See `Style and attribute internals`_ for more
details.

Most of the attributes discussed in this section are defined in
`Diagrams.Attributes`:mod:.

Color
^^^^^

Two-dimensional diagrams have two main colors, the color used to
stroke the paths in the diagram and the color used to fill them.
These can be set, respectively, with the `lc` (line color) and `fc`
(fill color) functions.

.. codeblock:: dia-lhs

  > example = circle 0.2 # lc purple # fc yellow

By default, diagrams use a black line color and a completely
transparent fill color.

Colors themselves are handled by the `colour`:pkg: package, which
provides a large set of predefined color names as well as many more
sophisticated color operations; see its documentation for more
information.  The `colour`:pkg: package uses a different type for
colors with an alpha channel (*i.e.* transparency). To make use of
transparent colors you can use `lcA` and `fcA`.

.. codeblock:: dia-lhs

  > import Data.Colour (withOpacity)
  >
  > colors  = map (blue `withOpacity`) [0.1, 0.2 .. 1.0]
  > example = hcat' with { catMethod = Distrib, sep = 1 }
  >                 (zipWith fcA colors (repeat (circle 1)))

Transparency can also be tweaked with the `Opacity` attribute, which
sets the opacity/transparency of a diagram as a whole. Applying
`opacity p` to a diagram, where `p` is a value between `0` and `1`,
results in a diagram `p` times as opaque.

.. codeblock:: dia-lhs

  > s c     = square 1 # fc c
  > reds    = (s darkred ||| s red) === (s pink ||| s indianred)
  > example = hcat' with { sep = 1 } . take 4 . iterate (opacity 0.7) $ reds

Line width
^^^^^^^^^^

Other line parameters
^^^^^^^^^^^^^^^^^^^^^

Many rendering backends provide some control over the particular way
in which lines are drawn.  Currently, ``diagrams`` provides support
for three aspects of line drawing:

* `lineCap` sets the `LineCap` style.
* `lineJoin` sets the `LineJoin` style.
* `dashing` allows for drawing dashed lines with arbitrary dashing
  patterns.

.. codeblock:: dia-lhs

  > path = fromVertices (map P [(0,0), (1,0.3), (2,0), (2.2,0.3)]) # lw 0.1
  > example = centerXY . vcat' with { sep = 0.1 }
  >           $ map (path #)
  >             [ lineCap LineCapButt   . lineJoin LineJoinMiter
  >             , lineCap LineCapRound  . lineJoin LineJoinRound
  >             , lineCap LineCapSquare . lineJoin LineJoinBevel
  >             , dashing [0.1,0.2,0.3,0.1] 0
  >             ]

2D Transformations
~~~~~~~~~~~~~~~~~~

Any diagram can be transformed by applying arbitrary affine
transformations to it. *Affine* transformations include *linear*
transformations (rotation, scaling, reflection, shears --- anything
which leaves the origin fixed and sends lines to lines) as well as
translations.  `Diagrams.TwoD.Transform`:mod: defines a number of
common affine transformations in two-dimensional space. (To construct
transformations more directly, see
`Graphics.Rendering.Diagrams.Transform`:mod:.)

Every transformation comes in two variants, a noun form and a verb
form.  For example, there are two functions for scaling along the
x-axis, `scalingX` and `scaleX`.  The noun form constructs a
transformation object, which can then be stored in a data structure,
passed as an argument, combined with other transformations, *etc.*,
and ultimately applied to a diagram with the `transform` function.
The verb form directly applies the transformation to a diagram.  The
verb form is much more common (and the documentation below will only
discuss verb forms), but getting one's hands on a transformation can
occasionally be useful.

Rotation
^^^^^^^^

Use `rotate` to rotate a diagram by a given angle__ about the origin.
Since `rotate` takes an angle, you must specify an angle type, as in
`rotate (80 :: Deg)`.  In the common case that you wish to rotate by
an angle specified as a certain fraction of a circle, like `rotate
(1/8 :: CircleFrac)`, you can use `rotateBy` instead. `rotateBy` is
specialized to only accept fractions of a circle, so in this example
you would only have to write `rotateBy (1/8)`.

You can also use `rotateAbout` in the case that you want to rotate
about some point other than the origin.

__ `Angles`_

Scaling and reflection
^^^^^^^^^^^^^^^^^^^^^^

Scaling by a given factor is accomplished with `scale` (scale
uniformly), `scaleX` (scale along the x-axis only), or `scaleY` (scale
along the y-axis only).  All of these can be used both for enlarging
(with a factor greater than one) and shrinking (with a factor less
than one).  Using a negative factor results in a reflection (in the
case of `scaleX` and `scaleY`) or a 180-degree rotation (in the case
of `scale`).

.. codeblock:: dia-lhs

  > eff = text "F" <> square 1 # lw 0
  > ts  = [ id, scale 2,    scaleX 2,    scaleY 2
  >       ,     scale (-1), scaleX (-1), scaleY (-1)
  >       ]
  >
  > example = hcat . map (eff #) $ ts

Scaling by zero is forbidden.  Let us never speak of it again.

For convenience, `reflectX` and `reflectY` perform reflection along
the x- and y-axes, respectively; but I think you can guess how they
are implemented.  Their names are slightly confusing (does `reflectX`
reflect *along* the x-axis or *across* the x-axis?) but you can just
remember that `reflectX = scaleX (-1)`.

To reflect in some line other than an axis, use `reflectAbout`.

.. codeblock:: dia-lhs

  > eff = text "F" <> square 1 # lw 0
  > example = eff 
  >        <> reflectAbout (P (0.2,0.2)) (rotateBy (-1/10) unitX) eff

Translation
^^^^^^^^^^^

Translation is achieved with `translate`, `translateX`, and
`translateY`, which should be self-explanatory.

Conjugation
^^^^^^^^^^^

`Diagrams.Transform`:mod: also provides [TODO]

use to e.g. scale along some direction other than x/y

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

Splines
~~~~~~~

Stroking and decorating paths
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Clipping
~~~~~~~~

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

Style and attribute internals
-----------------------------

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
