.. role:: pkg(literal)
.. role:: hs(literal)
.. role:: mod(literal)

.. default-role:: hs

====================
Diagrams User Manual
====================

.. contents::

Preliminaries
=============

Introduction
------------

``diagrams`` is a flexible, powerful embedded domain-specific language
(EDSL) for creating vector graphics.  It can be used for creating a
wide range of graphics, such as ...

.. container:: todo

  * A cool example or two

About this document
-------------------

This document attempts to explain all major aspects of using the
``diagrams`` core and standard libraries, organized by topic to make
it easy to find what you are looking for.  It is not, however, a
complete reference of every single function in the standard library:
for that, see the API documentation listed under `Other resources`_.
Most sections contain links to relevant module(s) you can follow to
read about other functions not covered in the text.

Module names in the text are typeset like this:
`Diagrams.Prelude`:mod:.  Click on a module name to visit its
documentation.  You can also click on any function or operator name in
code examples to take you to its documentation.  Try it:

.. class:: lhs

::

  example = circle 2 ||| unitCircle

Mathematical equations are typeset using MathJax_.  Right-click on any
equation to access MathJax options, like displaying the LaTeX source,
switching between MathML and HTML/CSS for display, zoom settings, and
so on.

.. _MathJax: http://www.mathjax.org/

This user manual is still under construction.  Content that has yet to
be written is noted by a light red box with a "document" icon on the
right hand side, like this:

.. container:: todo

  * Explain zygohistomorphic prepromorphisms
  * Essay on postmodernist critiques of ``diagrams`` vis-a-vis Kant

If you see a box like this in the place of something you would really
like to know about, please bug the developers (using the ``#diagrams`` IRC
channel on Freenode, or the `diagrams mailing list`_) so they can
prioritize it!

Other resources
---------------

Here are some other resources that may be helpful to you as you learn
about ``diagrams``:

  * The API reference documentation for all the ``diagrams`` packages
    is intended to be high-quality and up-to-date.  If you find an
    omission, error, or something confusing, please `report it as a
    bug`_!

        - `diagrams-core`:pkg:
        - `diagrams-lib`:pkg:
        - `diagrams-cairo`:pkg:

  * The ``diagrams`` website_ has a `gallery of examples`_ and links
    to tutorials, blog posts, and other documentation.
  * The ``#diagrams`` IRC channel on Freenode is a friendly place
    where you can get help from other ``diagrams`` developers and users.
  * Consider joining the `diagrams mailing list`_ for discussions
    and announcements about ``diagrams``.
  * See the `developer wiki`_ for more specialized documentation and
    information on planned and ongoing development.
  * See the `bug tracker`_ for a list of open tickets.  If you find a
    bug or would like to request a feature, please file a ticket!

.. _`report it as a bug`: http://code.google.com/p/diagrams/issues/list
.. _website: http://projects.haskell.org/diagrams
.. _`gallery of examples`: http://projects.haskell.org/diagrams/gallery.html
.. _`diagrams mailing list`: http://groups.google.com/group/diagrams-discuss?pli=1
.. _`developer wiki`: http://code.google.com/p/diagrams/
.. _`bug tracker` : http://code.google.com/p/diagrams/issues/list

Installation
------------

Before installing ``diagrams``, you will need the following:

  * The `Glasgow Haskell Compiler`_ (GHC), version 7.0.2 or later.
  * The latest release of the `Haskell Platform`_ (currently
    2011.2.0.1).

If you are on a Mac or Windows, GHC itself comes with the Haskell
Platform; if you are on Linux, you will have to install GHC first.

.. _`Glasgow Haskell Compiler`: http://www.haskell.org/ghc/
.. _`Haskell Platform`: http://hackage.haskell.org/platform/

Once you have successfully installed the Haskell platform, installing
``diagrams`` should be as easy as issuing the command:

::

  cabal install diagrams

.. container:: todo

  Currently this isn't quite true because of difficulty of installing
  cairo.  Make sure we either have an alternate backend in place OR
  add more information about installing cairo here before releasing.

Getting started
---------------

Create a file called ``TestDiagram.hs`` (or whatever you like) with
the following contents:

::

  {-# LANGUAGE NoMonomorphismRestriction #-}

  import Diagrams.Prelude
  import Diagrams.Backend.Cairo.CmdLine

  main = defaultMain (circle 1)

The first line turns off the evil `monomorphism restriction`_, which is
quite important when using ``diagrams``: otherwise you will quickly
run into lots of crazy error messages.

.. _`monomorphism restriction`: http://www.haskell.org/haskellwiki/Monomorphism_restriction

`Diagrams.Prelude`:mod: re-exports most everything from the standard
library; `Diagrams.Backend.Cairo.CmdLine`:mod: provides a command-line
interface to the cairo rendering backend.

To compile your program, type

::

  $ ghc --make TestDiagram

(Note that the ``$`` indicates a command prompt and should not
actually be typed.)  Then execute ``TestDiagram`` with some
appropriate options:

::

  $ ./TestDiagram -w 100 -h 100 -o TestDiagram.png

The above will generate a 100x100 PNG that should look like this:

.. class:: dia

::

> example = circle 1

Try typing

::

  $ ./TestDiagram --help

to see the other options that are supported.

.. container:: todo

  * Link to the tutorial
  * Change the above for whatever the recommended starter backend is,
    if it changes

Essential concepts
==================

Before we jump into the main content of the manual, this chapter
explains a number of general ideas and central concepts that will
recur throughought.  If you're eager to skip right to the good stuff,
feel free to skip this section at first, and come back to it when
necessary; there are many links to this chapter from elsewhere in the
manual.

Monoids
-------

A *monoid* consists of

  * A set of elements `S`:math:
  * An *associative binary operation* on the set, that is, some
    operation

    `\oplus \colon S \to S \to S`:math:

    for which

    `(x \oplus y) \oplus z = x \oplus (y \oplus z).`:math:

  * An *identity element* `i \in S`:math: which is the identity for
    `\oplus`:math:, that is,

    `x \oplus i = i \oplus x = x.`:math:

In Haskell, monoids are expressed using the `Monoid` type class,
defined in ``Data.Monoid``:

.. class:: lhs

::

  class Monoid m where
    mempty  :: m
    mappend :: m -> m -> m

The `mappend` function represents the associative binary operation,
and `mempty` is the identity element.  A function

.. class:: lhs

::

  mconcat :: Monoid m => [m] -> m

is also provided as a shorthand for the common operation of combining
a whole list of elements with `mappend`.

Since `mappend` is tediously long to write, ``diagrams`` provides the
operator `(<>)` as a synonym. (Hopefully this synonym will soon become
part of ``Data.Monoid`` itself!)

Monoids are used extensively in ``diagrams``: diagrams,
transformations, trails, paths, styles, and colors are all
instances.

Faking optional named arguments
-------------------------------

Many diagram-related operations can be customized in a wide variety of
ways.  For example, when creating a regular polygon, one can customize
the number of sides, the radius, the orientation, and so on. However,
to have a single function that takes all of these options as separate
arguments is a real pain: it's hard to remember what the arugments are
and what order they should go in, and often one wants to use default
values for many of the options and only override a few.  Some
languages (such as Python) support *optional, named* function
arguments, which are ideal for this sort of situation.  Sadly, Haskell
does not.  However, we can fake it!

Any function which should take some optional, named arguments instead
takes a single argument which is a record of options.  The record type
is declared to be an instance of the `Default` type class:

.. class:: lhs

::

> class Default d where
>   def :: d

That is, types which have a `Default` instance have some default value
called `def`.  For option records, `def` is declared to be the record
containing all the default arguments.  The idea is that you can pass
`def` as an argument to a function which takes a record of options,
and use record update syntax to override only the fields you want,
like this:

::

  foo (def { arg1 = someValue, arg6 = blah })

There are a couple more things to note.  First, record update actually
binds *more tightly* than function application, so the parentheses
above are actually not necessary.  Second, ``diagrams`` also defines
`with` as a synonym for `def`, which makes the syntax a bit more
natural.  So, instead of the above, you could write

::

  foo with { arg1 = someValue, arg6 = blah }

Vectors and points
------------------

Although much of this user manual focuses on constructing
two-dimensional diagrams, the definitions in the core library in fact
work for *any* vector space.  Vector spaces are defined in the
`Data.VectorSpace`:mod: module from the `vector-space`:pkg: package.

Many objects (diagrams, paths, backends...) inherently live in some
particular vector space.  The vector space associated to any type can
be computed by the type function `V`.  So, for example, the type

::

  Foo d => V d -> d -> d

is the type of a two-argument function whose first argument is a
vector in whatever vector space corresponds to the type `d` (which
must be an instance of `Foo`).

Each vector space has a type of *vectors* `v` and an associated type
of *scalars*, `Scalar v`.  A vector represents a direction and
magnitude, whereas a scalar represents only a magnitude.  Important
operations on vectors and scalars include:

  * Adding and subtracting vectors with `(^+^)` and `(^-^)`
  * Multiplying a vector by a scalar with `(*^)`

See `Data.VectorSpace`:mod: for other functions and operators.

One might think we could also identify *points* in a space with
vectors having one end at the origin.  However, this turns out to be a
poor idea. There is a very important difference between vectors and
points: namely, vectors are translationally invariant whereas points
are not.  A vector represents a direction and magnitude, not a
location. Translating a vector has no effect. Points, on the other
hand, represent a specific location. Translating a point results in a
different point.

Although it is a bad idea to *conflate* vectors and points, we can
certainly *represent* points using vectors. ``diagrams`` defines a
newtype wrapper around vectors called `Point`.  The most important
connection between points and vectors is given by `(.-.)`, defined in
`Data.AffineSpace`:mod:. If `p1` and `p2` are points, `p2 .-. p1` is
the vector giving the direction and distance from `p1` to `p2`.
Offsetting a point by a vector (resulting in a new point) is
accomplished with `(.+^)`.

Bounding functions and local vector spaces
------------------------------------------

In order to be able to position diagrams relative to one another, each
diagram must keep track of some bounding information.  Rather than use
a bounding *box* (which is neither general nor compositional) or even a
more general bounding *path* (which is rather complicated to deal with),
each diagram has an associated bounding *function*.  Given some
direction (represented by a vector) as input, the bounding function
answers the question: "how far in this direction must one go before
reaching a perpendicular (hyper)plane that completely encloses the
diagram on one side of it?"

That's a bit of a mouthful, so hopefully the below illustration will
help clarify things if you found the above description confusing.
(For completeness, the code used to generate the illustration is
included, although you certainly aren't expected to understand it yet
if you are just reading this manual for the first time!)

.. class:: dia-lhs

::

> illustrateBound v d
>   = mconcat
>     [ origin ~~ (origin .+^ v)
>       # lc black # lw 0.03
>     , polygon with { polyType   = PolyRegular 3 0.1
>                    , polyOrient = OrientTo (negateV v)
>                    }
>       # fc black
>       # translate v
>     , origin ~~ b
>       # lc green # lw 0.05
>     , p1 ~~ p2
>       # lc red # lw 0.02
>     ]
>     where
>       b  = boundary v d
>       v' = normalized v
>       p1 = b .+^ (rotateBy (1/4) v')
>       p2 = b .+^ (rotateBy (-1/4) v')
>
> d1 :: Path R2
> d1 = circlePath 1
>
> d2 :: Path R2
> d2 = (pentagon 1 === roundedRect (1.5,0.7) 0.3)
>
> example = (stroke d1 # showOrigin <> illustrateBound (-0.5,0.3) d1)
>       ||| (stroke d2 # showOrigin <> illustrateBound (0.5, 0.2) d2)

The black arrows represent inputs to the bounding functions for the
two diagrams; the bounding functions' outputs are the distances
represented by the thick green lines.  The red lines illustrate the
enclosing (hyper)planes (which are really to be thought of as
extending infinitely to either side): notice how they are as close as
possible to the diagrams without intersecting them at all.

Of course, the *base point* from which the bounding function is
measuring matters quite a lot!  If there were no base point, questions
of the form "*how far do you have to go...*" would be meaningless --
how far *from where*?  This base point (indicated by the red dots in
the diagram above) is called the *local origin* of a diagram.  Every
diagram has its own intrinsic *local vector space*; operations on
diagrams are always with respect to their local origin, and you can
affect the way diagrams are combined with one another by moving their
local origins.  The `showOrigin` function is provided as a quick way
of visualizing the local origin of a diagram (also illustrated above).

Postfix transformation
----------------------

You will often see idiomatic ``diagrams`` code that looks like this:

::

  foobar # attr1
         # attr2
         # attr3
         # transform1

There is nothing magical about `(#)`, and it is not required in order
to apply attributes or transformations. In fact, it is nothing more
than reverse function application with a high precedence (namely, 8):

::

  x # f = f x

`(#)` is provided simply because it often reads better to first write
down what a diagram *is*, and then afterwards write down attributes
and modifications.  Additionally, `(#)` has a high precedence so it
can be used to make "local" modifications without using lots of
parentheses:

.. class:: lhs

::

> example =     square 2 # fc red # rotateBy (1/3)
>           ||| circle 1 # lc blue # fc green

Note how the modifiers `fc red` and `rotateBy (1/3)` apply only to the square,
and `lc blue` and `fc green` only to the circle (`(|||)` has a
precedence of 6).

Creating 2D diagrams
====================

.. container:: todo

  * add some fun diagrams here?

The main purpose of ``diagrams`` is to construct two-dimensional
vector graphics, although it can be used for more general purposes as
well.  This section explains the building blocks provided by
`diagrams-core`:pkg: and `diagrams-lib`:pkg: for constructing
two-dimensional diagrams.

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

There are three main type synonyms defined for referring to
two-dimensional space:

* `R2` is the type of the two-dimensional Euclidean vector space.  It
  is a synonym for `(Double, Double)`.  The positive `x`:math:\-axis extends to
  the right, and the positive `y`:math:\-axis extends *upwards*.  This is
  consistent with standard mathematical practice, but upside-down with
  respect to many common graphics systems.  This is intentional: the
  goal is to provide an elegant interface which is abstracted as much
  as possible from implementation details.

  `unitX` and `unitY` are unit vectors in the positive `x`:math:\- and
  `y`:math:\-directions, respectively.  Their negated counterparts are `unit_X`
  and `unit_Y`.

* `P2` is the type of points in two-dimensional space. It is a synonym
  for `Point R2`.  The distinction between points and vectors is
  important; see `Vectors and points`_.

* `T2` is the type of two-dimensional affine transformations.  It is a
  synonym for `Transformation R2`.

Angles
~~~~~~

The `Angle` type class classifies types which measure two-dimensional
angles.  Three instances are provided by default (you can, of course,
also make your own):

* `CircleFrac` represents fractions of a circle.  A value of `1`
  represents a full turn.
* `Rad` represents angles measured in radians.  A value of `tau` (that
  is, `\tau = 2 \pi`:math:) represents a full turn. (If you haven't heard of
  `\tau`:math:, see `The Tau Manifesto`__.)
* `Deg` represents angles measured in degrees.  A value of `360`
  represents a full turn.

__ http://tauday.com

The intention is that to pass an argument to a function that expects a
value of some `Angle` type, you can write something like `(3 :: Deg)`
or `(3 :: Rad)`.  The `convertAngle` function is also provided for
converting between different angle representations.

The `direction` function computes the angle of a vector, measured
clockwise from the positive `x`:math:\-axis.

Primitive shapes
----------------

`diagrams-lib`:pkg: provides many standard two-dimensional shapes for
use in constructing diagrams.

Circles and ellipses
~~~~~~~~~~~~~~~~~~~~

Circles can be created with the `unitCircle` and `circle`
functions, defined in `Diagrams.TwoD.Ellipse`:mod:.

For example,

.. class:: dia-lhs

::

> example = circle 0.5 <> unitCircle

`unitCircle` creates a circle of radius 1 centered at the
origin; `circle` takes the desired radius as an argument.

Every ellipse is the image of the unit circle under some affine
transformation, so ellipses can be created by appropriately `scaling
and rotating`__ circles.

__ `2D Transformations`_

.. class:: dia-lhs

::

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

.. class:: dia-lhs

::

> example = arc (tau/4 :: Rad) (4 * tau / 7 :: Rad)

Pre-defined shapes
~~~~~~~~~~~~~~~~~~

`Diagrams.TwoD.Shapes`:mod: provides a number of pre-defined
polygons and other path-based shapes.  For example:

* `eqTriangle` constructs an equilateral triangle with sides of a
  given length.
* `square` constructs a square with a given side length; `unitSquare`
  constructs a square with sides of length `1`.
* `pentagon`, `hexagon`, ..., `dodecagon` construct other regular
  polygons with sides of a given length.
* In general, `regPoly` constructs a regular polygon with any number
  of sides.
* `rect` constructs a rectangle of a given width and height.
* `roundedRect` constructs a rectangle with circular rounded corners.

.. class:: dia-lhs

::

> example = square 1 ||| rect 0.3 0.5
>       ||| eqTriangle 1 ||| roundedRect (0.7,0.4) 0.1

More special polygons will likely be added in future versions of the
library.

Completing the hodgepodge in `Diagrams.TwoD.Shapes`:mod: for now, the
functions `hrule` and `vrule` create horizontal and vertical lines,
respectively.

.. class:: dia-lhs

::

> example = circle 1 ||| hrule 2 ||| circle 1

General polygons
~~~~~~~~~~~~~~~~

The `polygon` function from `Diagrams.TwoD.Polygons`:mod: can be used
to construct a wide variety of polygons.  Its argument is a record of
optional arguments that control the generated polygon:

* `polyType` specifies one of several methods for determining the
  vertices of the polygon:

    * `PolyRegular` indicates a regular polygon with a certain number
      of sides and a given *radius*.
    * `PolySides` specifies the vertices using a list of angles
      between edges, and a list of edge lengths.
    * `PolyPolar` specifies the vertices using polar coordinates: a
      list of central angles between vertices, and a list of vertex
      radii.

* `polyOrient` specifies the `PolyOrientation`: the polygon can be
  oriented with an edge parallel to the `x`:math:\-axis. with an edge parallel
  to the `y`:math:\-axis, or with an edge perpendicular to any given vector.
  You may also specify that no special orientation should be applied,
  in which case the first vertex of the polygon will be located along the
  positive `x`:math:\-axis.

* Additionally, a center other than the origin can be specified using
  `polyCenter`.

.. class:: dia-lhs

::

> poly1 = polygon with { polyType   = PolyRegular 13 5
>                      , polyOrient = OrientV }
> poly2 = polygon with { polyType   = PolyPolar (repeat (1/40 :: CircleFrac))
>                                               (take 40 $ cycle [2,7,4,6]) }
> example = (poly1 ||| strutX 1 ||| poly2) # lw 0.05

Notice the idiom of using `with` to construct a record of default
options and selectively overriding particular options by name. `with`
is a synonym for `def` from the type class `Default`, which specifies
a default value for types which are instances.  You can read more
about this idiom in the section `Faking optional named arguments`_.

Star polygons
~~~~~~~~~~~~~

A "star polygon" is a polygon where the edges do not connect
consecutive vertices; for example:

.. class:: dia-lhs

::

> example = star (StarSkip 3) (regPoly 13 1) # stroke

`Diagrams.TwoD.Polygons`:mod: provides the `star` function for
creating star polygons of this sort, although it is actually quite a
bit more general.

As its second argument, `star` expects a list of points.  One way
to generate a list of points is with polygon-generating functions such
as `polygon` or `regPoly`, or indeed, any function which can output
any `PathLike` type (see the section about `PathLike`_), since a list
of points is an instance of the `PathLike` class.  Of course, you are
free to construct the list of points using whatever method you like!

As its first argument, `star` takes a value of type `StarOpts`, for
which there are two possibilities:

* `StarSkip` specifies that every math:`n` th vertex should be
  connected by an edge.

  .. class:: dia-lhs

  ::

  > example = stroke (star (StarSkip 2) (regPoly 8 1))
  >       ||| strutX 1
  >       ||| stroke (star (StarSkip 3) (regPoly 8 1))

  As you can see, `star` may result in a path with multiple components,
  if the argument to `StarSkip` evenly divides the number of vertices.

* `StarFun` takes as an argument a function of type `(Int -> Int)`,
  which specifies which vertices should be connected to which other
  vertices.  Given the function `f`:math:, vertex `i`:math: is
  connected to vertex `j`:math: if and only if `f(i) \equiv j \pmod
  n`:math:, where `n`:math: is the number of vertices.  This can be
  used as a compact, precise way of specifying how to connect a set of
  points (or as a fun way to visualize functions in `Z_n`:math:!).

  .. class:: dia-lhs

  ::

  > funs          = map (flip (^)) [2..6]
  > visualize f	  = stroke' with { vertexNames = [[0 .. 6 :: Int]] }
  >                     (regPoly 7 1)
  >                   # lw 0
  >                   # showLabels
  >                   # fontSize 0.6
  >              <> star (StarFun f) (regPoly 7 1)
  >                   # stroke # lw 0.05 # lc red
  > example       = centerXY . hcat' with {sep = 0.5} $ map visualize funs

You may notice that all the above examples need to call `stroke` (or
`stroke'`), which converts a path into a diagram.  Many functions
similar to `star` are polymorphic in their return type over any
`PathLike`, but `star` is not. As we have seen, `star` may need to
construct a path with multiple components, which is not supported by
the `PathLike` class.

Combining diagrams
------------------

The ``diagrams`` framework is fundamentally *compositional*: complex
diagrams are created by combining simpler diagrams in various ways.
Many of the combination methods discussed in this section are defined
in `Diagrams.Combinators`:mod:.

Superimposing diagrams with ``atop``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The most fundamental way to combine two diagrams is to place one on
top of the other with `atop`.  The diagram `d1 \`atop\` d2` is formed
by placing `d1`'s local origin on top of `d2`'s local origin; that is,
by identifying their local vector spaces.

.. class:: dia-lhs

::

> example = circle 1 `atop` square (sqrt 2)

As noted before, diagrams form a monoid_
with composition given by identification of vector spaces.  `atop` is
simply a synonym for `mappend` (or `(<>)`), specialized to two
dimensions.

.. _monoid: Monoids_

This also means that a list of diagrams can be stacked with `mconcat`;
that is, `mconcat [d1, d2, d3, ...]` is the diagram with `d1` on top
of `d2` on top of `d3` on top of...

.. class:: dia-lhs

::

> example = mconcat [ circle 0.1 # fc green
>                   , eqTriangle 1 # scale 0.4 # fc yellow
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

.. class:: dia-lhs

::

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

.. class:: dia-lhs

::

> example = append (20,30) (circle 1 # fc orange) (circle 1.5 # fc purple)
>           # showOrigin

Since placing diagrams next to one another horizontally and vertically
is quite common, special combinators are provided for convenience.
`(|||)` and `(===)` are specializations of `beside` which juxtapose
diagrams in the `x`:math:\- and `y`:math:\-directions, respectively.

.. class:: dia-lhs

::

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

.. class:: dia-lhs

::

> example = position (zip (map mkPoint [-3, -2.8 .. 3]) (repeat dot))
>   where dot       = circle 0.2 # fc black
>         mkPoint x = P (x,x^2)

`cat` is like an iterated version of `beside`, which takes a direction
vector and a list of diagrams, laying out the diagrams beside one
another in a row.  The local origins of the subdiagrams will be placed
along a straight line in the direction of the given vector.

.. class:: dia-lhs

::

> example = cat (2,-1) (map p [3..8]) # showOrigin
>   where p n = regPoly n 1 # lw 0.03

Note, however, that the local origin of the final diagram is placed at
the local origin of the first diagram in the list.

For more control over the way in which the diagrams are laid out, use
`cat'`, a variant of `cat` which also takes a `CatOpts` record.  See
the documentation for `cat'` and `CatOpts` to learn about the various
possibilities.

.. class:: dia-lhs

::

> example = cat' (2,-1) with { catMethod = Distrib, sep = 2 } (map p [3..8])
>   where p n = regPoly n 1 # lw 0.03
>                           # scale (1 + fromIntegral n/4)
>                           # showOrigin

For convenience, `Diagrams.TwoD.Combinators`:mod: also provides `hcat`, `hcat'`,
`vcat`, and `vcat'`, variants of `cat` and `cat'` which concatenate
diagrams horizontally and vertically.

Finally, `appends` is like an iterated variant of `append`, with the
important difference that multiple diagrams are placed next to a
single central diagram without reference to one another; simply
iterating `append` causes each of the previously appended diagrams to
be taken into account when deciding where to place the next one.

.. class:: dia-lhs

::

> c        = circle 1 # lw 0.03
> dirs     = iterate (rotateBy (1/7)) unitX
> cdirs    = zip dirs (replicate 7 c)
> example1 = appends c cdirs
> example2 = foldl (\a (v,b) -> append v a b) c cdirs
> example  = example1 ||| strutX 3 ||| example2

`Diagrams.Combinators`:mod: also provides `decoratePath` and
`decorateTrail`, which are described in `Decorating trails and
paths`_.

Modifying diagrams
------------------

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

.. class:: dia-lhs

::

> example = circle 0.2 # lc purple # fc yellow

By default, diagrams use a black line color and a completely
transparent fill color.

Colors themselves are handled by the `colour`:pkg: package, which
provides a large set of predefined color names as well as many more
sophisticated color operations; see its documentation for more
information.  The `colour`:pkg: package uses a different type for
colors with an alpha channel (*i.e.* transparency). To make use of
transparent colors you can use `lcA` and `fcA`.

.. class:: dia-lhs

::

> import Data.Colour (withOpacity)
>
> colors  = map (blue `withOpacity`) [0.1, 0.2 .. 1.0]
> example = hcat' with { catMethod = Distrib, sep = 1 }
>                 (zipWith fcA colors (repeat (circle 1)))

Transparency can also be tweaked with the `Opacity` attribute, which
sets the opacity/transparency of a diagram as a whole. Applying
`opacity p` to a diagram, where `p` is a value between `0` and `1`,
results in a diagram `p` times as opaque.

.. class:: dia-lhs

::

> s c     = square 1 # fc c
> reds    = (s darkred ||| s red) === (s pink ||| s indianred)
> example = hcat' with { sep = 1 } . take 4 . iterate (opacity 0.7) $ reds

Line width
^^^^^^^^^^

To alter the *width* of the lines used to stroke paths, use `lw`. The
default line width is (arbitrarily) `0.01`.  You can also set the line
width to zero if you do not want a path stroked at all.

Line width actually more subtle than you might think.  Suppose you
create a diagram consisting of a square, and another square twice as
large next to it (using `scale 2`).  How should they be drawn?  Should
the lines be the same width, or should the larger square use a line
twice as thick?

In fact, in many situations the lines should actually be the *same*
thickness, so a collection of shapes will be drawn in a uniform way.
This is the default in ``diagrams``.  Specifically, the argument to
`lw` is measured with respect to the *final* vector space of a
complete, rendered diagram, *not* with respect to the local vector
space at the time the `lw` function is applied.  Put another way,
subsequent transformations do not affect the line width.  This is
perhaps a bit confusing, but trying to get line widths to look
reasonable would be a nightmare otherwise.

.. class:: dia-lhs

::

> example = (square 1
>       ||| square 1 # scale 2
>       ||| circle 1 # scaleX 3)   # lw 0.03

However, occasionally you *do* want subsequent transformations to
affect line width.  The `freeze` function is supplied for this
purpose.  Once `freeze` has been applied to a diagram, any subsequent
transformations will affect the line width.

.. class:: dia-lhs

::

> example = (square 1
>       ||| square 1 # freeze # scale 2
>       ||| circle 1 # freeze # scaleX 3)  # lw 0.03

Note that line width does not affect the bounding function of diagrams
at all.  Future versions of the standard library may provide a
function to convert a stroked path into an actual region, which would
allow line width to be taken into account.

Other line parameters
^^^^^^^^^^^^^^^^^^^^^

Many rendering backends provide some control over the particular way
in which lines are drawn.  Currently, ``diagrams`` provides support
for three aspects of line drawing:

* `lineCap` sets the `LineCap` style.
* `lineJoin` sets the `LineJoin` style.
* `dashing` allows for drawing dashed lines with arbitrary dashing
  patterns.

.. class:: dia-lhs

::

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
`x`:math:\-axis, `scalingX` and `scaleX`.  The noun form constructs a
transformation object, which can then be stored in a data structure,
passed as an argument, combined with other transformations, *etc.*,
and ultimately applied to a diagram with the `transform` function.
The verb form directly applies the transformation to a diagram.  The
verb form is much more common (and the documentation below will only
discuss verb forms), but getting one's hands on a transformation can
occasionally be useful.

Transformations in general
^^^^^^^^^^^^^^^^^^^^^^^^^^

Before looking at specific two-dimensional transformations, it's worth
saying a bit about transformations in general (a fuller treatment can
be found under `Transformations`_).  The `Transformation` type is
defined in `Graphics.Rendering.Diagrams.Transform`:mod:, from the
`diagrams-core`:pkg: package.  `Transformation` is parameterized by
the vector space over which it acts; recall that `T2` is provided as a
synonym for `Transformation R2`.

`Transformation v` is a `Monoid` for any vector space `v`:

* `mempty` is the identity transformation;
* `mappend` is composition of transformations: `t1 \`mappend\` t2`
  (also written `t1 <> t2`) performs first `t2`, then `t1`.

To invert a transformation, use `inv`.  For any transformation `t`,

`t <> inv t == inv t <> t == mempty`.

To apply a transformation to a diagram, use `transform`.  (In fact,
transformations can be applied not just to diagrams but to any
`Transformable` type, including vectors, points, trails, paths,
bounding functions, lists of `Transformable` things...)

Rotation
^^^^^^^^

Use `rotate` to rotate a diagram couterclockwise by a given angle__
about the origin.  Since `rotate` takes an angle, you must specify an
angle type, as in `rotate (80 :: Deg)`.  In the common case that you
wish to rotate by an angle specified as a certain fraction of a
circle, like `rotate (1/8 :: CircleFrac)`, you can use `rotateBy`
instead. `rotateBy` is specialized to only accept fractions of a
circle, so in this example you would only have to write `rotateBy
(1/8)`.

You can also use `rotateAbout` in the case that you want to rotate
about some point other than the origin.

__ `Angles`_

.. class:: dia-lhs

::

> eff = text "F" <> square 1 # lw 0
> rs  = map rotateBy [1/7, 2/7 .. 6/7]
> example = hcat . map (eff #) $ rs

Scaling and reflection
^^^^^^^^^^^^^^^^^^^^^^

Scaling by a given factor is accomplished with `scale` (which scales
uniformly in all directions), `scaleX` (which scales along the `x`:math:\-axis
only), or `scaleY` (which scales along the `y`:math:\-axis only).  All of these
can be used both for enlarging (with a factor greater than one) and
shrinking (with a factor less than one).  Using a negative factor
results in a reflection (in the case of `scaleX` and `scaleY`) or a
180-degree rotation (in the case of `scale`).

.. class:: dia-lhs

::

> eff = text "F" <> square 1 # lw 0
> ts  = [ scale (1/2), id, scale 2,    scaleX 2,    scaleY 2
>       ,     scale (-1), scaleX (-1), scaleY (-1)
>       ]
>
> example = hcat . map (eff #) $ ts

Scaling by zero is forbidden.  Let us never speak of it again.

For convenience, `reflectX` and `reflectY` perform reflection along
the `x`:math:\- and `y`:math:\-axes, respectively; but I think you can guess how they
are implemented.  Their names can be confusing (does `reflectX`
reflect *along* the `x`:math:\-axis or *across* the `x`:math:\-axis?) but you can just
remember that `reflectX = scaleX (-1)`.

To reflect in some line other than an axis, use `reflectAbout`.

.. class:: dia-lhs

::

> eff = text "F" <> square 1 # lw 0
> example = eff
>        <> reflectAbout (P (0.2,0.2)) (rotateBy (-1/10) unitX) eff

Translation
^^^^^^^^^^^

Translation is achieved with `translate`, `translateX`, and
`translateY`, which should be self-explanatory.

Conjugation
^^^^^^^^^^^

`Diagrams.Transform`:mod: exports useful transformation utilities
which are not specific to two dimensions.  At the moment there are
only two: `conjugate` and `under`.  The first simply performs
conjugation: `conjugate t1 t2 == inv t1 <> t2 <> t1`, that is,
performs `t1`, then `t2`, then undoes `t1`.

`under` performs a transformation using conjugation.  It takes as
arguments a function to perform some transformation as well as a
transformation to conjugate by.  For example, scaling by a factor of 2
along the diagonal line `y = x`:math: can be accomplished thus:

.. class:: dia-lhs

::

> eff = text "F" <> square 1 # lw 0
> example = (scaleX 2 `under` rotation (-1/8 :: CircleFrac)) eff

The letter F is first rotated so that the desired scaling axis lies
along the `x`:math:\-axis; then `scaleX` is performed; then it is rotated back
to its original position.

Note that `reflectAbout` and `rotateAbout` are implemented using
`under`.

Alignment
~~~~~~~~~

Since diagrams are always combined with respect to their local
origins, moving a diagram's local origin affects the way it combines
with others.  The position of a diagram's local origin is referred to
as its *alignment*.

The functions `moveOriginBy` and `moveOriginTo` are provided for
explicitly moving a diagram's origin, by an absolute amount and to an
absolute location, respectively.  `moveOriginBy` and `translate` are
actually dual, in the sense that

.. class:: law

::

    moveOriginBy v === translate (negateV v).

This duality comes about since `translate` moves a diagram with
respect to its origin, whereas `moveOriginBy` moves the *origin* with
respect to the *diagram*.  Both are provided so that you can use
whichever one corresponds to the most natural point of view in a given
situation, without having to worry about inserting calls to `negateV`.

Often, however, one wishes to move a diagram's origin with respect to
its bounding function.  To this end, some general tools are provided
in `Diagrams.Align`:mod:, and specialized 2D-specific ones by
`Diagrams.TwoD.Align`:mod:.

Functions like `alignT` (align Top) and `alignBR` (align Bottom Right)
move the local origin to the edge of the bounding region:

.. class:: dia-lhs

::

> s = square 1 # fc yellow
> x |-| y = x ||| strutX 0.5 ||| y
> example =  (s # showOrigin)
>        |-| (s # alignT  # showOrigin)
>        |-| (s # alignBR # showOrigin)

There are two things to note about the above example.  First, notice
how `alignT` and `alignBR` move the local origin of the square in the
way you would expect.  Second, notice that when placed "next to" each
other using the `(|||)` operator, the squares are placed so that their
local origins fall on a horizontal line.

Functions like `alignY` allow finer control over the alignment.  In
the below example, the origin is moved to a series of locations
interpolating between the bottom and top of the square:

.. class:: dia-lhs

::

> s = square 1 # fc yellow
> example = hcat . map showOrigin
>         $ zipWith alignY [-1, -0.8 .. 1] (repeat s)

Working with paths
------------------

.. container:: todo

  Write something general about paths

Segments
~~~~~~~~

The most basic path component is a `Segment`, which is some sort of
primitive path from one point to another.  Segments are
*translationally invariant*; that is, they have no inherent location,
and applying a translation to a segment has no effect (however, other
sorts of transformations, such as rotations and scales, have the
effect you would expect). In other words, a segment is not
a way to get from point A to point B; it is a way to get from
*wherever you are* to *somewhere else*.

Currently, ``diagrams`` supports
two types of segment, defined in `Diagrams.Segment`:mod:\:

* A *linear* segment is simply a straight line, defined by an offset
  from its beginning point to its end point; you can construct one
  using `straight`.

* A *Bézier* segment is a cubic curve defined by an offset from its
  beginning to its end, along with two control points; you can
  construct one using `bezier3`.  An example is shown below, with the
  endpoints shown in red and the control points in blue.  `Bézier
  curves`__ always start off from the beginning point heading towards
  the first control point, and end up at the final point heading away
  from the last control point.  That is, in any drawing of a Bézier
  curve like the one below, the curve will be tangent to the two
  dotted lines.

__ http://en.wikipedia.org/wiki/Bézier_curve

.. class:: dia-lhs

::

> illustrateBezier c1 c2 p2
>     =  endpt
>     <> endpt  # translate p2
>     <> ctrlpt # translate c1
>     <> ctrlpt # translate c2
>     <> l1
>     <> l2
>     <> fromSegments [bezier3 c1 c2 p2]
>   where
>     dashed  = dashing [0.1,0.1] 0
>     endpt   = circle 0.05 # fc red  # lw 0
>     ctrlpt  = circle 0.05 # fc blue # lw 0
>     l1      = fromOffsets [c1] # dashed
>     l2      = fromOffsets [p2 ^-^ c2] # translate c2 # dashed
>
> p2      = (3,-1) :: R2     -- endpoint
> [c1,c2] = [(1,2), (3,0)]   -- control points
>
> example = illustrateBezier c1 c2 p2

`Diagrams.Segment`:mod: also provides a few tools for working with
segments:

* `atParam` for computing points along a segment;
* `segOffset` for computing the offset from the start of a segment to its endpoint;
* `splitAtParam` for splitting a segment into two smaller segments;
* `arcLength` for approximating the arc length of a segment;
* `arcLengthToParam` for approximating the parameter corresponding to
  a given arc length along the segment; and
* `adjustSegment` for extending or shrinking a segment.

Trails
~~~~~~

`Trail`\s, defined in `Diagrams.Path`:mod:, are essentially lists of
segments laid end-to-end.  Since segments are translationally
invariant, so are trails; that is, trails have no inherent starting
location, and translating them has no effect.

Trails can also be *open* or *closed*: a closed trail is one with an
implicit (linear) segment connecting the endpoint of the trail to the
starting point.

To construct a `Trail`, you can use one of the following:

* `fromSegments` takes an explicit list of `Segment`\s.
* `fromOffsets` takes a list of vectors, and turns each one into a
  linear segment.
* `fromVertices` takes a list of vertices, generating linear segments
  between them.
* `(~~)` creates a simple linear trail between two points.
* `cubicSpline` creates a smooth curve passing through a given list of
  points; it is described in more detail in the section on `Splines`_.

If you look at the types of these functions, you will note that they
do not, in fact, return just `Trail`\s: they actually return any type
which is an instance of `PathLike`, which includes `Trail`\s, `Path`\s
(to be covered in the next section), `Diagram`\s, and lists of points.
See the `PathLike`_ section for more on the `PathLike` class.

Trails form a `Monoid` with *concatenation* as the binary operation,
and the empty (no-segment) trail as the identity element.  The example
below creates a two-segment trail called ``spike`` and then constructs
a starburst path by concatenating a number of rotated copies.
`strokeT` turns a trail into a diagram, with the start of the trail at
the local origin.

.. class:: dia-lhs

::

> spike :: Trail R2
> spike = fromOffsets [(1,3), (1,-3)]
>
> burst = mconcat . take 13 . iterate (rotateBy (-1/13)) $ spike
>
> example = strokeT burst # fc yellow # lw 0.1 # lc orange

For details on the functions provided for manipulating trails, see the
documentation for `Diagrams.Path`:mod:.  One other function worth
mentioning is `explodeTrail`, which turns each segment in a trail into
its own individual `Path`.  This is useful when you want to construct
a trail but then do different things with its individual segments.
For example, we could construct the same starburst as above but color
the edges individually:

.. class:: dia-lhs

::

> spike :: Trail R2
> spike = fromOffsets [(1,3), (1,-3)]
>
> burst = mconcat . take 13 . iterate (rotateBy (-1/13)) $ spike
>
> colors = cycle [aqua, orange, deeppink, blueviolet, crimson, darkgreen]
>
> example = lw 0.1
>         . mconcat
>         . zipWith lc colors
>         . map stroke . explodeTrail origin
>         $ burst

(If we wanted to fill the starburst with yellow as before, we would
have to separately draw another copy of the trail with a line width of
zero before exploding it; this is left as an exercise for the reader.)

Paths
~~~~~

A `Path`, also defined in `Diagrams.Path`:mod:, is a (possibly empty)
collection of trails, along with an absolute starting location for
each trail. Paths of a single trail can be constructed using the same
functions described in the previous section: `fromSegments`,
`fromOffsets`, `fromVertices`, `(~~)`, and `cubicSpline`.

`Path`\s also form a `Monoid`\, but the binary operation is
*superposition* (just like that of diagrams).  Paths with
multiple components can be used, for example, to create shapes with
holes:

.. class:: dia-lhs

::

> ring :: Path R2
> ring = circlePath 3 <> circlePath 2 # reversePath
>
> example = stroke ring # fc purple

`reversePath` is needed on the second segment because of the way path
filling is done; see `Fill rules`_.

`stroke` turns a path into a diagram, just as `strokeT` turns a trail
into a diagram. (In fact, `strokeT` really works by first turning the
trail into a path and then calling `stroke` on the result.)

`explodePath`, similar to `explodeTrail`, turns the segments of a path
into individual paths.  Since a path is a collection of trails, each
of which is a sequence of segments, `explodePath` actually returns a
list of lists of paths.

For information on other path manipulation functions such as
`pathFromTrail`, `pathFromTrailAt`, `pathVertices`, and `pathOffsets`,
see the documentation in `Diagrams.Path`:mod:.

Decorating trails and paths
~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. container:: todo

  * `decoratePath` and `decorateTrail` functions for placing items at
    the vertices of a path or trail

Fill rules
~~~~~~~~~~

.. container:: todo

   * Even-odd rule
   * Winding rule

.. _PathLike:

The ``PathLike`` class
~~~~~~~~~~~~~~~~~~~~~~

.. container:: todo

  * Explain `PathLike` class
  * Many functions can actually construct any `PathLike`
  * Convenient, but also have to be careful because it can change
    semantics (`Monoid` instances etc.)
  * note `strokeT` and `stroke` functions
  * Major exception: `circle`; use `circlePath` instead

Splines
~~~~~~~

Constructing Bézier segments by hand is tedious.  The
`Diagrams.CubicSpline`:mod: module provides the `cubicSpline`
function, which, given a list of points, constructs a smooth curved
path passing through each point in turn.  The first argument to
`cubicSpline` is a boolean value indicating whether the path should be
closed.

.. class:: dia-lhs

::

> pts = map P [(0,0), (2,3), (5,-2), (-4,1), (0,3)]
> dot = circle 0.2 # fc blue # lw 0
> mkPath closed = position (zip pts (repeat dot))
>              <> cubicSpline closed pts # lw 0.05
> example = mkPath False ||| strutX 2 ||| mkPath True

For more control over the generation of curved paths, see the
`diagrams-spiro`:pkg: package.

Clipping
~~~~~~~~

With backends that support clipping, paths can be used to *clip* other
diagrams.  Only the portion of a clipped diagram falling inside the
clipping path will be drawn.  Note that the diagram's bounding
function is unaffected.

.. class:: dia-lhs

::

> example = square 3
>         # fc green
>         # lw 0.05
>         # clipBy (square 3.2 # rotateBy (1/10))

Text
----

.. container:: todo

  * Creating text objects
  * Text objects take up no space
  * Font size
  * Other text attributes
  * Planned for future versions: better alignment, converting to paths

Images
------

Working with bounds
-------------------

.. container:: todo

  * `strut`, `pad`, `withBounds`, `phantom`

  * `width`, `height`, etc. from `Diagrams.TwoD.Util`:mod:

Named subdiagrams
-----------------

Using queries
-------------

Bounding boxes
--------------

Tools for backends
------------------

.. container:: todo

  * lots more stuff goes in this section

`Diagrams.Segment`:mod: exports a `FixedSegment` type, representing
segments which *do* have an inherent starting location. Trails and
paths can be "compiled" into lists of `FixedSegment`\s with absolute
locations using `fixTrail` and `fixPath`.  This is of interest to
authors of rendering backends that do not support relative drawing
commands.

Tips and tricks
---------------

Deciphering error messages
--------------------------

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

The ``Backend`` class
~~~~~~~~~~~~~~~~~~~~~

The ``Renderable`` class
~~~~~~~~~~~~~~~~~~~~~~~~

Cairo backend
=============
