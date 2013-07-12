.. role:: pkg(literal)
.. role:: hs(literal)
.. role:: mod(literal)
.. role:: repo(literal)

.. default-role:: hs

======================
 Diagrams User Manual
======================

.. contents::

Preliminaries
=============

Introduction
------------

``diagrams`` is a flexible, powerful embedded domain-specific language
(EDSL) for creating vector graphics and animations.  The ``diagrams``
framework is:

  * **Declarative**: you specify *what* a diagram is, not *how* to
    draw it.  ``diagrams`` takes care of the how.

  * **Compositional**: diagrams can be easily *combined* in many ways to
    produce more complex diagrams.

  * **Embedded**: the full power of Haskell_, including every library
    on Hackage_, is available to help construct and manipulate
    graphics.

.. _Haskell: http://haskell.org/
.. _Hackage: http://hackage.haskell.org/

  * **Extensible**: extending diagrams with additional or higher-level
    functionality is as simple as writing a Haskell module.

  * **Flexible**: diagrams is designed from the ground up to be as
    generic and flexible as possible, with support for pluggable
    rendering backends and multiple vector spaces (2D, 3D, ...).

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
be written is noted by a light blue box with a "document" icon on the
right hand side, like this:

.. container:: todo

  * Explain zygohistomorphic prepromorphisms
  * Essay on postmodernist critiques of ``diagrams`` vis-a-vis Kant

If you see a box like this in the place of something you would really
like to know about, please bug the developers (using the ``#diagrams`` IRC
channel on Freenode, or the `diagrams mailing list`_) so they can
prioritize it!

Warnings, "gotchas", and other important asides are in a yellow box with
a "warning" icon, like this:

.. container:: warning

   Diagrams is extremely addictive and may be hazardous to your
   health!

You would do well to pay special attention to the contents of such boxes.

Other resources
---------------

Here are some other resources that may be helpful to you as you learn
about ``diagrams``:

  * The API reference documentation for all the ``diagrams`` packages
    is intended to be high-quality and up-to-date, and is available
    `from the diagrams website`_.  If you find an omission, error, or
    something confusing, please `report it as a bug`_!
  * The ``diagrams`` website_ has a `gallery of examples`_ and links
    to tutorials, blog posts, and other documentation.
  * The `diagrams wiki`_ is a good place to find tips and tricks,
    examples, answers to frequently asked questions, and more.
  * The ``#diagrams`` IRC channel on Freenode is a friendly place
    where you can get help from other ``diagrams`` users and developers.
  * Consider joining the `diagrams mailing list`_ for discussions
    and announcements about ``diagrams``.
  * See the issue trackers in the `diagrams organization on github`_
    for a list of open tickets.  If you find a bug or would like to
    request a feature, please file a ticket!

.. _`from the diagrams website`: http://projects.haskell.org/diagrams/doc/index.html
.. _`report it as a bug`: https://github.com/diagrams/diagrams-doc/issues
.. _website: http://projects.haskell.org/diagrams
.. _`diagrams wiki`: http://haskell.org/haskellwiki/Diagrams
.. _`gallery of examples`: http://projects.haskell.org/diagrams/gallery.html
.. _`diagrams mailing list`: http://groups.google.com/group/diagrams-discuss?pli=1
.. _`developer wiki`: http://code.google.com/p/diagrams/
.. _`diagrams organization on github` : https://github.com/diagrams/

Installation
------------

Before installing ``diagrams``, you will need the following:

  * The `Glasgow Haskell Compiler`_ (GHC), version 6.12 or later
    (*except* 7.0.1, which has a type inference bug making
    ``diagrams`` hard to use).

  * It is recommended (but not required) to have the latest release of
    the `Haskell Platform`_ (currently 2012.4.0.0).  At the very least
    you will want the `cabal-install`_ tool.

.. _`cabal-install`: http://hackage.haskell.org/trac/hackage/wiki/CabalInstall

If you are on OS X or Windows, GHC itself comes with the Haskell
Platform; if you are on Linux, you will have to install GHC first.

.. _`Glasgow Haskell Compiler`: http://www.haskell.org/ghc/
.. _`Haskell Platform`: http://hackage.haskell.org/platform/

Once you have successfully installed the Haskell platform, installing
``diagrams`` should be as easy as issuing the command:

::

  cabal install diagrams

The `diagrams`:pkg: package is a convenience wrapper that simply pulls
in (by default) four other packages:

* `diagrams-core`:pkg: (core data
   type definitions and utilities),
* `diagrams-lib`:pkg: (standard primitives and combinators),
* `diagrams-contrib`:pkg: (user-contributed extensions), and
* `diagrams-svg`:pkg: (Haskell-native backend generating SVG files).

There is also a backend based on the `cairo graphics
library`_; it has support for more
features than the SVG backend and additional output formats (PNG, PS,
PDF), but can be much more difficult to install on some platforms
(notably OS X).  If you want the cairo backend, you can issue the
command

.. _`cairo graphics library`: http://www.cairographics.org/

::

  cabal install gtk2hs-buildtools -fcairo diagrams

(You can omit ``gtk2hs-buildtools`` if you have already installed it
previously.  If you don't want the SVG backend at all, you can add
the ``-f-svg`` flag.)

There is also a Haskell-native `postscript backend`_ which will soon
become officially supported but is currently unreleased; you are
welcome to build it from source and try it out.

.. _`postscript backend`: http://github.com/fryguybob/diagrams-postscript/

`See the wiki for the most up-to-date information`_ regarding
installation.  If you have trouble installing diagrams, feel free to
send email to the `diagrams mailing list`_; we would like to collect
reports of problems and solutions on various platforms.

.. _`See the wiki for the most up-to-date information`: http://www.haskell.org/haskellwiki/Diagrams/Install


Getting started
---------------

Create a file called ``TestDiagram.hs`` (or whatever you like) with
the following contents:

::

  {-# LANGUAGE NoMonomorphismRestriction #-}

  import Diagrams.Prelude
  import Diagrams.Backend.SVG.CmdLine
  -- or:
  -- import Diagrams.Backend.Cairo.CmdLine
  -- if using the Cairo backend

  main = defaultMain (circle 1)

The first line turns off the `dreaded monomorphism restriction`_, which is
quite important when using ``diagrams``: otherwise you will probably
run into lots of crazy error messages.

.. _`dreaded monomorphism restriction`: http://www.haskell.org/haskellwiki/Monomorphism_restriction

`Diagrams.Prelude`:mod: re-exports most everything from the standard
library; `Diagrams.Backend.SVG.CmdLine`:mod: provides a command-line
interface to the SVG rendering backend.

To compile your program, type

::

  $ ghc --make TestDiagram

(Note that the ``$`` indicates a command prompt and should not
actually be typed.)  Then execute ``TestDiagram`` with some
appropriate options:

::

  $ ./TestDiagram -w 100 -h 100 -o TestDiagram.svg

The above will generate a 100x100 SVG that should look like this:

.. class:: dia

::

> example = circle 1

(If you are using the cairo backend you can also request a ``.png``,
``.ps``, or ``.pdf`` file; the output type is automatically
determined by the extension.)

Try typing

::

  $ ./TestDiagram --help

to see the other options that are supported.

To get started quickly, you may wish to continue by reading the `quick
start tutorial`_; or you can continue reading the rest of this user
manual.

.. _`quick start tutorial`: http://projects.haskell.org/diagrams/tutorial/DiagramsTutorial.html

Contributing
------------

``diagrams`` is an open-source project, and contributions are
encouraged!  All diagrams-related repositories are in the `diagrams
organization`_ on github.  The `Contributing page`_ on the
diagrams wiki explains how to get the repositories and make
contributions.

.. _`diagrams organization`: http://github.com/diagrams
.. _`Contributing page`: http://www.haskell.org/haskellwiki/Diagrams/Contributing


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
operator `(<>)` as a synonym when built with versions of `base`:pkg:
prior to 4.5.  base-4.5 and newer provide `(<>)` in
`Data.Monoid`:mod:, which diagrams re-exports.

Monoids are used extensively in ``diagrams``: diagrams,
transformations, envelopes, traces, trails, paths, styles, colors,
and queries are all instances of `Monoid`.

Faking optional named arguments
-------------------------------

Many diagram-related operations can be customized in a wide variety of
ways.  For example, when creating a regular polygon, one can customize
the number of sides, the radius, the orientation, and so on. However,
to have a single function that takes all of these options as separate
arguments is a real pain: it's hard to remember what the arguments are
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
above are actually not necessary (this is a strange corner of Haskell
syntax but it works nicely for our purposes here).  Second,
``diagrams`` also defines `with` as a synonym for `def`, which makes
the syntax a bit more natural.  So, instead of the above, you could
write

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
magnitude, whereas a scalar represents only a magnitude.  Useful
operations on vectors and scalars include:

  * Adding and subtracting vectors with `(^+^)` and `(^-^)`
  * Multiplying a vector by a scalar with `(*^)`
  * Linearly interpolating between two vectors with `lerp`
  * Finding the `magnitude` (length) of a vector
  * Projecting one vector onto another with `project`.

See `Data.VectorSpace`:mod: for other useful functions and operators.

One might think we could also identify *points* in a space with
vectors having one end at the origin.  However, this turns out to be a
poor idea. There is a very important difference between vectors and
points: namely, vectors are translationally invariant whereas points
are not.  A vector represents a direction and magnitude, not a
location. Translating a vector has no effect. Points, on the other
hand, represent a specific location. Translating a point results in a
different point.

Although it is a bad idea to *conflate* vectors and points, we can
certainly *represent* points using vectors. The
`vector-space-points`:pkg: package defines newtype wrapper around
vectors called `Point`.  The most important connection between points
and vectors is given by `(.-.)`, defined in
`Data.AffineSpace`:mod:. If `p1` and `p2` are points, `p2 .-. p1` is
the vector giving the direction and distance from `p1` to `p2`.
Offsetting a point by a vector (resulting in a new point) is
accomplished with `(.+^)`.

Envelopes and local vector spaces
---------------------------------

In order to be able to position diagrams relative to one another, each
diagram must keep track of some bounds information.  Rather than use a
bounding box (which is neither general nor compositional) or even a
more general bounding *path* (which is rather complicated to deal
with), each diagram has an associated bounding *function*, called the
*envelope*.  Given some direction (represented by a vector) as input,
the envelope answers the question: "how far in this direction must one
go before reaching a perpendicular (hyper)plane that completely
encloses the diagram on one side of it?"

That's a bit of a mouthful, so hopefully the below illustration will
help clarify things if you found the above description confusing.
(For completeness, the code used to generate the illustration is
included, although you certainly aren't expected to understand it yet
if you are just reading this manual for the first time!)

.. class:: dia-lhs

::

> illustrateEnvelope v d
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
>       b  = envelopeP v d
>       v' = normalized v
>       p1 = b .+^ (rotateBy (1/4) v')
>       p2 = b .+^ (rotateBy (-1/4) v')
>
> d1 :: Path R2
> d1 = circle 1
>
> d2 :: Path R2
> d2 = (pentagon 1 === roundedRect 1.5 0.7 0.3)
>
> example = (stroke d1 # showOrigin <> illustrateEnvelope (r2 (-0.5,0.3)) d1)
>       ||| (stroke d2 # showOrigin <> illustrateEnvelope (r2 (0.5, 0.2)) d2)

The black arrows represent inputs to the envelopes for the
two diagrams; the envelopes' outputs are the distances
represented by the thick green lines.  The red lines illustrate the
enclosing (hyper)planes (which are really to be thought of as
extending infinitely to either side): notice how they are as close as
possible to the diagrams without intersecting them at all.

Of course, the *base point* from which the envelope is
measuring matters quite a lot!  If there were no base point, questions
of the form "*how far do you have to go...*" would be
meaningless---how far *from where*?  This base point (indicated by the
red dots in the diagram above) is called the *local origin* of a
diagram.  Every diagram has its own intrinsic *local vector space*;
operations on diagrams are always with respect to their local origin,
and you can affect the way diagrams are combined with one another by
moving their local origins.  The `showOrigin` function is provided as
a quick way of visualizing the local origin of a diagram (also
illustrated above).

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
can be used to make "local" modifications without requiring lots of
parentheses:

.. class:: lhs

::

> example =     square 2 # fc red # rotateBy (1/3)
>           ||| circle 1 # lc blue # fc green

Note how the modifiers `fc red` and `rotateBy (1/3)` apply only to the
square, and `lc blue` and `fc green` only to the circle (`(|||)` has a
precedence of 6, lower than that of `(#)`).

Types and type classes
----------------------

Diagrams has been designed with the goals of *flexibility* and
*power*.  The tradeoff is that it is far from simple, and the types
can be intimidating at first.  For example, `hcat` is a function which
takes a list of diagrams and lays them out in a horizontal row.  So
one might expect its type to be something like `[Diagram] ->
Diagram`.  In actuality, its type is

.. class:: lhs

::

  hcat :: (Juxtaposable a, HasOrigin a, Monoid' a, V a ~ R2) => [a] -> a

which may indeed be intimidating at first glance, and at any rate
takes a bit of time and practice to understand!  The essential idea is
to realize that `hcat` is actually quite a bit more general than
previously described: it can lay out not just diagrams, but any
two-dimensional things (``V a ~ R2``) which can be positioned "next
to" one another (`Juxtaposable`), can be translated (`HasOrigin`), and
are an instance of `Monoid` (`Monoid'` is actually a synonym for the
combination of `Monoid` and `Semigroup`).  This certainly includes
diagrams, but it also includes other things like paths, envelopes,
animations, and even tuples, lists, sets, or maps containing any of
these things.

At first, you may want to just try working through some examples
intuitively, without worrying too much about the types involved.
However, at some point you will of course want to dig deeper into
understanding the types, either to understand an error message (though
for help interpreting some common error messages, see `Deciphering
error messages`_) or to wield diagrams like a true type ninja.  When
that point comes, you should refer to `Understanding diagrams types`_
and the `Type class reference`_.

Creating 2D diagrams
====================

The main purpose of ``diagrams`` is to construct two-dimensional
vector graphics (although it can be used for more general purposes as
well).  This section explains the building blocks provided by
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

* `R2` is the type of the two-dimensional Euclidean vector space. The
  positive `x`:math:\-axis extends to the right, and the positive
  `y`:math:\-axis extends *upwards*.  This is consistent with standard
  mathematical practice, but upside-down with respect to many common
  graphics systems.  This is intentional: the goal is to provide an
  elegant interface which is abstracted as much as possible from
  implementation details.

  `unitX` and `unitY` are unit vectors in the positive `x`:math:\- and
  `y`:math:\-directions, respectively.  Their negated counterparts are
  `unit_X` and `unit_Y`.

  Vectors of type `R2` can be created by passing a pair of type
  `(Double, Double)` to the function `r2`; vectors can likewise be
  converted back into pairs using `unr2`.

  Vectors can also be constructed and pattern-matched using the
  utilities defined in `Diagrams.Coordinates`:mod:, which provides a
  uniform interface for constructing points and vectors of any
  dimension.  Vectors can be created using the syntax `(x & y)` and
  pattern-matched by calling `coords` and then matching on the pattern
  `(x :& y)`.

* `P2` is the type of points in two-dimensional space. It is a synonym
  for `Point R2`.  The distinction between points and vectors is
  important; see `Vectors and points`_.

  Points can be created from pairs of coordinates using `p2` and
  converted back using `unp2`. They can also be constructed and
  destructed using the same syntax as for vectors, as defined in
  `Diagrams.Coordinates`:mod:.

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
* `roundedRect'` works like `roundedRect` but allowing a different radius to be set for each corner, using `RoundedRectOpts`.

.. class:: dia-lhs

::

> example = square 1 ||| rect 0.3 0.5
>       ||| eqTriangle 1
>       ||| roundedRect  0.5 0.4 0.1
>       ||| roundedRect  0.5 0.4 (-0.1)
>       ||| roundedRect' 0.7 0.4 with { radiusTL = 0.2
>                                     , radiusTR = -0.2
>                                     , radiusBR = 0.1 }

More special polygons may be added in future versions of the library.

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

As its second argument, `star` expects a list of points.  One way to
generate a list of points is with polygon-generating functions such as
`polygon` or `regPoly`, or indeed, any function which can output any
`PathLike` type (see the section about `PathLike`_), since a list of
points is an instance of the `PathLike` class.  But of course, you are
free to construct the list of points using whatever method you like.

As its first argument, `star` takes a value of type `StarOpts`, for
which there are two possibilities:

* `StarSkip` specifies that every :math:`n` th vertex should be
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

Composing diagrams
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

> example = beside (r2 (20,30))
>                  (circle 1 # fc orange)
>                  (circle 1.5 # fc purple)
>           # showOrigin

As can be seen from the above example, the *length* of the vector
makes no difference, only its *direction* is taken into account. (To
place diagrams at a certain fixed distance from each other, see
`cat'`.)  As can also be seen, the local origin of the new, combined
diagram is the same as the local origin of the first diagram.  (This
makes `beside v` associative, so diagrams under `beside v` form a
semigroup---but *not* a monoid, since there is no identity
element. `mempty` is a right but not a left identity for `beside v`.)

In older versions of ``diagrams``, the local origin of the combined
diagram was at the point of tangency between the two diagrams.  To
recover the old behavior, simply perform an alignment on the first in
the same direction before combining (see `Alignment`_):

.. class:: dia-lhs

::

> example = beside (r2 (20,30))
>                  (circle 1   # fc orange # align (r2 (20,30)))
>                  (circle 1.5 # fc purple)
>           # showOrigin

If you want to place two diagrams next to each other using the local
origin of the *second* diagram, you can use something like `beside' =
flip . beside . negateV`, that is, use a vector in the opposite
direction and give the diagrams in the other order.

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

Juxtaposing without composing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Sometimes, one may wish to *position* a diagram next to another
diagram without actually composing them.  This can be accomplished
with the `juxtapose` function.  In particular, `juxtapose v d1 d2`
returns a modified version of `d2` which has been translated to be
next to `d1` in the direction of `v`.  (In fact, `beside` itself is
implemented as a call to `juxtapose` followed by a call to `(<>)`.)

.. class:: dia-lhs

::

> d1 = juxtapose unitX             (square 1) (circle 1 # fc red)
> d2 = juxtapose (unitX ^+^ unitY) (square 1) (circle 1 # fc green)
> d3 = juxtapose unitY             (square 1) (circle 1 # fc blue)
> example = mconcat [d1, d2, d3]

See `envelopes and local vector spaces`_ for more information on what
"next to" means, `Working with envelopes`_ for information on
functions available for manipulating envelopes, or the section on
`Diagrams.Core.Envelope`_ for precise details.

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
>         mkPoint x = p2 (x,x^2)

`cat` is an iterated version of `beside`, which takes a direction
vector and a list of diagrams, laying out the diagrams beside one
another in a row.  The local origins of the subdiagrams will be placed
along a straight line in the direction of the given vector, and the
local origin of the first diagram in the list will be used as the
local origin of the final result.

.. class:: dia-lhs

::

> example = cat (r2 (2,-1)) (map p [3..8]) # showOrigin
>   where p n = regPoly n 1 # lw 0.03

Semantically speaking, `cat v === foldr (beside v) mempty`, although
the actual implementation of `cat` uses a more efficient balanced fold.

For more control over the way in which the diagrams are laid out, use
`cat'`, a variant of `cat` which also takes a `CatOpts` record.  See
the documentation for `cat'` and `CatOpts` to learn about the various
possibilities.

.. class:: dia-lhs

::

> example = cat' (r2 (2,-1)) with { catMethod = Distrib, sep = 2 } (map p [3..8])
>   where p n = regPoly n 1 # lw 0.03
>                           # scale (1 + fromIntegral n/4)
>                           # showOrigin

For convenience, `Diagrams.TwoD.Combinators`:mod: also provides `hcat`, `hcat'`,
`vcat`, and `vcat'`, variants of `cat` and `cat'` which concatenate
diagrams horizontally and vertically.

Finally, `appends` is like an iterated variant of `beside`, with the
important difference that multiple diagrams are placed next to a
single central diagram without reference to one another; simply
iterating `beside` causes each of the previously appended diagrams to
be taken into account when deciding where to place the next one.  Of
course, `appends` is implemented in terms of `juxtapose` (see
`Juxtaposing without composing`_).

.. class:: dia-lhs

::

> c        = circle 1 # lw 0.03
> dirs     = iterate (rotateBy (1/7)) unitX
> cdirs    = zip dirs (replicate 7 c)
> example1 = appends c cdirs
> example2 = foldl (\a (v,b) -> beside v a b) c cdirs
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
attributes as well; for details, see the section on `Diagrams.Core.Style`_.

In many examples, you will see attributes applied to diagrams using
the `(#)` operator.  Keep in mind that there is nothing special about
this operator as far as attributes are concerned. It is merely
backwards function application, which is used for attributes since it
often reads better to have the main diagram come first, followed by
modifications to its attributes.  See `Postfix transformation`_.

In general, inner attributes (that is, attributes applied earlier)
override outer ones.  Note, however, that this is not a requirement.
Each attribute may define its own specific method for combining
multiple values.  See the section on `Diagrams.Core.Style`_ for more
details.

Most of the attributes discussed in this section are defined in
`Diagrams.Attributes`:mod:.

Color
+++++

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

To "set the background color" of a diagram, use the `bg`
function---which does not actually set any attributes, but simply
superimposes the diagram on top of a bounding rectangle of the given
color.

.. class:: dia-lhs

::

> t = regPoly 3 1
>
> example = t ||| t # bg orange

Line width
++++++++++

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

Note that line width does not affect the envelope of diagrams
at all.  Future versions of the standard library may provide a
function to convert a stroked path into an actual region, which would
allow line width to be taken into account.

Other line parameters
+++++++++++++++++++++

Many rendering backends provide some control over the particular way
in which lines are drawn.  Currently, ``diagrams`` provides support
for three aspects of line drawing:

* `lineCap` sets the `LineCap` style.
* `lineJoin` sets the `LineJoin` style.
* `dashing` allows for drawing dashed lines with arbitrary dashing
  patterns.

.. class:: dia-lhs

::

> path = fromVertices (map p2 [(0,0), (1,0.3), (2,0), (2.2,0.3)]) # lw 0.1
> example = centerXY . vcat' with { sep = 0.1 }
>           $ map (path #)
>             [ lineCap LineCapButt   . lineJoin LineJoinMiter
>             , lineCap LineCapRound  . lineJoin LineJoinRound
>             , lineCap LineCapSquare . lineJoin LineJoinBevel
>             , dashing [0.1,0.2,0.3,0.1] 0
>             ]

The ``HasStyle`` class
++++++++++++++++++++++

Functions such as `fc`, `lc`, `lw`, and `lineCap` do not take only
diagrams as arguments.  They take any type which is an instance of the
`HasStyle` type class.  Of course, diagrams themselves are an
instance.

However, the `Style` type is also an instance.  This is useful in
writing functions which offer the caller flexible control over the
style of generated diagrams.  The general pattern is to take a `Style`
(or several) as an argument, then apply it to a diagram along with
some default attributes:

.. class:: lhs

::

> myFun style = d # applyStyle style # lc red # ...
>   where d = ...

This way, any attributes provided by the user in the `style` argument
will override the default attributes specified afterwards.

To call `myFun`, a user can construct a `Style` by starting with an
empty style (`mempty`, since `Style` is an instance of `Monoid`) and
applying the desired attributes:

.. class:: lhs

::

> foo = myFun (mempty # fontSize 10 # lw 0 # fc green)

If the type `T` is an instance of `HasStyle`, then `[T]` is also.
This means that you can apply styles uniformly to entire lists of
diagrams at once, which occasionally comes in handy.  Likewise, there
are `HasStyle` instances for pairs, `Map`\s, `Set`\s, and functions.

2D Transformations
~~~~~~~~~~~~~~~~~~

Any diagram can be transformed by applying arbitrary affine
transformations to it. *Affine* transformations include *linear*
transformations (rotation, scaling, reflection, shears---anything
which leaves the origin fixed and sends lines to lines) as well as
translations.  `Diagrams.TwoD.Transform`:mod: defines a number of
common affine transformations in two-dimensional space. (To construct
transformations more directly, see
`Diagrams.Core.Transform`:mod:.)

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
++++++++++++++++++++++++++

Before looking at specific two-dimensional transformations, it's worth
saying a bit about transformations in general (a fuller treatment can
be found in the section on `Diagrams.Core.Transform`_).  The
`Transformation` type is defined in `Diagrams.Core.Transform`:mod:,
from the `diagrams-core`:pkg: package.  `Transformation` is
parameterized by the vector space over which it acts; recall that `T2`
is provided as a synonym for `Transformation R2`.

`Transformation v` is a `Monoid` for any vector space `v`:

* `mempty` is the identity transformation;
* `mappend` is composition of transformations: `t1 \`mappend\` t2`
  (also written `t1 <> t2`) performs first `t2`, then `t1`.

To invert a transformation, use `inv`.  For any transformation `t`,

`t <> inv t === inv t <> t === mempty`.

To apply a transformation to a diagram, use `transform`.

Rotation
++++++++

Use `rotate` to rotate a diagram couterclockwise by a given angle__
about the origin.  Since `rotate` takes an angle, you must specify an
angle type, such as `rotate (80 :: Deg)`.  In the common case that you
wish to rotate by an angle specified as a certain fraction of a
circle, like `rotate (1/8 :: CircleFrac)`, you can use `rotateBy`
instead. `rotateBy` is specialized to only accept fractions of a
circle, so in this example you would only have to write
`rotateBy (1/8)`.

You can also use `rotateAbout` in the case that you want to rotate
about some point other than the origin.

__ `Angles`_

.. class:: dia-lhs

::

> eff = text "F" <> square 1 # lw 0
> rs  = map rotateBy [1/7, 2/7 .. 6/7]
> example = hcat . map (eff #) $ rs

Scaling and reflection
++++++++++++++++++++++

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
>        <> reflectAbout (p2 (0.2,0.2)) (rotateBy (-1/10) unitX) eff

Translation
+++++++++++

Translation is achieved with `translate`, `translateX`, and
`translateY`, which should be self-explanatory.

Conjugation
+++++++++++

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

.. _`The Transformable class`:

The ``Transformable`` class
+++++++++++++++++++++++++++

Transformations can be applied not just to diagrams, but values of any
type which is an instance of the `Transformable` type class.
Instances of `Transformable` include vectors, points, trails, paths,
envelopes, and `Transformations` themselves.  In addition,
tuples, lists, maps, or sets of `Transformable` things are also
`Transformable` in the obvious way.

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
its envelope.  To this end, some general tools are provided
in `Diagrams.Align`:mod:, and specialized 2D-specific ones by
`Diagrams.TwoD.Align`:mod:.

Functions like `alignT` (align Top) and `alignBR` (align Bottom Right)
move the local origin to the edge of the envelope:

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

Paths are one of the most fundamental tools in ``diagrams``.  They can
be used not only directly to draw things, but also as guides to help
create and position other diagrams.

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

> illustrateBezier c1 c2 x2
>     =  endpt
>     <> endpt  # translate x2
>     <> ctrlpt # translate c1
>     <> ctrlpt # translate c2
>     <> l1
>     <> l2
>     <> fromSegments [bezier3 c1 c2 x2]
>   where
>     dashed  = dashing [0.1,0.1] 0
>     endpt   = circle 0.05 # fc red  # lw 0
>     ctrlpt  = circle 0.05 # fc blue # lw 0
>     l1      = fromOffsets [c1] # dashed
>     l2      = fromOffsets [x2 ^-^ c2] # translate c2 # dashed
>
> x2      = r2 (3,-1) :: R2         -- endpoint
> [c1,c2] = map r2 [(1,2), (3,0)]   -- control points
>
> example = illustrateBezier c1 c2 x2

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

* `fromOffsets` takes a list of vectors, and turns each one into a
  linear segment.
* `fromVertices` takes a list of vertices, generating linear segments
  between them.
* `(~~)` creates a simple linear trail between two points.
* `cubicSpline` creates a smooth curve passing through a given list of
  points; it is described in more detail in the section on `Splines`_.
* `fromSegments` takes an explicit list of `Segment`\s.

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
> spike = fromOffsets . map r2 $ [(1,3), (1,-3)]
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
> spike = fromOffsets . map r2 $ [(1,3), (1,-3)]
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
zero and fill that; this is left as an exercise for the reader.)

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
> ring = circle 3 <> circle 2
>
> example = stroke ring # fc purple # fillRule EvenOdd

(See `Fill rules`_ for an explanation of the call to `fillRule
EvenOdd`.)

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

Stroking trails and paths
~~~~~~~~~~~~~~~~~~~~~~~~~

The `strokeT` and `stroke` functions, which turn trails and paths into
diagrams respectively, have already been mentioned; they are defined
in `Diagrams.TwoD.Path`:mod:.  Both also have primed variants,
`strokeT'` and `stroke'`, which take a record of `StrokeOpts`.
Currently, `StrokeOpts` has two fields:

* `vertexNames` takes a list of lists of names, and zips each list
  with a component of the path, creating point subdiagrams (using
  `pointDiagram`) associated with the names.  This means that the
  names can be used to later refer to the locations of the path
  vertices (see `Named subdiagrams`_).  In the case of `strokeT'`,
  only the first list is used.

  By default, `vertexNames` is an empty list.

* `queryFillRule` specifies the fill rule (see `Fill rules`_) used to
  determine which points are inside the diagram, for the purposes of
  its query (see `Using queries`_).  Note that it does *not* affect
  how the diagram is actually drawn; for that, use the `fillRule`
  function.  (This is not exactly a feature, but for various technical
  reasons it is not at all obvious how to have this field actually
  affect both the query and the rendering of the diagram.)

  By default, `queryFillRule` is set to `Winding`.

Decorating trails and paths
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Paths (and trails) can be used not just to draw certain shapes, but
also as tools for positioning other objects.  To this end,
``diagrams`` provides `decoratePath` and `decorateTrail`, which
position a list of objects at the vertices of a given path or trail,
respectively.

For example, suppose we want to create an equilateral triangular
arrangement of dots.  One possibility is to create horizontal rows of
dots, center them, and stack them vertically.  However, this is
annoying, because we must manually compute the proper vertical
stacking distance between rows. Whether you think this sounds easy or
not, it is certainly going to involve the `sqrt` function, or perhaps
some trig, or both, and we'd rather avoid all that.

Fortunately, there's an easier way: after creating the horizontal
rows, we create the path corresponding to the left-hand side of the
triangle (which can be done using a simple rotation), and then
decorate it with the rows.

.. class:: dia-lhs

::

> dot = circle 1 # fc black
> mkRow n = hcat' with {sep = 0.5} (replicate n dot)
> mkTri n = decoratePath
>             (fromOffsets (replicate (n-1) (2.5 *^ unitX))
>                # rotateBy (1/6))
>             (map mkRow [n, n-1 .. 1])
> example = mkTri 5

.. _PathLike:

The ``PathLike`` class
~~~~~~~~~~~~~~~~~~~~~~

As you may have noticed by now, a large class of functions in the
standard library---such as `square`, `polygon`, `fromVertices`, and so
on---generate not just diagrams, but *any* type which is an instance
of the `PathLike` type class.

The `PathLike` type class has only a single method, `pathLike`:

.. class:: lhs

::

> pathLike :: Point (V p)
>          -> Bool
>          -> [Segment (V p)]
>          -> p

* The first argument is a starting point for the path-like thing;
  path-like things which are translationally invariant (such as
  `Trail`\s) simply ignore this argument.

* The second argument indicates whether the path-like thing should be
  closed.

* The third argument specifies the segments of the path-like thing.

Currently, there are four instances of `PathLike`:

* `Trail`: as noted before, the implementation of `pathLike` for
  `Trail`\s ignores the first argument, since `Trail`\s have no inherent
  starting location.
* `Path`: of course, `pathLike` can only construct paths of a single
  component.
* `Diagram b R2`: as long as the backend `b` knows how to render 2D
  paths, `pathLike` can construct a diagram by stroking the generated
  single-component path.
* `[Point v]`: this instance generates the vertices of the path.

It is quite convenient to be able to use, say, `square 2` as a
diagram, path, trail, or list of vertices, whichever suits one's
needs.  Otherwise, either four different functions would be needed for
each primitive (like ``square``, ``squarePath``, ``squareTrail``, and
``squareVertices``, ugh), or else explicit conversion functions would
have to be inserted when you wanted something other than what the
`square` function gave you by default.

As an (admittedly contrived) example, the following diagram defines
`s` as an alias for `square 2` and then uses it at all four instances of
`PathLike`:

.. class:: dia-lhs

::

> s = square 2  -- a squarish thing.
>
> blueSquares = decoratePath s {- 1 -}
>                 (replicate 4 (s {- 2 -} # scale 0.5) # fc blue)
> paths       = lc purple . stroke $ star (StarSkip 2) s {- 3 -}
> aster       = centerXY . lc green . strokeT
>             . mconcat . take 5 . iterate (rotateBy (1/5))
>             $ s {- 4 -}
> example = (blueSquares <> aster <> paths) # lw 0.05

Exercise: figure out which occurrence of `s` has which type. (Answers
below.)

At its best, this type-directed behavior results in a "it just
works/do what I mean" experience.  However, it can occasionally be
confusing, and care is needed.  The biggest gotcha occurs when
combining a number of shapes using `(<>)` or `mconcat`: diagrams,
paths, trails, and lists of vertices all have `Monoid` instances, but
they are all different, so the combination of shapes has different
semantics depending on which type is inferred.

.. class:: dia-lhs

::

> ts = mconcat . take 3 . iterate (rotateBy (1/9)) $ eqTriangle 1
> example = (ts ||| stroke ts ||| strokeT ts ||| fromVertices ts) # fc red

The above example defines `ts` by generating three equilateral
triangles offset by 1/9 rotations, then combining them with `mconcat`.
The sneaky thing about this is that `ts` can have the type of any
`PathLike` instance, and it has completely different meanings
depending on which type is chosen.  The example uses `ts` at each of
the four `PathLike` types:

* Since `example` is a diagram, the first `ts`, used by itself, is
  also a diagram; hence it is interpreted as three equilateral
  triangle diagrams superimposed on one another with `atop`.

* `stroke` turns `Path`\s into diagrams, so the second `ts` has type
  `Path R2`.  Hence it is interpreted as three triangular paths
  superimposed into one three-component path, which is then stroked.

* `strokeT` turns `Trail`\s into diagrams, so the third occurrence of
  `ts` has type `Trail R2`.  It is thus interpreted as three
  triangular trails (*without* the implicit closing segments)
  sequenced end-to-end into one long trail.

* The last occurrence of `ts` is a list of points, namely, the
  concatenation of the vertices of the three triangles.  Turning this
  into a diagram with `fromVertices` generates a single-component,
  open path that visits each of the points in turn.  The generated
  diagram looks passingly similar to the one from the second
  occurrence of `ts`, but a careful look reveals that they are quite
  different.

Of course, one way to avoid all this would be to give `ts` a specific
type signature, if you know which type you would like it to be.  Then
using it at a different type will result in a type error, rather than
confusing semantics.

Answers to the `square 2` type inference challenge:

#. `Path R2`
#. `Diagram b R2`
#. `[P2]`
#. `Trail R2`

The ``Closeable`` class
~~~~~~~~~~~~~~~~~~~~~~~

Creating closed paths can be accomplished with the `close` method of
the `Closeable` type class.  There is also an `open` method, which
does what you would think.  Currently, there are only two instances of
`Closeable`: `Trail` and `Path`.

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

> pts = map p2 [(0,0), (2,3), (5,-2), (-4,1), (0,3)]
> dot = circle 0.2 # fc blue # lw 0
> mkPath closed = position (zip pts (repeat dot))
>              <> cubicSpline closed pts # lw 0.05
> example = mkPath False ||| strutX 2 ||| mkPath True

For more control over the generation of curved paths, see
`diagrams-spiro`_, which provides an FFI binding to the ``spiro``
library (the cubic spline library used by Inkscape).

.. _diagrams-spiro: http://patch-tag.com/r/fryguybob/diagrams-spiro

Fill rules
~~~~~~~~~~

There are two main algorithms or "rules" used when determining which
areas to fill with color when filling the interior of a path: the
*winding rule* and the *even-odd rule*.  The rule used to draw a
path-based diagram can be set with `fillRule`, defined in
`Diagrams.TwoD.Path`:mod:. For simple, non-self-intersecting paths,
determining which points are inside is quite simple, and the two
algorithms give the same results. However, for self-intersecting
paths, they usually result in different regions being filled.

.. class:: dia-lhs

::

> loopyStar = fc red
>           . mconcat . map (cubicSpline True)
>           . pathVertices
>           . star (StarSkip 3)
>           $ regPoly 7 1
> example = loopyStar # fillRule EvenOdd
>       ||| strutX 1
>       ||| loopyStar # fillRule Winding

* The *even-odd rule* specifies that a point is inside the path if a
  straight line extended from the point off to infinity (in one
  direction only) crosses the path an odd number of times.  Points
  with an even number of crossings are outside the path.  This rule is
  simple to implement and works perfectly well for
  non-self-intersecting paths.  For self-intersecting paths, however,
  it results in a funny pattern of alternatingly filled and unfilled
  regions, as seen in the above example.  Sometimes this pattern is
  desirable for its own sake.

* The *winding rule* specifies that a point is inside the path if its
  *winding number* is nonzero.  The winding number measures how many
  times the path "winds" around the point, and can be intuitively
  computed as follows: imagine yourself standing at the given point,
  facing some point on the path.  You hold one end of an (infinitely
  stretchy) rope; the other end of the rope is attached to a train
  sitting at the point on the path at which you are looking.  Now the
  train begins traveling around the path. As it goes, you keep hold of
  your end of the rope while standing fixed in place, not turning at
  all.  After the train has completed one circuit around the path,
  look at the rope: if it is wrapped around you some number of times,
  you are inside the path; if it is not wrapped around you, you are
  outside the path.  More generally, we say that the number of times
  the rope is wrapped around you (positive for one direction and
  negative for the other) is the point's winding number.

  .. container:: todo

      Draw a picture of you and the train

  For example, if you stand outside a circle looking at a train
  traveling around it, the rope will move from side to side as the
  train goes around the circle, but ultimately will return to exactly
  the state in which it started.  If you are standing inside the
  circle, however, the rope will end up wrapped around you once.

  For paths with multiple components, the winding number is simply the
  sum of the winding numbers for the individual components.  This
  means, for example, that "holes" can be created in shapes using a
  path component traveling in the *opposite direction* from the outer
  path.

  This rule does a much better job with self-intersecting paths, and
  it turns out to be (with some clever optimizations) not much more
  difficult to implement or inefficient than the even-odd rule.

Clipping
~~~~~~~~

With backends that support clipping, paths can be used to *clip* other
diagrams.  Only the portion of a clipped diagram falling inside the
clipping path will be drawn.  Note that the diagram's envelope is
unaffected.

.. class:: dia-lhs

::

> example = square 3
>         # fc green
>         # lw 0.05
>         # clipBy (square 3.2 # rotateBy (1/10))

Altering a diagram's envelope can be accomplished using `withEnvelope`
(see `Envelope-related functions`_).  The `view` function is also
provided for the special case of setting a diagram's envelope to some
rectangle, often used for the purpose of selecting only a part of a
diagram to be "viewed" in the final output.  It takes a point---the
lower-left corner of the viewing rectangle---and the vector from the
lower-left to upper-right corner.

.. class:: dia-lhs

::

> circles = (c ||| c) === (c ||| c) where c = circle 1 # fc fuchsia
> example = circles # centerXY # view (p2 (-1,-1)) (r2 (1.3, 0.7))

Note in the above example how the actual portion of the diagram that
ends up being visible is larger than the specification given to
`view`---this is because the aspect ratio of the requested output
image does not match the aspect ratio of the rectangle given to
`view` (and also because of the use of `pad` by the framework which
renders the user manual examples).  If the aspect ratios matched the
viewed portion would be exactly that specified in the call to `view`.

Text
----

Text objects, defined in `Diagrams.TwoD.Text`:mod:, can be created
most simply with the `text` function, which turns a `String` into a
diagram with (centered) text:

.. class:: dia-lhs

::

> example = text "Hello world!" <> rect 8 1

Text with different alignments can be created using `topLeftText`,
`baselineText`, or, more generally, `alignedText`:

.. class:: dia-lhs

::

> pt = circle 0.1 # fc red
>
> t1 = pt <> topLeftText         "top left"   <> rect 8 1
> t2 = pt <> baselineText        "baseline"   <> rect 8 1
> t3 = pt <> alignedText 0.7 0.5 "(0.7, 0.5)" <> rect 8 1
>
> d1 =/= d2 = d1 === strutY 2 === d2
> example = t1 =/= t2 =/= t3

The most important thing to keep in mind when working with text
objects is that they *take up no space*; that is, the envelope for a
text object is constantly zero.  If we omitted the rectangle from the
above example, there would be no output.

.. container:: warning

   Text objects take up no space!

There are two reasons for this.  First, computing the size of some
text in a given font is rather complicated, and ``diagrams`` cannot
(yet) do it natively.  The cairo backend can do it (see below) but we
don't want to tie diagrams to a particular backend.

The second reason is that font size is handled similarly to line
width, so the size of a text object cannot be known at the time of its
creation anyway!  (Future versions of ``diagrams`` may include some
sort of constraint-solving engine to be able to handle this sort of
situation, but don't hold your breath.)  Font size is treated
similarly to line width for a similar reason: we often want disparate
text elements to be the same size, but those text elements may be part
of subdiagrams that have been transformed in various ways.

Note, however, that the cairo backend includes a module
`Diagrams.Backend.Cairo.Text`:mod: with functions for querying font
and text extents, and creating text diagrams that take up an
appropriate amount of space.  So it *is* possible to have
automatically-sized text objects, at the cost of being tied to the
cairo backend and bringing `IO` into the picture (or being at peace
with some probably-justified uses of `unsafePerformIO`).

To set the font size, use the `fontSize` function; the default font
size is (arbitrarily) 1.  Remember, however, that the font size is
measured in the *final* vector space of the diagram, rather than in
the local vector space in effect at the time of the text's creation.

Other attributes of text can be set using `font`, `bold` (or, more
generally, `fontWeight`), `italic`, and `oblique` (or, more generally,
`fontSlant`).  Text is colored with the current fill color (see
`Color`_).

.. class:: dia-lhs

::

> text' s t = text t # fontSize s <> strutY (s * 1.3)
> example = centerXY $
>       text' 10 "Hello" # italic
>   === text' 5 "there"  # bold # font "freeserif"
>   === text' 3 "world"  # fc green

Native font support
~~~~~~~~~~~~~~~~~~~

The `SVGFonts package`_
implements native SVG font support for diagrams.  Among other things,
it provides its own `textSVG` function which can be used to convert
text into a *path* tracing the outline of the text.  This may
eventually be merged into the main diagrams codebase; for now, you can
just import it if you need fancier font support.

.. _`SVGFonts package`: http://hackage.haskell.org/package/SVGFonts

Images
------

The `Diagrams.TwoD.Image`:mod: module provides basic support for
including external images in diagrams.  Simply use the `image`
function and specify a file name and size for the image:

.. class:: dia-lhs

::

> no = (circle 1 <> hrule 2 # rotateBy (1/8))
>    # lw 0.2 # lc red
> example = no <> image "manual/static/phone.png" 1.5 1.5

Unfortunately, you must specify both a width and a height for each
image.  You might hope to be able to specify just a width or just a
height, and have the other dimension computed so as to preserve the
image's aspect ratio.  However, there is no way for ``diagrams`` to
query an image's aspect ratio until rendering time, but (until such
time as a constraint solver is added) it needs to know the size of the
image when composing it with other subdiagrams.  Hence, both
dimensions must be specified, and for the purposes of positioning
relative to other diagrams, the image will be assumed to occupy a
rectangle of the given dimensions.

However, note that the image's aspect ratio will be preserved: if you
specify dimensions that do not match the actual aspect ratio of the
image, blank space will be left in one of the two dimensions to
compensate.  If you wish to alter an image's aspect ratio, you can do
so by scaling nonuniformly with `scaleX`, `scaleY`, or something
similar.

Currently, the cairo backend can only include images in ``.png``
format, but hopefully this will be expanded in the future.  Other
backends may be able to handle other types of external images.

Advanced tools for diagram creation
===================================

This section covers some of the more advanced tools provided by the
core and standard libraries for constructing diagrams.  Most of the
content in this section is applicable to diagrams in any vector space,
although 2D diagrams are used as illustrations.

Working with envelopes
----------------------

The `Envelope` type, defined in
`Diagrams.Core.Envelope`:mod:, encapsulates *envelopes*
(see `envelopes and local vector spaces`_).  Things which have an
associated envelope---including diagrams, segments, trails, and
paths---are instances of the `Enveloped` type class.

Envelopes are used implicitly when placing diagrams next to
each other (see `Juxtaposing diagrams`_) or when aligning diagrams
(see `Alignment`_).

Envelope-related functions
~~~~~~~~~~~~~~~~~~~~~~~~~~

* `strut` creates a diagram which produces no output but takes up the
  same space as a line segment.  There are also versions specialized
  to two dimensions, `strutX` and `strutY`.  These functions are
  useful for putting space in between diagrams.

  .. class:: dia-lhs

  ::

  > example = circle 1 ||| strutX 2 ||| square 2

* `pad` increases the envelope of a diagram by a certain
  factor in all directions.

  .. class:: dia-lhs

  ::

  > surround d = c === (c ||| d ||| c) # centerXY === c
  >   where c = circle 0.5
  >
  > example = surround (square 1) ||| strutX 1
  >       ||| surround (pad 1.2 $ square 1)

  However, the behavior of `pad` often trips up first-time users of
  ``diagrams``:

  .. container:: warning

     `pad` expands the envelope *relative to the local
     origin*.  So if you want the padding to be equal on all sides, use
     `centerXY` first.

  For example,

  .. class:: dia-lhs

  ::

  > surround d = c === (c ||| d ||| c) # centerXY === c
  >   where c = circle 0.5
  >
  > p = strokeT (square 1)
  >
  > example = surround (pad 1.2 $ p # showOrigin) ||| strutX 1
  >       ||| surround (pad 1.2 $ p # centerXY # showOrigin)

* Manually setting the envelope of a diagram can be
  accomplished using `withEnvelope`.  Additionally, `phantom` can be
  used to create a diagram which produces no output but takes up a
  certain amount of space, for use in positioning other diagrams.

  .. class:: dia-lhs

  ::

  > example = hcat [ square 2
  >                , circle 1 # withEnvelope (square 3 :: D R2)
  >                , square 2
  >                , text "hi" <> phantom (circle 2 :: D R2)
  >                ]

  In the above example, `withEnvelope` is used to put more space
  surrounding the circle, and `phantom` is used to put space around
  `text "hi"` (which would otherwise take up no space).  Note that we
  could equally well have written
  `text "hi" # withEnvelope (circle 2 :: D R2)`.  Notice that the
  `D R2` annotations are necessary, since otherwise GHC will not know
  what types to pick for `square 3` and `circle 2`.  See `No instances
  for Backend b0 R2 ...`_ for more information.

* `Diagrams.TwoD.Size`:mod: provides functions for extracting
  information from the envelopes of two-dimensional diagrams,
  such as `width`, `height`, `extentX`, `extentY`, and `center2D`.

  It also provides functions `sized` and `sizedAs`, which can be used
  for changing the size of an object.  For example:

  .. class:: dia-lhs

  ::

  > shapes = circle 1
  >      ||| square 2
  >      ||| circle 1 # scaleY 0.3 # sizedAs (square 2 :: D R2)
  >
  > example = hrule 1 # sizedAs (shapes # scale 0.5 :: D R2) <> shapes # centerX

The ``Enveloped`` class
~~~~~~~~~~~~~~~~~~~~~~~

All objects with an associated envelope are instances of the
`Enveloped` type class.  This includes diagrams, segments, trails, and
paths.  `Enveloped` provides a single method,

.. class:: lhs

::

> getEnvelope :: Enveloped b => b -> Envelope (V b)

which returns the envelope of an object.

In addition, the list type `[b]` is an instance of `Enveloped`
whenever `b` is.  The envelope for a list is simply the
combination of all the individual envelopes of the list's
elements---that is, an envelope that contains all of the list
elements.  In conjunction with the `Transformable` instance for lists
(see `The Transformable class`_), this can be used to do things such
as apply an alignment to a list of diagrams *considered as a group*.
For some examples and an explanation of why this might be useful, see
`Delayed composition`_.

Traces
------

Envelopes are useful for placing diagrams relative to one another, but
they are not particularly useful for finding actual points on the
boundary of a diagram.  Finding points on the boundary of a diagram
can be useful for things like drawing lines or arrows between two
shapes, or deciding how to position another diagram relative to a
given one.

Every diagram (and, more generally, anything which is an instance of
the `Traced` type class) has a *trace*, a function which is like an
"embedded ray tracer" for finding points on the diagram boundary.  In
particular, the trace function takes a *ray* as input (represented by
a base point and a vector indicating the direction) and returns the
positive distance (given as a multiple of the input vector magnitude)
to the nearest point where the ray intersects the diagram.

Normally, a trace is accessed using one of the four functions
`traceV`, `traceP`, `maxTraceV`, and `maxTraceP`.

* `traceV` takes as inputs a base point ``p``, a vector ``v``, and any
  instance of `Traced`.  It looks for intersection points with the
  given object along the entire line determined by ``p`` and ``v``
  (that is, the line with parametric form ``p .+^ (t *^ v)``), and
  returns a scalar multiple of ``v`` which extends from ``p`` to the
  point of intersection with the smallest parameter, or ``Nothing`` if
  there is no such intersection.

  Intuitively, the result of `traceV` extends from the given point to
  the "closest" intersection, but it must be kept in mind that if any
  intersection points lie in the direction opposite ``v``, it is
  really the *furthest* such point which will be used (that is, the
  point with the smallest, *i.e.* most negative, parameter).

  This behavior is illustrated below.  Note that it can be somewhat
  unintuitive when the base point lies inside the diagram.

  .. class:: dia-lhs

  ::

  > import Data.Maybe (fromMaybe)
  >
  > drawV v = (arrowHead <> shaft) # fc black
  >   where
  >     shaft     = origin ~~ (origin .+^ v)
  >     arrowHead = eqTriangle 0.1
  >               # rotateBy (direction v - 1/4)
  >               # translate v
  > drawTraceV v d
  >   = lc green $
  >     fromMaybe mempty
  >       ((origin ~~) <$> traceP origin v d)
  > illustrateTraceV v d = (d <> drawV v <> drawTraceV v d) # showOrigin
  >
  > example = hcat' with {sep = 1}
  >         . lw 0.03
  >         . map (illustrateTraceV (0.5 *^ (1 & 1)))
  >         $ [ circle 1 # translate ((-1.5) & (-1.5))
  >           , circle 1
  >           , circle 1 # translate (1.5 & 1.5)
  >           ]

* `traceP` works similarly, except that it returns the point of
  intersection itself, which lies on the boundary of the object, or
  ``Nothing`` if there is no such point.

  `traceV` and `traceP` are related: ``traceP p v x == Just p'`` if and
  only if ``traceV p v x == Just (p' .-. p)``.

* `maxTraceV` and `maxTraceP` are similar to `traceV` and `traceP`,
  respectively, except they look for the point of intersection with
  the *greatest* parameter.

For slightly more low-level access, the `Traced` class provides the
`getTrace` method, which can be used to directly access the trace
function for an object.  Currently, given inputs ``p`` and ``v``, it
returns a scalar ``s`` such that ``s *^ v`` extends from ``p`` to the
intersection point with the smallest parameter.  In the future, it may
be extended to return a list of all intersection points instead of
just the smallest; please contact the developers if you would be
interested in this feature.

The below diagram illustrates the use of the `traceP` function to
identify points on the boundaries of several diagrams.

.. class:: dia-lhs

::

> import Data.Maybe (mapMaybe)
>
> illustrateTrace d = d <> traceLines
>   where
>     traceLines  = mconcat
>                 . mapMaybe traceLine
>                 . iterateN 30 (rotateBy (1/60))
>                 $ unitX
>     traceLine v = (basePt ~~) <$> traceP basePt v d
>     basePt = 0 & (-2)
>
> example
>   = hcat' with {sep = 1}
>   . map illustrateTrace
>   $ [ square 1
>     , circle 1
>     , eqTriangle 1 # rotateBy (-1/4) ||| eqTriangle 1 # rotateBy (1/4)
>     ]

Of course, diagrams are not the only instance of `Traced`.  Paths are
also `Traced`, as are trails, segments, and points.  Lists and tuples
are `Traced` as long as all of their components are---the trace for a
list or tuple is the combination of all the element traces.

Named subdiagrams
-----------------

Although the simple combinatorial approach to composing diagrams can
get you a long way, for many tasks it becomes necessary (or, at least,
much simpler) to have a way to refer to previously placed subdiagrams.
That is, we want a way to give a name to a particular diagram, combine
it with some others, and then later be able to refer back to the the
subdiagram by name. Any diagram can be given a name with the `named`
function.

.. container:: warning

   The name mechanism described in this section should be considered
   experimental; it is quite likely to change (in both small and large
   ways) in future versions of diagrams.  Your feedback on the current
   design is greatly appreciated!

User-defined names
~~~~~~~~~~~~~~~~~~

Anything can be used as a name, as long as its type is an instance of
the `IsName` type class; to be an instance of the `IsName` class, it
suffices for a type to be an instance of `Typeable`, `Eq`, `Ord`, and
`Show`.  Making a user-defined type an instance of `IsName` is as
simple as:

.. class:: lhs

::

> {-# LANGUAGE DeriveDataTypeable #-}
>
> data Foo = Baz | Bar | Wibble
>   deriving (Typeable, Eq, Ord, Show)
>
> instance IsName Foo

That's it!  No method definitions are even needed for the `IsName`
instance, since `toName` (the sole method of `IsName`) has a default
implementation which works just fine.

Listing names
~~~~~~~~~~~~~

Sometimes you may not be sure what names exist within a diagram---for
example, if you have obtained the diagram from some external module,
or are debugging your own code.  The `names` function extracts a list
of all the names recorded within a diagram and the locations of any
associated subdiagrams.

When using `names` you will often need to add a type annotation such
as `D R2` to its argument, as shown below---for an explanation and
more information, see `No instances for Backend b0 R2 ...`_.

::

    ghci> names (circle 1 # named "joe" ||| circle 2 # named "bob" :: D R2)
    [("bob",[P (2.9999999999999996 & 0.0)]),("joe",[P (0.0 & 0.0)])]

Of course, there is in fact an entire subdiagram (or subdiagrams)
associated with each name, not just a point; but subdiagrams do not
have a `Show` instance.

Accessing names
~~~~~~~~~~~~~~~

.. container:: todo

  Create better tools for extracting subdiagrams directly and write
  about them here.  Should also write about the low-level interface
  here, I suppose.

Once we have given names to one or more diagrams, what can we do with
them?  The primary tool for working with names is `withName`, which
has the (admittedly scary-looking!) type

.. class:: lhs

::

  withName :: ( IsName n, AdditiveGroup (Scalar v), Floating (Scalar v)
              , InnerSpace v, HasLinearMap v)
           => n -> (Subdiagram v -> QDiagram b v m -> QDiagram b v m)
                -> (QDiagram b v m -> QDiagram b v m)

Let's pick this apart a bit.  First, we see that the type `n` must be
a name type. So far so good.  Then there are a bunch of constraints
involving `v`, but we can ignore those; they just ensure that `v` is a
vector space with the right properties.  So the first argument of
`withName` is a name---that makes sense.  The second argument is a
function of type

.. class:: lhs

::

  Subdiagram v -> QDiagram b v m -> QDiagram b v m

We can see this function as a transformation on diagrams, except that
it also gets to use some extra information---namely, a
"`Subdiagram v`", which records the local origin and envelope
associated with the name we pass as the first argument to `withName`.

Finally, the return type of `withName` is itself a transformation of
diagrams.

So here's how `withName` works.  Suppose we call it with the arguments
`withName n f d`.  If some subdiagram of `d` has the name `n`, then
`f` is called with the located envelope associated with `n`
as its first argument, and `d` itself as its second argument.  So we
get to transform `d` based on information about where the subdiagram
named `n` is located within it.  And what if there is no subdiagram
named `n` in `d`? In that case `f` is ignored, and `d` is returned
unmodified.

Here's a simple example making use of names to draw a line connecting
the centers of two subdiagrams.

.. class:: dia-lhs

::

> data Foo = Baz | Bar | Wibble
>   deriving (Typeable, Eq, Ord, Show)
>
> instance IsName Foo
>
> connect n1 n2
>   = withName n1 $ \b1 ->
>     withName n2 $ \b2 ->
>       atop ((location b1 ~~ location b2) # lc red # lw 0.03)
>
> example = (square 3 # named Baz ||| circle 2.3 # named Bar)
>         # connect Baz Bar

The `connect` function takes two names and returns a *function* from
diagrams to diagrams, which adds a red line connecting the locations
denoted by the two names.  Note how the two calls to `withName` are
chained, and how we have written the second arguments to `withName`
using lambda expressions (this is a common style).  Finally, we draw a
line between the two points (using the `location` function to access
the base points of the located envelopes), give it a style, and
specify that it should be layered on top of the diagram given as the
third argument to `connect`.

We then draw a square and a circle, give them names, and use `connect`
to draw a line between their centers.  Of course, in this example, it
would not be too hard to manually compute the endpoints of the line
(this is left as an exercise for the reader); but in more complex
examples such manual calculation can be quite out of the question.

`withName` also has two other useful variants:

* `withNameAll` takes a single name and makes available a list of
  *all* located envelopes associated with that name.
  (`withName`, by contrast, returns only the most recent.)  This is
  useful when you want to work with a collection of named subdiagrams all
  at once.

* `withNames` takes a list of names, and makes available a list of the
  most recent located envelopes associated with each.  Instead of the
  two calls to `withName` in the example above, we could have written

  .. class:: lhs

  ::

  > connect n1 n2
  >   = withNames [n1,n2] $ \[b1,b2] ->
  >       ...

There is also a function `place`, which is simply a flipped version of
`moveTo`, provided for convenience since it can be useful in
conjunction with `withName`.  For example, to draw a square at the
location of a given name, one can write something like

.. class:: lhs

::

> withName n $ atop . place (square 1) . location

This computes the location of the name `n`, positions a square at that
location, and then superimposes the positioned square atop the diagram
containing `n`.

Subdiagrams
~~~~~~~~~~~

So far, the examples we have seen have only made use of the local
origin associated with each subdiagram, accessed using the `location`
function.  However, subdiagrams are full-fledged diagrams, so there is
much more information to be taken advantage of.  For example, the
below code draws a tree of circles, using subdiagram traces (see
`Traces`_) to connect the *bottom* edge of the parent circle to the
*top* edge of each child circle, instead of connecting their centers.

.. class:: dia-lhs

::

> import Data.Maybe (fromMaybe)
>
> root   = circle 1 # named "root"
> leaves = centerXY
>        . hcat' with {sep = 0.5}
>        $ map (\c -> circle 1 # named c) "abcde"
>
> parentToChild child
>   = withName "root" $ \rb ->
>     withName child  $ \cb ->
>       atop (   fromMaybe origin (traceP (location rb) unitY rb)
>             ~~ fromMaybe origin (traceP (location cb) unit_Y cb))
>
> nodes  = root === strutY 2 === leaves
>
> example = nodes # applyAll (map parentToChild "abcde")

.. container:: todo

  Explain this example. Also, make some nicer tools so the code isn't
  so terrible.  Reinstate 'boundaryFrom' function?

Qualifying names
~~~~~~~~~~~~~~~~

To avoid name clashes, sometimes it is useful to be able to *qualify*
existing names with one or more prefixes.  Names actually consist of a
*sequence* of atomic names, much like Haskell module names consist of
a sequence of identifiers like `Diagrams.TwoD.Shapes`:mod:.

To qualify an existing name, use the `(|>)` operator, which can be
applied not only to individual names but also to an entire diagram
(resulting in all names in the diagram being qualified).  To construct
a qualified name explicitly, separate the components with `(.>)`.

.. class:: dia-lhs

::

> data Corner = NW | NE | SW | SE
>   deriving (Typeable, Eq, Ord, Show)
> instance IsName Corner
>
> connect n1 n2
>   = withName n1 $ \b1 ->
>     withName n2 $ \b2 ->
>       atop ((location b1 ~~ location b2) # lc red # lw 0.03)
>
> squares =  (s # named NW ||| s # named NE)
>        === (s # named SW ||| s # named SE)
>   where s = square 1
>
> d = hcat' with {sep = 0.5} (zipWith (|>) [0::Int ..] (replicate 5 squares))
>
> pairs :: [(Name, Name)]
> pairs = [ ((0::Int) .> NE, (2::Int) .> SW)
>         , ((1::Int) .> SE, (4::Int) .> NE)
>         , ((3::Int) .> NW, (3::Int) .> SE)
>         , ((0::Int) .> SE, (1::Int) .> NW)
>         ]
>
> example = d # applyAll (map (uncurry connect) pairs)

We create a four-paned square with a name for each of its panes; we
then make five copies of it.  At this point, each of the copies has
the same names, so there would be no way to refer to any of them
individually.  The solution is to qualify each of the copies
differently; here we have used a numeric prefix.

(As an aside, note how we had to use a type annotation on the integers
that we used as names; numeric literals are polymorphic and `(|>)`
needs to know what type of atomic name we are using. Without the type
annotations, we would get an `error about an "ambiguous type variable"`_.
It's a bit annoying to insert all these annotations, of course;
another option would be to use monomorphic constants like `String`\s
or `Char`\s instead, or to create our own data type with a short
constructor name that wraps an `Int`.)

.. _`error about an "ambiguous type variable"`: `More ambiguity`_

Note how we also made use of `applyAll`, which takes a list of
functions as an argument and composes them into one; that is,
`applyAll [f, g, h] === f . g . h`.

Using queries
-------------

Every diagram has an associated *query*, which assigns a value to
every point in the diagram.  These values must be taken from some
monoid (see `Monoids`_).  Combining two diagrams results in their
queries being combined pointwise.

The default query
~~~~~~~~~~~~~~~~~

The default query assigns a value of type `Any` to each point in a
diagram.  In fact, `Diagram b v` is really a synonym for
`QDiagram b v Any`.  `Any` represents the monoid on the booleans
with logical or as the binary operation (and hence `False` as the
identity).  The default query simply indicates which points are
"inside" the diagram and which are "outside".

.. container:: warning

   The default `Any` query and the envelope are quite
   different, and may give unrelated results.  The envelope
   is an approximation used to be able to place diagrams next to one
   another; the `Any` query is a more accurate record of which points
   are enclosed by the diagram.  (Using the query in order to position
   diagrams next to each other more accurately/snugly would be,
   generally speaking, computationally infeasible.)

The following example queries an ellipse (using the `sample` function
to sample it at a set of particular points), coloring points inside
the ellipse red and points outside it blue.

.. class:: dia-lhs

::

> c :: Diagram Cairo R2
> c = circle 5 # scaleX 2 # rotateBy (1/14) # lw 0.03
>
> -- Generated by fair dice roll, guaranteed to be random
> points = map p2 $
>          [ (0.8936218079179525,6.501173563301563)
>          , (0.33932828810065985,9.06458375044167)
>          , (2.12546952534467,4.603130561299622)
>          , (-8.036711369641125,6.741718165576458)
>          , (-9.636495308950543,-8.960315063595772)
>          , (-5.125008672475815,-4.196763141080737)
>          , (-8.740284494124353,-1.748269759118557)
>          , (-2.7303729625418782,-9.902752498164773)
>          , (-1.6317121405154467,-6.026127282530069)
>          , (-3.363167801871896,7.5571909081190825)
>          , (5.109759075567126,-5.433154460042715)
>          , (8.492015791125596,-9.813023637980223)
>          , (7.762080919928849,8.340037921443582)
>          , (-6.8589746952056885,3.9604472182691097)
>          , (-0.6083773449063301,-3.7738202372565866)
>          , (1.3444943726062775,1.1363744735717773)
>          , (0.13720748480409384,8.718934659846127)
>          , (-5.091010760515928,-8.887260649353266)
>          , (-5.828490639105439,-9.392594425007701)
>          , (0.7190148020163178,1.4832069771364331)
>          ]
>
> mkPoint p = (p, circle 0.3
>           	  # lw 0
>           	  # fc (case sample c p of
>           	          Any True  -> red
>           	          Any False -> blue
>           	       )
>             )
>
> example = c <> position (map mkPoint points)

Using other monoids
~~~~~~~~~~~~~~~~~~~

You can use monoids besides `Any` to record other information about a
diagram.  For example, the diagram below uses the `Sum` monoid to draw
dots whose size is determined by the number of overlapping shapes at a
given point.  Note the use of the `value` function to switch from the
default `Any` to a different monoid: `value v` replaces `Any True` with `v`
and `Any False` with `mempty`.

.. class:: dia-lhs

::

> withCount = (# value (Sum 1))
>
> c :: QDiagram Cairo R2 (Sum Int)
> c = (   circle 5 # scaleX 2 # rotateBy (1/14) # withCount
>      <> circle 2 # scaleX 5 # rotateBy (-4/14) # withCount
>     )
>     # lw 0.03
>
> -- Generated by fair dice roll, guaranteed to be random
> points = map p2 $
>          [ (0.8936218079179525,6.501173563301563)
>          , (0.33932828810065985,9.06458375044167)
>          , (2.12546952534467,4.603130561299622)
>          , (-8.036711369641125,6.741718165576458)
>          , (-9.636495308950543,-8.960315063595772)
>          , (-5.125008672475815,-4.196763141080737)
>          , (-8.740284494124353,-1.748269759118557)
>          , (-2.7303729625418782,-9.902752498164773)
>          , (-1.6317121405154467,-6.026127282530069)
>          , (-3.363167801871896,7.5571909081190825)
>          , (5.109759075567126,-5.433154460042715)
>          , (8.492015791125596,-9.813023637980223)
>          , (7.762080919928849,8.340037921443582)
>          , (-6.8589746952056885,3.9604472182691097)
>          , (-0.6083773449063301,-3.7738202372565866)
>          , (1.3444943726062775,1.1363744735717773)
>          , (0.13720748480409384,8.718934659846127)
>          , (-5.091010760515928,-8.887260649353266)
>          , (-5.828490639105439,-9.392594425007701)
>          , (0.7190148020163178,1.4832069771364331)
>          ]
>
> mkPoint p = (p, circle (case sample c p of
>                           Sum n  -> 2 * fromIntegral n / 5 + 1/5)
>                 # fc black
>             )
>
> example = c # clearValue <> position (map mkPoint points)

Notice also the use of `clearValue` to get rid of the custom query;
the program that builds this documentation requires `example` to have
the type `QDiagram Cairo R2 Any`.

As another interesting example, consider using a set monoid to keep
track of names or identifiers for the diagrams at a given point.  This
could be used, say, to identify which element(s) of a diagram have been
selected by the user after receiving the coordinates of a mouse click.

Bounding boxes
--------------

.. container:: todo

  Expand on this section, based on Michael Sloan's recent refactoring.

Envelopes (see `Working with envelopes`_) are more flexible and
compositional than bounding boxes for the purposes of combining
diagrams.  However, occasionally it is useful for certain applications
to be able to work with bounding boxes, which support fast tests for
inclusion as well as union and intersection operations (envelopes
support union but not inclusion testing or intersection).

To this end, a generic implementation of arbitrary-dimensional
bounding boxes is provided in `Diagrams.BoundingBox`:mod:.  Bounding
boxes can be created from sets of points or from any `Enveloped`
object, used for inclusion or exclusion testing, and combined via
union or intersection.

To obtain a rectangle corresponding to a diagram's bounding box, use
`boundingRect`.

Scale-invariance
----------------

The `ScaleInv` wrapper can be used to create "scale-invariant"
objects. (Note that `ScaleInv` is not exported from
`Diagrams.Prelude`:mod:; to use it, import
`Diagrams.TwoD.Transform`:mod:.)  In the diagram below, the same
transformation is applied to each pair of arrows; the arrowheads on
the right are wrapped in `ScaleInv` but the ones on the left are not.

.. class:: dia-lhs

::

> {-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
>
> import Diagrams.TwoD.Transform
>
> class Drawable d where
>   draw :: d -> Diagram Cairo R2
>
> instance Drawable (Diagram Cairo R2) where
>   draw = id
>
> instance Drawable a => Drawable (ScaleInv a) where
>   draw = draw . unScaleInv
>
> instance (Drawable a, Drawable b) => Drawable (a,b) where
>   draw (x,y) = draw x <> draw y
>
> arrowhead, shaft :: Diagram Cairo R2
> arrowhead = eqTriangle 0.5 # fc black # rotateBy (-1/4)
> shaft = origin ~~ (3 & 0)
>
> arrow1 = (shaft,          arrowhead       # translateX 3)
> arrow2 = (shaft, scaleInv arrowhead unitX # translateX 3)
>
> showT tr = draw (arrow1 # transform tr) 
>        ||| strutX 1 
>        ||| draw (arrow2 # transform tr)
>
> example = vcat' with {sep = 0.5} 
>             (map (centerX . showT)
>               [ scalingX (1/2)
>               , scalingY 2
>               , scalingX (1/2) <> rotation (-1/12 :: CircleFrac)
>               ])

Type reference
==============

This section serves as a reference in understanding the types used in
the diagrams framework.

Understanding diagrams types
----------------------------

Let's look again at the type of `hcat`, mentioned in `Types and type
classes`_:

.. class:: lhs

::

  hcat :: (Juxtaposable a, HasOrigin a, Monoid' a, V a ~ R2) => [a] -> a

This is fairly typical of the types you will encounter when using
diagrams.  They can be intimidating at first, but with a little
practice they are not hard to read.  Let's look at the components of
this particular type from right to left:

* `[a] -> a`.  This part is simple enough: it denotes a function from
  a list of `a`\'s to a single `a`.  Typically, the type to the right
  of `=>` will be some simple polymorphic type.

* `V a ~ R2`.  This is a `type equality constraint`_, which says that
  the types `V a` and `R2` must be equal.  In this case `R2` is the
  `type of two-dimensional vectors`_, and `V` is a `type family`_
  which tells us the vector space that corresponds to a particular
  type.  So `V a ~ R2` means "the vector space corresponding to `a`
  must be `R2`", or more informally, "`a` must be a type representing
  two-dimensional things".

* `Juxtaposable a, ...` These are type class constraints on `a`,
  specifying what primitive operations `a` must support in order to be
  meaningfully used with `hcat`.  For a complete reference on all the
  type classes used by diagrams, see the next section, `Type class
  reference`_.

.. _`type equality constraint`: http://www.haskell.org/ghc/docs/latest/html/users_guide/equality-constraints.html
.. _`type of two-dimensional vectors`: `Basic 2D types`_
.. _`type family`: http://www.haskell.org/haskellwiki/GHC/Type_families

Type class reference
--------------------

This section serves as a reference for all the type classes defined or
used by diagrams; there are quite a lot. (Some might even say too
many!)  Most, if not all, of these are also covered elsewhere, but it
is useful to have them collected all in one place.  The declaration of
each type class is shown along with a short explanation, a list of
instances, and links to further reading.

Classes for transforming and combining
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

HasOrigin
+++++++++

`HasOrigin` is defined in `Diagrams.Core.HasOrigin`:mod:.

.. class:: lhs

::

> class VectorSpace (V t) => HasOrigin t where
>   moveOriginTo :: Point (V t) -> t -> t

`HasOrigin` classifies types with a notion of a fixed "location"
relative to some "local origin", and provides a means of moving the
local origin.  This is provided as a separate class from
`Transformable` since some things with a local origin do not support
other sorts of transformations; and contrariwise some things that
support transformations are translation-invariant (like trails and
vectors) and hence do not have a `HasOrigin` instance.

The `moveOriginTo` method moves the *local origin* to the given
point.

Instances:

  * The instances for `Point`, `SubMap`, `Subdiagram`, and `QDiagram`
    all have the meaning you would expect.
  * The instances for `Trace`, `Envelope`, and `Query` all obey the
    invariant that, *e.g.*, ``getEnvelope . moveOriginTo p t ==
    moveOriginTo p t . getEnvelope``. That is, if ``e`` is the
    envelope/trace/query for diagram ``d``, moving the origin of ``e``
    to ``p`` yields the envelope/trace/query for ``d`` with its origin
    moved to ``p``.
  * Container types can be translated by translating each
    element (``(a,b)``, ``[a]``, `Set`, `Map`).
  * Things wrapped in `TransInv` are not supposed to be affected by
    translation, so the `TransInv` instance has `moveOriginTo = const
    id`.
  * The instance for `Transformation` constructs a translation and
    composes it appropriately.

Further reading: `Alignment`_.

Transformable
+++++++++++++

`Transformable` is defined in `Diagrams.Core.Transform`:mod:.

.. class:: lhs

::

> class HasLinearMap (V t) => Transformable t where
>   transform :: Transformation (V t) -> t -> t

It represents types which support arbitrary affine transformations (or
linear transformations, in the case of translationally invariant
things).

Instances:
  * `Prim`, `SubMap`, `Subdiagram`, `QDiagram`: these have the meaning
    you would expect.
  * Of course, `Transformation` is itself transformable, by composition.
  * Container types can be transformed by transforming each
    element (``(t,t)``, ``(t,t,t)``, ``[t]``, `Set`, `Map`).
  * ``Point v`` is transformable whenever ``v`` is; translations
    actually affect points (whereas they might not have an effect on
    the underlying type ``v``).
  * Anything wrapped in `TransInv` will not be affected by
    translation.
  * Anything wrapped in `ScaleInv` will not be affected by scaling.
    See `Scale-invariance`_ for more information.
  * Applying a transformation to a `Style`
    simply applies it to every attribute.
  * The meaning of transforming an `Attribute` depends on the
    particular attribute.
  * The instances for `Trace`, `Envelope`, and `Query` all obey the
    invariant that, *e.g.*, ``getEnvelope . transform t == transform t
    . getEnvelope``. That is, if ``e`` is the envelope/trace/query for
    diagram ``d``, transforming ``e`` with ``t`` yields the
    envelope/trace/query for ``d`` transformed by ``t``.
  * The instance for `Deletable` simply lifts transformations on the
    underlying type.
  * The instance for `NullPrim` does nothing, since there is nothing
    to transform.
  * Uniform scales can be applied to `Double` and `Rational` values;
    translations can also be applied but have no effect.

Further reading: `Euclidean 2-space`_; `2D Transformations`_.

Juxtaposable
++++++++++++

`Juxtaposable` is defined in `Diagrams.Core.Juxtapose`:mod:.

.. class:: lhs

::

> class Juxtaposable a where
>   juxtapose :: V a -> a -> a -> a

`Juxtaposable` represents types of things which can be positioned
"next to" one another.  Note that this is more general than "having an
envelope" (though certainly any instance of `Enveloped` can be made an
instance of `Juxtaposable`, using `juxtaposeDefault`).  For example,
animations are an instance of `Juxtaposable` (which corresponds to
juxtaposing them at every point in time), but not of `Enveloped`.

`juxtapose v a1 a2` positions `a2` next to `a1` in the
direction of `v`.  In particular, it places `a2` so that `v` points
from the local origin of `a1` towards the old local origin of
`a2`; `a1`\'s local origin becomes `a2`\'s new local origin.  The
result is just a translated version of `a2`.  (In particular,
`juxtapose` does not *combine* `a1` and `a2` in any way.)

Instances:
  * `QDiagram` and `Envelope` are of course instances.
  * Many container types are also instances, since container types
    have `Enveloped` instances that work by superimposing all the
    envelopes of the individual elements: `[a]`, `(a,b)`, `Set`, `Map`

Further reading: `Juxtaposing diagrams`_; `Juxtaposing without composing`_.

Enveloped
+++++++++

`Enveloped` is defined in `Diagrams.Core.Envelope`:mod:.  It
classifies types which have an associated `Envelope`.

.. class:: lhs

::

> class (InnerSpace (V a), OrderedField (Scalar (V a))) => Enveloped a where
>   getEnvelope :: a -> Envelope (V a)

The `getEnvelope` method simply computes or projects out its
argument's associated `Envelope`.  `InnerSpace`, defined in
`Data.VectorSpace`:mod:, classifies vector spaces with an inner (dot)
product.  Computing envelopes almost always involves projection of one
vector onto another, which requires an inner product.  The
`OrderedField` class is simply a synonym for a collection of classes,
requiring that the scalar type have multiplicative inverses and be
linearly ordered.  See `OrderedField`_.

Instances:
  * The instance for `QDiagram` does what you would expect.
  * The instance for `Subdiagram` yields an envelope positioned
    relative to the parent diagram.
  * Every `Point` has a "point envelope" consisting of the constantly
    zero envelope translated to the given point.  Note this is not the
    same as the empty envelope.
  * Many container types have instances which work by combining all
    the envelopes of the individual elements: `[a]`, `(a,b)`, `Set`,
    `Map`.

Further reading: `Envelopes and local vector spaces`_; `Working with
envelopes`_; `Diagrams.Core.Envelope`_.

Traced
++++++

`Traced` is defined in `Diagrams.Core.Trace`:mod:, and plays a similar
role as `Enveloped`.  `Traced` types have an associated `Trace`, which
is like an embedded ray tracer that can be used to find points on the
boundary of an object.

.. class:: lhs

::

> class (Ord (Scalar (V a)), VectorSpace (V a)) => Traced a where
>   getTrace :: a -> Trace (V a)

Instances:
  * The instance for `QDiagram` does what you would expect.
  * The instance for `Subdiagram` yields a trace positioned
    relative to the parent diagram.
  * The trace of a `Point` is the empty trace.
  * Many container types have instances which work by combining all
    the envelopes of the individual elements: `[a]`, `(a,b)`, `Set`,
    `Map`.

Further reading: `Traces`_.

Classes for attributes and styles
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

AttributeClass
++++++++++++++

`AttributeClass`, defined in `Diagrams.Core.Style`:mod:, is simply a
proxy for `Typeable` and `Semigroup`; it has no methods.  Any type
used as an attribute must be made a member of this class.

.. class:: lhs

::

> class (Typeable a, Semigroup a) => AttributeClass a

Instances: many; see `Diagrams.Attributes`:mod: and
`Diagrams.TwoD.Path`:mod:.

Further reading: `Attributes and styles`_; `Text`_; `Diagrams.Core.Style`_.

HasStyle
++++++++

`HasStyle`, also defined in `Diagrams.Core.Style`:mod:, classifies
things to which a `Style` can be applied.

.. class:: lhs

::

> class HasStyle a where
>   applyStyle :: Style (V a) -> a -> a

`applyStyle` applies the given `Style` to an object, combining it on
the left with the existing `Style` (according to the `Monoid` instance
of `Style`).

Instances:
  * `Style` itself is an instance.
  * Many container types are instances as long as their elements are;
    applying a style to a container simply applies the style uniformly
    to every element: `(a,b)`, `Map k a`, `Set`, `[a]`.
  * Functions `(b -> a)` are an instance as long as `a` is.  (This can
    also be thought of as a "container type".)
  * Of course, `QDiagram b v m` is an instance, given a few
    restrictions on `v` and `m`.

Further reading: `Attributes and styles`_; `Text`_;
`Diagrams.Core.Style`_.

Classes for names
~~~~~~~~~~~~~~~~~

IsName
++++++

`IsName` is defined in `Diagrams.Core.Names`:mod:. It simply provides
the `toName` method for converting to `Name`, with a default
implementation that wraps up a value as an atomic name.  It allows
values of arbitrary types to be used as names for subdiagrams.

.. class:: lhs

::

> class (Typeable a, Ord a, Show a) => IsName a where
>   toName :: a -> Name
>   toName = Name . (:[]) . AName

Instances:
  * Many primitive types such as `()`, `Bool`, `Char`, `Int`, `Float`,
    `Double`, `Integer`, `String`, `[a]`, `(a,b)`, `(a,b,c)` have a
    default `IsName` instance.
  * `AName` is an instance; converting an atomic name to `Name` works
    by creating a singleton list.
  * `Name` is an instance, with `toName` as the identity function.

Further reading: `Stroking trails and paths`_; `Named subdiagrams`_;
`User-defined names`_.

Qualifiable
+++++++++++

`Qualifiable` is also defined in `Diagrams.Core.Names`:mod:. Instances
of `Qualifiable` are things which can be "qualified" by prefixing them
with a name.

.. class:: lhs

::

> class Qualifiable q where
>   -- | Qualify with the given name.
>   (|>) :: IsName a => a -> q -> q

Instances:
  * `Name`: qualifying one name with another is just concatenation.
  * `SubMap` and `QDiagram`: qualifying prefixes a name on all the
    existing names.

Further reading: `Named subdiagrams`_; `Subdiagrams`_; `Qualifying
names`_.

Classes for paths
~~~~~~~~~~~~~~~~~

PathLike
++++++++

The `PathLike` class, defined in `Diagrams.Path`:mod:, abstracts over
things that are "path-like", so that functions such as `square` can be
used to construct a diagram, a path, a trail, *etc.*.

.. class:: lhs

::

> class (Monoid' p, VectorSpace (V p)) => PathLike p where
>
>   pathLike :: Point (V p)      -- ^ The starting point of the
>                                --   path.  Some path-like things
>                                --   (e.g. 'Trail's) may ignore this.
>            -> Bool             -- ^ Should the path be closed?
>            -> [Segment (V p)]  -- ^ Segments of the path.
>            -> p

The `pathLike` method provides a generic way to build a "path-like"
thing by specifying the low-level path data.  Note that there should
usually not be any need for end users to call `pathLike` directly
(though there certainly may be some use cases).

Instances:
  * `Trail`: this is the most "direct" instance (in some sense the
    class probably should have been named "TrailLike" instead of
    "PathLike").  However, the starting point is ignored.
  * `Path`: this instance creates a `Path` with a single `Trail`,
    beginning at the given starting point.
  * `QDiagram b R2 Any`: the diagram obtained by stroking the given
    path.
  * `[Point v]`: a list of the path's vertices.
  * `Active p` (for any `PathLike p`): creates a constant `Active`
    value.

Further reading: `Working with paths`_; `Trails`_; `Paths`_; `The
PathLike class`_.

Closable
++++++++

The `Closeable` class, also defined in `Diagrams.Path`:mod:,
represents path-like things which can be "open" or "closed".

.. class:: lhs

::

> class PathLike p => Closeable p where
>   -- | "Open" a path-like thing.
>   open  :: p -> p
>
>   -- | "Close" a path-like thing, by implicitly connecting the
>   --   endpoint(s) back to the starting point(s).
>   close :: p -> p

Instances: `Trail` and `Path`.

Further reading: `Working with paths`_; `The Closeable class`_.

Classes for backends
~~~~~~~~~~~~~~~~~~~~

Backend
+++++++

The `Backend` class, defined in `Diagrams.Core.Types`:mod:, defines
the primary interface for any diagrams rendering backend.  Unlike many
of the other type classes in diagrams, it is quite large.  For a full
discussion, see `Backends`_.

MultiBackend
++++++++++++

`MultiBackend`, also defined in `Diagrams.Core.Types`:mod:, is for
backends which support rendering multiple diagrams, for example to a
multi-page pdf or something similar.  It simply provides the
`renderDias` function for rendering multiple diagrams at once; the
meaning of this function depends on the backend.

.. class:: lhs

::

> class Backend b v => MultiBackend b v where
>   renderDias :: (InnerSpace v, OrderedField (Scalar v), Monoid' m)
>              => b -> Options b v -> [QDiagram b v m] -> Result b v

So far, the only backend which supports multi-diagram rendering is
the `postscript backend`_.

Further reading: `Rendering backends`_; `Backends`_.

Renderable
++++++++++

The `Renderable` type class (from `Diagrams.Core.Types`:mod:) is a
two-parameter type class connecting backends to primitives which they
know how to render.  Backend `B` declares that it knows how to draw
primitive `P` by giving a `Renderable P B` instance, which requires
implementing the `render` function which takes a primitive and renders
it.

.. class:: lhs

::

> class Transformable t => Renderable t b where
>   render :: b -> t -> Render b (V t)

Instances: There are many instances defined by each backend.

Further reading: `Rendering backends`_.

Poor man's type synonyms
~~~~~~~~~~~~~~~~~~~~~~~~

There are several cases where a certain set of type class constraints
are used together so often that it is convenient to define a synonym
to stand in for the entire set of constraints.  In more recent
versions of GHC that support the ``ConstraintKinds`` extension, this
could be accomplished with a simple type synonym.  However, since
diagrams still supports older versions of GHC, these are declared as a
new type class with no methods and a single universal instance.  For
example,

.. class:: lhs

::

> class (Class1 a, Class2 a, Class3 a) => Synonym a
> instance (Class1 a, Class2 a, Class3 a) => Synonym a

Ideally, at some point in the future diagrams will drop support for
versions of GHC without ``ConstraintKinds`` and switch to the more
sensible way of defining constraint synonyms.

Monoid'
+++++++

`Monoid' m` is a synonym for

  `(Semigroup m, Monoid m)`,

defined in `Diagrams.Core`:mod:. This is something of an unfortunate
hack: although every monoid is a semigroup mathematically speaking,
`Semigroup` is not actually a superclass of `Monoid`, so if we want to
use both we have to actually declare both.

HasLinearMap
++++++++++++

`HasLinearMap v` is a synonym for

  `(HasBasis v, HasTrie (Basis v), VectorSpace v)`,

all of which are necessary for constructing linear maps over the
vector space `v`.

OrderedField
++++++++++++

`OrderedField s`, defined in `Diagrams.Core.Envelope`:mod:, is a
synonym for

  `(Fractional s, Floating s, Ord s, AdditiveGroup s)`,

*i.e.* a floating-point type which is totally ordered.  When dealing
with `Envelopes` it's often necessary to have scalars which support
all four arithmetic operations as well as square root, and can be
compared for ordering.

Type family reference
---------------------

V
~

The `V` type family is defined in `Diagrams.Core.V`.  The idea is that
many types have an "associated" vector space, *i.e.* the vector space
in which they "live".  `V` simply maps from types to their associated
vector space.

Render
~~~~~~

`Render` is an associated data family of the `Backend` class.  It
determines the type of rendering operations for a given backend.  For
more information, see `Backends`_.

Result
~~~~~~

`Result` is an associated type family of the `Backend` class.  It
determines the type of the final result obtained from the backend
after rendering a complete diagram.  For more information, see
`Backends`_.

Options
~~~~~~~

`Options` is an associated data family of the `Backend` class.  It
determines the type of options which can be passed to the backend when
initiating a rendering operation.  For more information, see `Backends`_.

Tips and tricks
===============

Using absolute coordinates
--------------------------

Diagrams tries to make it easy to construct many types of graphics
while thinking in only "relative" terms: put this to the right of
that; lay these out in a row; draw this wherever that other thing
ended up; and so on.  Sometimes, however, this is not enough, and one
really wants to just think in absolute coordinates: draw this here,
draw that there.  If you find yourself wanting this, here are some
tips:

  * If you have a list of diagrams which are already correctly
    positioned, you can combine them with `mconcat`.
  * The `position` function takes a list of diagrams associated with
    positions and combines them while placing them at the indicated
    absolute positions.
  * `moveTo` can be used to position a single diagram absolutely.
  * `place` is a flipped version of `moveTo` which is sometimes
    convenient.

Delayed composition
-------------------

Suppose we have four diagrams that we want to lay out relative to one
another.  For example:

.. class:: dia-lhs

::

> t = eqTriangle 5   # fc orange
> s = square     3   # fc red
> o = ellipseXY  2 3 # fc blue
> c = circle     2   # fc green
>
> d = centerX (t ||| s ||| o ||| c)
>
> example = d

(Instead of `(|||)` we could equivalently have used `hcat`.)  Now
`d` is the diagram consisting of these four shapes laid out in a
centered row.

But what if we want to do further processing on the individual shapes?
At this point, we are out of luck.  There is no way to break apart a
diagram into subdiagrams once it has been composed together (for good
reasons).  We could use `juxtapose` (which positions one diagram
relative to another without actually doing any composition) but that
would get ugly and unintuitive.

Here is where the nifty trick comes in: simply enclose each shape in a
list, like so:

.. class:: lhs

::

> ds = centerX ([t] ||| [s] ||| [o] ||| [c])

Now `ds` is a *list* of four diagrams, which are the same as the
original `t`, `s`, `o`, `c` except that they have been positioned as
they would be in `d`!  We can now go on to do other things with them
individually.  For example, we could alter their positions slightly
before composing them (*e.g.* this makes for an easy way to apply some
random "jitter" to a layout):

.. class:: dia-lhs

::

> t = eqTriangle 5   # fc orange
> s = square     3   # fc red
> o = ellipseXY  2 3 # fc blue
> c = circle     2   # fc green
>
> ds = centerX ([t] ||| [s] ||| [o] ||| [c])
> d' = mconcat $ zipWith translateY [0.5, -0.6, 0, 0.4] ds
>
> example = d'

In other words, enclosing diagrams in a list allows them to be
positioned, aligned, *etc.* as they normally would, *except* that it
delays actually composing them!

This works because lists are instances of `Juxtaposable`, `Alignable`,
`Enveloped`, `HasOrigin`, `HasStyle`, `Transformable`, and, of course,
`Monoid`.  All these instances work in the "obvious" way---for
example, the envelope for a list is the combination of the envelopes
of the elements---so applying an operation to a list of diagrams has
the same effect as applying the operation to the composition of those
diagrams.  In other words, operations such as `centerX`, `scale`,
`juxtapose`, *etc.* all commute with `mconcat`.

Naming vertices
---------------

Most functions that create some sort of shape (*e.g.* `square`,
`pentagon`, `polygon`...) can in fact create any instance of the
`PathLike` class (see `The PathLike class`_).  You can often
take advantage of this to do some custom processing of shapes by
creating a *path* instead of a diagram, doing some processing, and
then turning the path into a diagram.

In particular, assigning names to the vertices of a shape can be
accomplished as follows. Instead of writing just (say) `pentagon`, write

.. class:: lhs

::

> stroke' with { vertexNames = [[0..]] } pentagon

which assigns consecutive numbers to the vertices of the pentagon.

Deciphering error messages
--------------------------

Although making ``diagrams`` an *embedded* domain specific language
has many benefits, it also has (at least) one major downside:
difficult-to-understand error messages.  Interpreting error messages
often requires understanding particular details about the internals of
the ``diagrams`` framework as well as the particular behavior of GHC.
This section attempts to make the situation a bit more palatable by
explaining a few common types of error message you might get while
using ``diagrams``, along with some suggestions as to their likely
causes and solutions.

This section is certainly incomplete; please send examples of other
error messages to the `diagrams mailing list`_ for help interpreting
them and/or so they can be added to this section.

Couldn't match type `V P2` with `R2`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This error is due to what appears to be a bug in recent versions of
GHC.  For some reason the definition of the `V` type family for points
is not exported.  To solve this you can add an explicit import of the
form `import Diagrams.Core.Points` to the top of your
file.

No instances for Backend b0 R2 ...
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There will probably come a time when you get an error message such as

::

    <interactive>:1:8:
        No instances for (Backend b0 R2,
                          Renderable Diagrams.TwoD.Ellipse.Ellipse b0)
          arising from a use of `circle'

The problem really has nothing to do with missing instances, but with
the fact that a concrete backend type has not been filled in for `b0`
(or whatever type variable shows up in the error message).  Such
errors arise when you pass a diagram to a function which is
polymorphic in its input but monomorphic in its output, such as
`width`, `height`, `phantom`, or `names`.  Such functions compute some
property of the diagram, or use it to accomplish some other purpose,
but do not result in the diagram being rendered.  If the diagram does
not have a monomorphic type, GHC complains that it cannot determine
the diagram's type.

For example, here is the error we get if we try to compute the
width of a radius-1 circle:

::

    ghci> width (circle 1)

    <interactive>:1:8:
        No instances for (Backend b0 R2,
                          Renderable Diagrams.TwoD.Ellipse.Ellipse b0)
          arising from a use of `circle'
        Possible fix:
          add instance declarations for
          (Backend b0 R2, Renderable Diagrams.TwoD.Ellipse.Ellipse b0)
        In the first argument of `width', namely `(circle 1)'
        In the expression: width (circle 1)
        In an equation for `it': it = width (circle 1)

GHC complains that it cannot find an instance for "`Backend b0
R2`"; what is really going on is that it does not have enough
information to decide which backend to use for the circle (hence
the type variable `b0`).  This is annoying because *we* know that
the choice of backend cannot possibly affect the width of the
circle; but there is no way for GHC to know that.

The special type `D` is provided for exactly this situation, defined as

.. class:: lhs

::

> type D v = Diagram NullBackend v

`NullBackend` is a "backend" which simply does nothing: perfect
for use in cases where GHC insists on knowing what backend to use but
the backend really does not matter.

For exapmle, the solution to the problem with `width` is to annotate
`circle 1` with the type `D R2`, like so:

::

    ghci> width (circle 1 :: D R2)
    2.0

More ambiguity
~~~~~~~~~~~~~~

You may also see error messages that directly complain about
ambiguity. For example, the code below is taken from the example in
the section on `Qualifying names`_:

.. class:: lhs

::

> hcat' with {sep = 0.5} (zipWith (|>) [0 .. ] (replicate 5 squares))

It is an attempt to qualify the names in five copies of `squares` with
the numbers `0`, `1`, `2`, ...  However, it generates the error shown below:

::

    Ambiguous type variable `a0' in the constraints:
      (IsName a0) arising from a use of `|>'
                  at /tmp/Diagram2499.lhs:13:39-42
      (Num a0) arising from the literal `0' at /tmp/Diagram2499.lhs:13:45
      (Enum a0) arising from the arithmetic sequence `0 .. '
                at /tmp/Diagram2499.lhs:13:44-49
    Probable fix: add a type signature that fixes these type variable(s)
    In the first argument of `zipWith', namely `(|>)'
    In the second argument of `hcat'', namely
      `(zipWith (|>) [0 .. ] (replicate 5 squares))'
    In the expression:
      hcat'
        (with {sep = 0.5}) (zipWith (|>) [0 .. ] (replicate 5 squares))

The problem, again, is that GHC does not know what type to choose for
some polymorphic value.  Here, the polymorphic values in question are
the numbers `0`, `1`, ... Numeric literals are polymorphic in Haskell,
so GHC does not know whether they should be `Int`\s or `Integer`\s or
`Double`\s or... The solution is to annotate the `0` with the desired
type.


Animation
=========

As of version 0.5, diagrams has experimental support for the creation of
*animations*.  It is by no means complete, so bug reports and feature
requests are especially welcome.

Animations are created with the help of a generic `Active`
abstraction, defined in the `active`:pkg: package.

Active
------

The `active`:pkg: package defines a simple abstraction for working
with *time-varying values*. A value of type `Active a` is either a
constant value of type `a`, or a time-varying value of type `a`
(*i.e.* a function from time to `a`) with specific start and end
times. Since active values have start and end times, they can be
aligned, sequenced, stretched, or reversed. In a sense, this is sort
of like a stripped-down version of functional reactive programming
(FRP), without the reactivity.

There are two basic ways to create an `Active` value. The first is to
use `mkActive` to create one directly, by specifying a start and end
time and a function of time. More indirectly, one can use the
`Applicative` instance for `Active` together with the "unit interval"
`ui`, which takes on values from the unit interval from time 0 to time
1, or `interval`, which is like `ui` but over an arbitrary interval.

For example, to create a value of type `Active Double` which represents
one period of a sine wave starting at time 0 and ending at time 1, we
could write

.. class:: lhs

::

> mkActive 0 1 (\t -> sin (fromTime t * tau))

or

.. class:: lhs

::

> (sin . (*tau)) <$> ui

`pure` can also be used to create `Active` values which are constant
and have no start or end time. For example,

.. class:: lhs

::

> mod <$> (floor <$> interval 0 100) <*> pure 7

cycles repeatedly through the numbers 0-6.

To take a "snapshot" of an active value at a particular point in time,
the `runActive` function can be used to turn one into a function of
time.  For example,

::

  > runActive ((sin . (*tau)) <$> ui) $ 0.2
  0.9510565162951535

.. container:: todo

  Write more about using the active library.  For now, you can read
  the `package documentation`_ for more information.

  * Transforming active values
  * Combining active values

.. _`package documentation`: http://hackage.haskell.org/packages/archive/active/latest/doc/html/Data-Active.html


Using Active with diagrams
--------------------------

An animation is defined, simply, as something of type
`Active (Diagram b v)` for an appropriate backend type `b` and vector
space `v`.  Hence it is possible to make an animation by using the
`mkActive` function and specifying a function from time to diagrams.

However, most often, animations are constructed using the
`Applicative` interface.  For example, to create a moving circle we
can write

.. class:: lhs

::

> translateX <$> ui <*> circle 2

`diagrams-cairo`:pkg: includes a very primitive animation rendering
function, `animMain`, which takes an animation and spits out a bunch
of image files, one for each frame.  You can then assemble the
generated frames into an animation using, *e.g.*, ``ffmpeg``. (More
sophisticated animation rendering will be added in future releases.)
If you use `animMain` to visualize the above animation, however, you
will find that all the generated frames look the same---the circle is
not moving!

Actually, it *is* moving, it's just that it gets centered in the
output at each instant. It's as if the viewport is panning along at
the same rate as the circle, with the result that it appears
stationary.  The way to fix this is by placing the moving circle on
top of something larger and stationary in order to "fix" the
viewpoint.  Let's use an invisible square:

.. class:: lhs

::

> (translateX <$> ui <*> circle 2) <> (pure (square 6 # lw 0))

Notice that we composed two animations using `(<>)`, which does
exactly what you would think: superimposes them at every instant in time.

Since this is such a common thing to want, the
`Diagrams.Animation`:mod: module provides a function `animEnvelope`
for expanding the envelope of an animation to the union of all the
envelopes over time (determined by sampling at a number of points).  That
is, the animation will now use a constant envelope that encloses the
entirety of the animation at all points in time.

.. class:: lhs

::

> animEnvelope (translateX <$> ui <*> circle 2)

Since `Active` is generic, it is also easy (and useful) to
create active `Point`\s, `Path`\s, colors, or values of any other type.

.. container:: todo

  * Examples of animating things other than diagrams

Rendering backends
==================

Diagrams has a system for "pluggable" rendering backends, so new
backends can be added by implementing instances of some type classes.
There are currently four "officially supported" backends, described
below, and several other experimental backends.  New backends are
welcome!  To get started, take a look at the existing backends for
examples, and read the sections below on `Tools for backends`_ and
`Backends`_.

The SVG backend
---------------

The SVG backend, `diagrams-svg`:pkg:, outputs SVG files.  It is the
default "out-of-the-box" backend, i.e. what one gets by typing just
``cabal install diagrams``.  It is implemented purely in Haskell, with
no dependencies on external libraries via the FFI.  This means that it
should be easy to install on all platforms.

Note that at the moment the SVG backend does not yet support embedding
images, but this is planned for a future release.  Otherwise, the SVG
backend is on a par with the cairo backend in terms of features
(excluding a few special features specific to the cairo backend,
described above).  For information on making use of the SVG backend,
see `Diagrams.Backend.SVG`:mod:.

The source code for the SVG backend can be found in the
`diagrams-svg`:repo: repository.

The cairo backend
-----------------

The cairo backend, `diagrams-cairo`:pkg:, is built on top of the
`cairo`:pkg: package, which contains bindings to the `cairo 2D
graphics library`_.  Although it is quite full-featured, the cairo
library itself can be unfortunately difficult to install on some
platforms, particularly OS X.

.. _`cairo 2D graphics library`: http://www.cairographics.org/

The cairo backend can produce PNG, SVG, PDF, and postscript output.
For specific information on how to make use of it, see the
documentation for the `Diagrams.Backend.Cairo`:mod: module.

``diagrams-cairo`` was the first officially supported backend, and has
quite a few advanced features that other backends do not have:

* `Diagrams.Backend.Cairo.Text`:mod: has functions for working with
  text and creating diagrams from text with proper bounding boxes
  (though currently it seems a bit buggy).

* `Diagrams.Backend.Cairo.List`:mod: exports the `renderToList`
  function, which can convert a 2D diagram to a matrix of pixel color
  values.

* `Diagrams.Backend.Cairo.Ptr`:mod: exports functions for rendering
  diagrams directly to buffers in memory.

The source code for the cairo backend can be found in the
`diagrams-cairo`:repo: repository.

The GTK backend
---------------

The GTK backend, `diagrams-gtk`:pkg:, used to be part of the cairo
backend (and is still built on top of it), but has been split out into
a separate package in order to reduce the dependencies of the cairo
backend, hence making it easier to install for those who don't need
GTK support.

The GTK backend allows rendering diagrams directly to GTK windows
instead of to a file.  Note that it is possible to receive mouse
clicks and then query the corresponding location in a diagram to find
out which part the user clicked on (see `Using queries`_).

The source code for the GTK backend can be found in the
`diagrams-gtk`:repo: repository.

The postscript backend
----------------------

The postscript backend, `diagrams-postscript`:pkg:, like the SVG
backend, is written purely in Haskell.  It outputs encapsulated
PostScript (EPS) files.  Note that by nature, EPS does not support
transparency.  The postscript backend also does not support embedded
images.  However, it is fairly complete in its support for other
features.

The source code for the postscript backend can be found in the
`diagrams-postscript`:repo: repository.

Other backends
--------------

For a list of other backends and their status, see `the diagrams
wiki`_.

.. _`the diagrams wiki`: http://www.haskell.org/haskellwiki/Diagrams/Projects#Backends

Tools for backends
------------------

* `Diagrams.Segment`:mod: exports a `FixedSegment` type, representing
  segments which *do* have an inherent starting location. Trails and
  paths can be "compiled" into lists of `FixedSegment`\s with absolute
  locations using `fixTrail` and `fixPath`.  This is of interest to
  authors of rendering backends that do not support relative drawing
  commands.

* A test harness for comparing the outputs of different backends can be
  found in the `diagrams-backend-tests`:repo: repo; the output of the
  test harness for all officially supported backends is `kept up-to-date
  here <http://projects.haskell.org/diagrams/backend-tests/all-index.html>`_.

Core library
============

This chapter explains the low-level inner workings of
`diagrams-core`:pkg:.  Casual users of ``diagrams`` should not need to
read this section (although a quick skim may well turn up something
interesting).  It is intended more for developers and power users who
want to learn how ``diagrams`` actually works under the hood.

Chris Mears has written `an article giving a quick walkthrough of some
of the internals
<http://www.cmears.id.au/articles/diagrams-internals.html>`_ which is
useful for getting started.

The remainder of this section is organized around the modules in
`diagrams-core`:pkg:.  At some level, there is no substitute for just
diving in and reading the source code (see the `diagrams-core`:repo:
repository), but the hope is that this chapter can serve to orient you
and supply useful commentary.

Diagrams.Core.V
---------------

`Diagrams.Core.V`:mod: contains the definition of the `V` type family
(see the `V`_ section in the `Type family reference`_) along with some
basic instances.

Diagrams.Core.Points
--------------------

.. container:: todo
  
  Should some of this go elsewhere?

Diagrams makes a strong type distinction between vectors and points.
The `vector-space-points`:pkg: package defines a `Point` type for
points built on top of the `vector-space`:pkg: package, along with an
"unsafe" `unPoint` function, the `origin` constant for referring to
the origin of a vector space, and the `(*.)` operator for scaling a
point.

The `Diagrams.Core.Points`:mod: module simply re-exports a few things
from `vector-space-points`:pkg:, and defines an instance of `V` for
`Point`.

Diagrams.Core.Names
-------------------

`Diagrams.Core.Names`:mod: defines the infrastructure for names which
can be used to identify subdiagrams.

`AName`, representing *atomic names*, is an existential wrapper,
allowing (almost) any type to be used for names, as the user finds
convenient.  Strings may be used of course, but also numbers,
characters, even user-defined types.  The only restriction is that the
wrapped type must be an instance of the following three classes:

* `Typeable` (so values can be pulled back out of the wrappers in a
  type-safe way),
* `Show` (so names can be displayed, for debugging purposes), and
* `Ord` (in order to be able to create a `Map` from names to
  subdiagrams).

Equality on atomic names works as expected: two names are equal if their
types match and their values are equal.

The `Ord` instance for atomic names works by first ordering names
according to (a `String` representation of) their type, and then by
value for equal types (using the required `Ord` instance).

A *qualified name* (`Name`) is a list of atomic names.  The `IsName`
class covers things which can be used as a name, including many
standard base types as well as `ANames` and `Names`.  Most user-facing
functions which take a name as an argument actually take any type with
an `IsName` constraint, so the user can just pass in a `String` or an
`Int` or whatever they want.

The motivation for having names consist of *lists* of atomic names is
that it is not always convenient or even feasible to have globally
unique names (especially when multiple modules by different authors
are involved).  In such a situation it is possible to *qualify* all
the names in a particular diagram by some prefix.  This operation
governed by the `Qualifiable` class, containing the function ``(|>) ::
IsName a => a -> q -> q`` for performing qualification.

Diagrams.Core.HasOrigin
-----------------------

Diagrams.Core.Juxtapose
-----------------------

Diagrams.Core.Transform
-----------------------

Diagrams.Core.Envelope
----------------------

Diagrams.Core.Trace
-------------------

Diagrams.Core.Query
-------------------

Diagrams.Core.Style
-------------------

Diagrams.Core.Types
-------------------

QDiagram
~~~~~~~~

Backends
~~~~~~~~


Related projects
================

.. container:: todo  XXX

   * diagrams-spiro
   * diagrams-ghci
   * diagrams-hint
   * hs-logo
   * gtk-toy-diagrams

   * see diagrams wiki
