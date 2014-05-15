.. role:: pkg(literal)
.. role:: hs(literal)
.. role:: mod(literal)
.. role:: repo(literal)

.. default-role:: hs
.. sectnum:: :depth: 2

.. contents:: :depth: 2

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
  rendering backends, arbitrary graphics primitives, and multiple
  vector spaces (2D, 3D, ...).

About this document
-------------------

This document attempts to explain all major aspects of using the
``diagrams`` core and standard libraries, organized by topic to make
it easy to find what you are looking for.  It is not, however, a
complete reference of every single function in the standard library:
for that, see the API documentation listed under `Other resources`_.
Most sections contain links to relevant modules you can follow to
read about other functions not covered in the text.

Module names in the text are typeset like this:
`Diagrams.Prelude`:mod:.  Click on a module name to visit its
documentation.  You can also click on any function or operator name in
code examples to take you to its documentation.  Try it:

.. class:: lhs

::

  example = circle 2 ||| pentagon 3

Mathematical equations are typeset using MathJax_:

`\sum_{k=1}^\infty \frac{1}{k^2} = \frac{\pi^2}{6}`:math:

Right-click on any equation to access MathJax options, like displaying
the LaTeX source, switching between MathML and HTML/CSS for display,
zoom settings, and so on.

.. _MathJax: http://www.mathjax.org/

Occasionally content may be missing or incomplete; this is noted by a
light blue box with a "document" icon on the right hand side, like
this:

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
* The ``diagrams`` website_ has a `gallery of examples`_ and a
  `list of tutorials`_, as well as links to blog posts and
  other documentation.
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
.. _`list of tutorials`: http://projects.haskell.org/diagrams/documentation.html
.. _`diagrams wiki`: http://haskell.org/haskellwiki/Diagrams
.. _`gallery of examples`: http://projects.haskell.org/diagrams/gallery.html
.. _`IRC channel on Freenode`: http://webchat.freenode.net/?channels=diagrams
.. _`diagrams mailing list`: http://groups.google.com/group/diagrams-discuss?pli=1
.. _`developer wiki`: http://code.google.com/p/diagrams/
.. _`diagrams organization on github` : https://github.com/diagrams/

Installation
------------

Before installing ``diagrams``, you will need the following:

* The `Glasgow Haskell Compiler`_ (GHC), version 7.4.x or later
  (7.6.3 is recommended).

* It is recommended (but not required) to have the latest release of
  the `Haskell Platform`_ (currently 2013.2.0.0).  At the very least
  you will want the `cabal-install`_ tool.  Diagrams is always
  tested on at least two versions of the Haskell Platform (the
  current and previous releases), and may work on earlier HP
  releases as well.

.. _`cabal-install`: http://hackage.haskell.org/trac/hackage/wiki/CabalInstall

If you are on OS X or Windows, GHC itself comes with the Haskell
Platform; if you are on Linux, you will have to install GHC first.

.. _`Glasgow Haskell Compiler`: http://www.haskell.org/ghc/
.. _`Haskell Platform`: http://hackage.haskell.org/platform/

Once you have successfully installed the Haskell platform, installing
``diagrams`` should be as easy as issuing the command:

::

  cabal install diagrams -jN

where ``N`` is the number of cores you wish to use for
compilation.

The `diagrams`:pkg: package is a convenience wrapper that simply pulls
in (by default) four other packages:

* `diagrams-core`:pkg: (core data type definitions and utilities),
* `diagrams-lib`:pkg: (standard primitives and combinators),
* `diagrams-contrib`:pkg: (user-contributed extensions), and
* `diagrams-svg`:pkg: (Haskell-native backend generating SVG files).

There is also a Haskell-native `postscript backend`_, which supports
all features except transparency.  To get it, add the ``-fps`` flag:

::

  cabal install -fps diagrams

.. _`postscript backend`: http://github.com/diagrams/diagrams-postscript/

There is also a backend based on the `cairo graphics
library`_; it has support for more
features than the SVG backend and additional output formats (PNG, PS,
PDF), but can be much more difficult to install on some platforms
(notably OS X).  If you want the cairo backend, you can issue the
command

.. _`cairo graphics library`: http://www.cairographics.org/

::

  cabal install gtk2hs-buildtools
  cabal install -fcairo diagrams

(You can omit ``gtk2hs-buildtools`` if you have already installed it
previously, though note that you may need to reinstall it if you are
building under GHC 7.6 and the last time you installed
``gtk2hs-buildtools`` was sufficiently long ago---otherwise you may
get FFI-related errors when building the `cairo`:pkg: package.)

You can also mix and match these flags to get multiple backends.
Note, if you don't want the SVG backend at all, you must add the
``-f-svg`` flag to disable it.

`See the wiki for the most up-to-date information`_ regarding
installation.  If you have trouble installing diagrams, feel free to
send email to the `diagrams mailing list`_; we would like to collect
reports of problems and solutions on various platforms.

.. _`See the wiki for the most up-to-date information`: http://www.haskell.org/haskellwiki/Diagrams/Install


Getting started
---------------

Create a file called ``TestDiagram.hs`` (or whatever you like) with
the following contents:

.. class:: lhs

::

  {-# LANGUAGE NoMonomorphismRestriction #-}

  import Diagrams.Prelude
  import Diagrams.Backend.SVG.CmdLine
  -- or:
  -- import Diagrams.Backend.Cairo.CmdLine
  -- or:
  -- import Diagrams.Backend.Postscript.CmdLine
  -- or:
  -- import Diagrams.Backend.Rasterific.CmdLine

  myCircle :: Diagram B R2
  myCircle = circle 1

  main = mainWith myCircle

The first line turns off the `dreaded monomorphism restriction`_, which is
quite important when using ``diagrams``: otherwise you will probably
run into lots of crazy error messages.

.. _`dreaded monomorphism restriction`: http://www.haskell.org/haskellwiki/Monomorphism_restriction

`Diagrams.Prelude`:mod: re-exports most everything from the standard
library; `Diagrams.Backend.SVG.CmdLine`:mod: provides a command-line
interface to the SVG rendering backend.  We then declare `myCircle` to
have the type `Diagram B R2`: the `R2` part means that it is a
two-dimensional diagram; `B` is an alias for a tag representing the
particular backend.  All the backends export `B` as an alias for
themselves, so you can switch backends just by changing an import,
without having to change type annotations on your diagrams; `B` simply
refers to whichever backend is in scope.  Finally, `mainWith` takes a
diagram and creates a command-line-driven executable for rendering it.

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

If you are using the cairo backend you can also request a ``.png``,
``.ps``, or ``.pdf`` file (the format is automatically determined by
the extension), or an ``.eps`` file if using the postscript
backend. The rasterific backend allows ``.png``, ``.jpg``, ``.tif``,
and ``.bmp``.

Try typing

::

  $ ./TestDiagram --help

to see the other options that are supported.

To get started quickly, you may wish to continue by reading the `quick
start tutorial`_; or you can continue reading the rest of this user
manual.

.. _`quick start tutorial`: /doc/quickstart.html

Note that `Diagrams.Backend.SVG.CmdLine` is provided for convenience;
it's not the only interface to the backend though. If you want to roll
your own code, e.g. as a component of another program, use the
`renderDia` function, or see the related section under `Rendering
backends`_ for additional backend specific entry points.

Contributing
------------

``diagrams`` is an open-source project, and contributions are
encouraged!  All diagrams-related repositories are in the `diagrams
organization`_ on github.  The `Contributing page`_ on the
diagrams wiki explains how to get the repositories and make
contributions.  To find out about the latest developments, join the
``#diagrams`` IRC channel on Freenode, and check out the `diagrams
Trello board`_.

.. _`diagrams organization`: http://github.com/diagrams
.. _`Contributing page`: http://www.haskell.org/haskellwiki/Diagrams/Contributing
.. _`diagrams Trello board`: https://trello.com/b/pL6YdKgz/diagrams

Essential concepts
==================

Before we jump into the main content of the manual, this chapter
explains a number of general ideas and central concepts that will
recur throughought.  If you're eager to skip right to the good stuff,
feel free to skip this section at first, and come back to it when
necessary; there are many links to this chapter from elsewhere in the
manual.

Semigroups and monoids
----------------------

A *semigroup* consists of

* A set of elements `S`:math:
* An *associative binary operation* on the set, that is, some
  operation

  `\oplus \colon S \to S \to S`:math:

  for which

  `(x \oplus y) \oplus z = x \oplus (y \oplus z).`:math:

A *monoid* is a semigroup with the addition of

* An *identity element* `i \in S`:math: which is the identity for
  `\oplus`:math:, that is,

  `x \oplus i = i \oplus x = x.`:math:

In Haskell, semigroups are expressed using the `Semigroup` type class
from the `semigroups`:pkg: package:

.. class:: lhs

::

  class Semigroup s where
    (<>) :: s -> s -> s

and monoids are expressed using the `Monoid` type class, defined in
``Data.Monoid``:

.. class:: lhs

::

  class Monoid m where
    mempty  :: m
    mappend :: m -> m -> m

The `mappend` function represents the associative binary operation,
and `mempty` is the identity element.  (`mappend` and `(<>)` should
always be the same; there are two different functions for historical
reasons.) A function

.. class:: lhs

::

  mconcat :: Monoid m => [m] -> m

is also provided as a shorthand for the common operation of combining
a whole list of elements with `(<>)`/`mappend`.

Semigroups and monoids are used extensively in ``diagrams``: diagrams,
transformations, envelopes, traces, trails, paths, styles, colors, and
queries are all instances of both `Semigroup` and `Monoid`.

Faking optional named arguments
-------------------------------

Many diagram-related operations can be customized in a wide variety of
ways.  For example, when creating a regular polygon, one can customize
the number of sides, the radius, the orientation, and so on. However,
to have a single function that takes all of these options as separate
arguments would be a real pain: it's hard to remember what the arguments are
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
and override only the fields you want, like this:

.. class:: lhs

::

> foo (def & arg1 .~ someValue & arg6 .~ blah)

This is using machinery from the `lens`:pkg: package; but you don't have to
understand `lens`:pkg:, or know anything beyond the above syntax in
order to use diagrams (for convenience, diagrams re-exports the `(&)`
and `(.~)` operators from `lens`:pkg:).  In fact, in most cases, you
can also use record update syntax instead (note the underscores):

::

  foo (def { _arg1 = someValue, _arg6 = blah })

In some cases, however, the lens library is used to provide convenient
"virtual" fields which do not correspond to real record fields; for
example, `headColor` can be used to set the color of an arrowhead,
even though the arrow options record actually contains a general style
instead of just a color.

Finally, note that ``diagrams`` also defines `with` as a synonym for
`def`, which can read a bit more nicely.  So, instead of the above, you
could write

::

  foo (with & arg1 .~ someValue & arg6 .~ blah)

Most functions which take an optional arguments record have two
variants: one named `foo` which uses all default arguments, and one
named `foo'` (with a trailing prime) which takes an options record.

Vectors and points
------------------

Although much of this user manual focuses on constructing
two-dimensional diagrams, the definitions in the core library in fact
work for *any* vector space.  Vector spaces are defined in the
`Data.VectorSpace`:mod: module from Conal Elliott's `vector-space`:pkg: package.

Many objects (diagrams, paths, backends...) inherently live in some
particular vector space.  The vector space in which a given type
"lives" can be computed by the type function `V`.  So, for example,
the type

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

See `this tutorial for a more in-depth introduction to working with vectors
and points`__.

__ vector.html

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
`vector-space-points`:pkg: package defines a newtype wrapper around
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
>     [arrowAt' (with & arrowHead .~ tri) origin v
>     , origin ~~ b
>       # lc green # lw veryThick
>     , p1 ~~ p2
>       # lc red
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

Measurement units
-----------------

Certain attributes (such as line width, dashing size, arrowhead size,
and font size) can be specified with respect to several different
reference frames.  For example, the lines used to draw a certain
square can be specified as an absolute two pixels wide, or as a
certain percentage of the size of the final diagram, or in units
relative to the size of the square.  More specifically, values of type
`Measure v` represent `Scalar v` values, interpreted in one of four
"reference frames": `Local`, `Global`, `Normalized`, or `Output`,
described below in turn.

In addition to the four reference frames described here, it is
possible to combine them into more complex expressions using a small
DSL for specifying measurements; see `Measurement expressions`_.

Local units
~~~~~~~~~~~

`Local` units are the most straightforward to explain.  Values in
`Local` units are interpreted in the context of the *local* vector
space, just as most other length measurements (*e.g.* arguments to
functions like `circle` and `square`).  For example, `square 1 # lwL
0.2` specifies a square which is drawn with lines one fifth as wide as
its sides are long---and will *always* be, even if it is scaled: the
line width scales right along with the square. (Note that `lwL`
specifies the line width using `Local` units, and is a synonym for `lw
. Local`.),

.. class:: dia-lhs

::

> localSq = square 1 # lwL 0.2
> example =
>   hcat' (with & sep .~ 0.5)
>   [localSq, localSq # scale 2, localSq # scaleX 2]

It's important to note that---as illustrated by the third figure in
the above picture---line width always scales uniformly, even when a
non-uniform scaling is applied.  Previous versions of diagrams had a
`freeze` operation which could be used to apply non-uniform scaling to
lines; to achieve such an effect, you can first turn a stroked line
into a closed path, as described in `Offsets of segments, trails, and
paths`_.

A important consequence of `Local` units having the *current* vector
space as their reference is that attribute-setting functions such as
`lwL` do *not* commute with transformations.

.. class:: dia-lhs

::

> example =
>   hcat' (with & sep .~ 0.5)
>   [ square 1 # lwL 0.2 # scale 2
>   , square 1 # scale 2 # lwL 0.2
>   ]
>   # frame 0.5

Global units
~~~~~~~~~~~~

Whereas `Local` values are interpreted in the current, "local" vector
space, `Global` values are interpreted in the final, "global" vector
space of the diagram that is rendered.  In the following example,
`theSq` is specified as having a `Global` line width of `1`; five
differently-scaled copies of the square are laid out, so that the entire
scaled diagram has a width of around `6` units.  The lines, having a
line width of `Global 0.05`, are thus about 0.8% of the width of the
entire diagram.

.. class:: dia-lhs

::

> theSq = square 1 # lwG 0.05
>
> example =
>   hcat' (with & sep .~ 0.2)
>     (map (\s -> theSq # scale s) [0.5, 0.8, 1, 1.5, 2])

Versions of ``diagrams`` prior to `1.2` actually had a semantics for
`lw` equivalent to `lwG`.  One advantage, as can be seen from the
above example, is that different shapes having the same `Global` line
width, even when differently scaled, will all be drawn with the same
apparent line width. However, `Normalized` and `Output` have that
property as well, and are probably more useful; the problem with
`Global` units is that in order to decide on values, one has to know
the final size of the diagram, which is not typically something one
knows in advance.  In particular, note that applying something like
`scale 20` to the `example` above---a seemingly innocuous
change---would result in extremely thin lines (or even invisible,
depending on the backend), as shown below.  Making this look
reasonable again would require changing the argument to `lwG`.

.. class:: dia-lhs

::

> theSq = square 1 # lwG 0.05
>
> example =
>   hcat' (with & sep .~ 0.2)
>     (map (\s -> theSq # scale s) [0.5, 0.8, 1, 1.5, 2])
>   # scale 20

In short, `Global` units tend to go against ``diagrams`` emphasis on
local, scale-invariant thinking.  They were left in for backwards
compatibility, and because they can occasionaly be useful in special
situations where you do already have some absolute, global coordinate
system in mind: for example, if you know you want to construct a
100x100 diagram using lines that are 1 unit wide.

Normalized units
~~~~~~~~~~~~~~~~

`Normalized` units, like `Global` units, are measured with respect to
the final size of a diagram. However, for the purposes of interpreting
`Normalized` units, the diagram is considered to be one "normalized
unit" in both height and width.  For example, a `Normalized` value of
`0.1` means "10% of the height/width of the final diagram".  Thus,
scaling the diagram has no effect on the relative size of the lines
(just as with `Local`), but lines look consistent even across shapes
that have been scaled differently (as with `Global`).

.. class:: dia-lhs

::

> theSq = square 1 # lwN 0.01
>
> example =
>   hcat' (with & sep .~ 0.2)
>     (map (\s -> theSq # scale s) [0.5, 0.8, 1, 1.5, 2])
>   # scale 20

Note that the `scale 20` threatened in the `Global` example has been
applied here, but makes no difference: changing the `20` to any other
nonzero value has no effect on the appearance of the rendered diagram.

Output units
~~~~~~~~~~~~

Values measured in `Output` units are interpreted with respect to the
*requested output size* of a diagram.  Sometimes you really do know
that you want your lines to be exactly 1/2 inch wide when printed.  In
this case, scaling a diagram will preserve its appearance, but
requesting a different output size might not.

.. container:: todo

  Expand on this.  Show some examples.  Need a better story about
  physical units.

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

*Flexibility*, *power*, *simplicity*: in general, you can have any two
of these but not all three.  Diagrams chooses *flexibility* and
*power*, at the expense of *simplicity*. (In comparison, the excellent
`gloss`:pkg: library instead chooses *flexibility* and *simplicity*.)
In particular, the types in the diagrams library can be quite
intimidating at first.  For example, `hcat` is a function which takes
a list of diagrams and lays them out in a horizontal row.  So one
might expect its type to be something like `[Diagram] -> Diagram`.  In
actuality, its type is

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

* `R2` is the type of the two-dimensional Euclidean vector
  space. Standard ``diagrams`` backends render images with the
  positive `x`:math:\-axis extending to the right, and the positive
  `y`:math:\-axis extending *upwards*.  This is consistent with standard
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
  dimension.  Vectors can be created using the syntax `(x ^& y)` and
  pattern-matched by calling `coords` and then matching on the pattern
  `(x :& y)`.

  For more in-depth information on working with `R2`, `see this
  tutorial`__.

  __ vector.html

* `P2` is the type of points in two-dimensional space. It is a synonym
  for `Point R2`.  The distinction between points and vectors is
  important; see `Vectors and points`_.

  Points can be created from pairs of coordinates using `p2` and
  converted back using `unp2`. They can also be constructed and
  destructed using the same syntax as for vectors, as defined in
  `Diagrams.Coordinates`:mod:.

  For more in-depth information on working with `P2`, `see this
  tutorial`__.

  __ vector.html

* `T2` is the type of two-dimensional affine transformations.  It is a
  synonym for `Transformation R2`.

Angles
~~~~~~

The `Angle` type represents two-dimensional angles.  Angles can be
expressed in radians, degrees, or fractions of a circle. Isomorphisms
`turn`, `rad`, and `deg` are provided (represented using the `Iso`
type from the `lens`:pkg: package), which convert between abstract
`Angle` values and `Double` values with various units.  To construct
an `Angle`, use the `(@@)` operator, as in `(3 @@ deg)` or `(3 @@
rad)`. To project an `Angle` back to a `Double`, use the `(^.)`
operator, as in `someAngle ^. rad`.

* `turn` represents fractions of a circle.  A value of `1 @@ turn` represents
  a full turn, `1/4 @@ turn` constructs a right angle, and so on.  The
  measure of an Angle ``a`` in turns (represented with `Double`)
  can be obtained using `a ^. turn`.
* `rad` represents angles measured in radians.  A value of `tau` (that
  is, `\tau = 2 \pi`:math:) represents a full turn. (If you haven't heard of
  `\tau`:math:, see `The Tau Manifesto`__.)
* `deg` represents angles measured in degrees.  A value of `360`
  represents a full turn.

__ http://tauday.com

`fullTurn :: Angle` represents one full turn, equivalent to `1 @@
turn`, `tau @@ rad`, or `360 @@ deg`.

The `direction` function computes the direction of a vector,
represented by an angle measured clockwise from the positive
`x`:math:\-axis (shown in green below).

.. class:: dia

::

> example = mconcat
>   [ exampleVector
>   , angleArrow
>   , axes
>   ]
>   # (<> rect 12 6 # alignB # lw none)
>   # center # frame 1
>
> axes = (arrowV (6 *^ unitX) # centerX <> arrowV (6 *^ unitY) # centerY)
> theDir = 200 @@ deg
> theV = 3 *^ fromDirection theDir
> exampleVector = arrowV theV
>   # lc blue
> angleArrow = arrowBetween' (with & arrowShaft .~ arc zeroV theDir)
>   (origin .+^ (1 *^ unitX))
>   (origin .+^ (theV # normalized))
>   # dashingG [0.05,0.05] 0
>   # lc green

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
counterclockwise to the second, as well as `wedge` which constructs a
wedge shape, `annularWedge` (an arc plus two radii) and various other
functions for conveniently constructing arcs.

__ `Angles`_

.. class:: dia-lhs

::

> example = hcat' (with & sep .~ 0.5) [arc a1 a2, wedge 1 a1 a2, annularWedge 1 0.6 a1 a2]
>   where
>     a1 = tau/4 @@ rad
>     a2 = 4 * tau / 7 @@ rad

Pre-defined shapes
~~~~~~~~~~~~~~~~~~

`Diagrams.TwoD.Shapes`:mod: provides a number of pre-defined
polygons and other path-based shapes.  For example:

* `triangle` constructs an equilateral triangle with sides of a
  given length.
* `square` constructs a square with a given side length; `unitSquare`
  constructs a square with sides of length `1`.
* `pentagon`, `hexagon`, ..., `dodecagon` construct other regular
  polygons with sides of a given length. (For constructing polygons
  with a given *radius*, see `General polygons`_.)
* In general, `regPoly` constructs a regular polygon with any number
  of sides.
* `rect` constructs a rectangle of a given width and height.
* `roundedRect` constructs a rectangle with circular rounded corners.
* `roundedRect'` works like `roundedRect` but allowing a different radius to be set for each corner, using `RoundedRectOpts`.

.. class:: dia-lhs

::

> example = square 1
>       ||| rect 0.3 0.5
>       ||| triangle 1
>       ||| roundedRect  0.5 0.4 0.1
>       ||| roundedRect  0.5 0.4 (-0.1)
>       ||| roundedRect' 0.7 0.4 (with & radiusTL .~ 0.2
>                                      & radiusTR .~ -0.2
>                                      & radiusBR .~ 0.1)

Completing the hodgepodge in `Diagrams.TwoD.Shapes`:mod: for now, the
functions `hrule` and `vrule` create horizontal and vertical lines,
respectively.

.. class:: dia-lhs

::

> example = c ||| hrule 1 ||| c
>   where c = circle 1 <> vrule 2

General polygons
~~~~~~~~~~~~~~~~

The `polygon` function from `Diagrams.TwoD.Polygons`:mod: can be used
to construct a wide variety of polygons.  Its argument is a record of
optional parameters that control the generated polygon:

* `polyType` specifies one of several methods for determining the
  vertices of the polygon:

  * `PolyRegular` indicates a regular polygon with a certain number
    of sides and a given *radius*.

    .. class:: dia-lhs

    ::

    > example = strutX 1 ||| p 6 ||| p 24 ||| strutX 1
    >   where p n = polygon (with
    >                 & polyType .~ PolyRegular n 1 )

  * `PolySides` specifies the vertices using a list of angles
    between edges, and a list of edge lengths.

    .. class:: dia-lhs

    ::

    > example = polygon ( with
    >   & polyType .~ PolySides
    >       [ 20 @@ deg, 90 @@ deg, 40 @@ deg, 100 @@ deg ]
    >       [ 1        , 5        , 2        , 4          ]
    >   )

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

> poly1 = polygon ( with & polyType  .~ PolyRegular 13 5
>                        & polyOrient .~ OrientV )
> poly2 = polygon ( with & polyType  .~ PolyPolar (repeat (1/40 @@ turn))
>                                                 (take 40 $ cycle [2,7,4,6]) )
> example = (poly1 ||| strutX 1 ||| poly2)

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
`TrailLike` type (see the section about `TrailLike`_), since a list of
points is an instance of the `TrailLike` class.  But of course, you are
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
  if the argument to `StarSkip` and the number of vertices have a
  nontrivial common divisor.

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
  > visualize f	  = stroke' (with & vertexNames .~ [[0 .. 6 :: Int]] )
  >                     (regPoly 7 1)
  >                   # lw none
  >                   # showLabels
  >                   # fontSize (Local 0.6)
  >              <> star (StarFun f) (regPoly 7 1)
  >                   # stroke # lw thick # lc red
  > example       = center . hcat' (with & sep .~ 0.5) $ map visualize funs

You may notice that all the above examples need to call `stroke` (or
`stroke'`), which converts a path into a diagram.  Many functions
similar to `star` are polymorphic in their return type over any
`TrailLike`, but `star` is not. As we have seen, `star` may need to
construct a path with multiple components, which is not supported by
the `TrailLike` class.

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

As noted before, diagrams form a monoid_ with composition given by
superposition.  `atop` is simply a synonym for `mappend` (or `(<>)`),
specialized to two dimensions.

.. _monoid: `Semigroups and monoids`_

This also means that a list of diagrams can be stacked with `mconcat`;
that is, `mconcat [d1, d2, d3, ...]` is the diagram with `d1` on top
of `d2` on top of `d3` on top of...

.. class:: dia-lhs

::

> example = mconcat [ circle 0.1 # fc green
>                   , triangle 1 # scale 0.4 # fc yellow
>                   , square 1   # fc blue
>                   , circle 1   # fc red
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
diagram is the same as the local origin of the first diagram.  This
makes `beside v` associative, so diagrams under `beside v` form a
semigroup.  In fact, they form a monoid, since `mempty` is a left and
right identity for `beside v`, as can be seen in the example below:

.. class:: dia-lhs

::

> example = hcat' (with & sep .~ 1) . map showOrigin
>         $ [ d, mempty ||| d, d ||| mempty ]
>   where d = square 1

In older versions of ``diagrams``, the local origin of the combined
diagram was at the point of tangency between the two diagrams.  To
recover the old behavior, simply perform an alignment on the first
diagram in the same direction as the argument to `beside` before
combining (see `Alignment`_):

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
> example = circles ||| strutX 1 ||| (circles <> square 1)
>   where circles = mconcat [d1, d2, d3]

See `envelopes and local vector spaces`_ for more information on what
"next to" means, and `Envelopes`_ for information on
functions available for manipulating envelopes.  To learn about how
envelopes are implemented, see the `core library reference`__.

__ core.html

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

> example = cat (r2 (2, -1)) (map p [3..8]) # showOrigin
>   where p n = regPoly n 1

Semantically, `cat v === foldr (beside v) mempty`, although the actual
implementation of `cat` uses a more efficient balanced fold.

For more control over the way in which the diagrams are laid out, use
`cat'`, a variant of `cat` which also takes a `CatOpts` record.  See
the documentation for `cat'` and `CatOpts` to learn about the various
possibilities.

.. class:: dia-lhs

::

> example = cat' (r2 (2,-1)) (with & catMethod .~ Distrib & sep .~ 2 ) (map p [3..8])
>   where p n = regPoly n 1 # scale (1 + fromIntegral n/4)
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

> c        = circle 1
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
attributes as well; for details, see the `core library reference`__.

__ core.html

In many examples, you will see attributes applied to diagrams using
the `(#)` operator.  Keep in mind that there is nothing special about
this operator as far as attributes are concerned. It is merely
backwards function application, which is used for attributes since it
often reads better to have the main diagram come first, followed by
modifications to its attributes.  See `Postfix transformation`_.

In general, inner attributes (that is, attributes applied earlier)
override outer ones.  Note, however, that this is not a requirement.
Each attribute may define its own specific method for combining
multiple values.  Again, see the `core library reference`__ for more
details.

__ core.html

Most of the attributes discussed in this section are defined in
`Diagrams.TwoD.Attributes`:mod:.

Texture
~~~~~~~

Two-dimensional diagrams can be filled and stroked with a `Texture`. A
`Texture` can be either a solid color, a linear gradient or a radial
gradient. Not all backends support gradients, in particular gradients are
supported by the SVG, Cairo, and Rasterific backends (see `Rendering backends`_).
Future releases should also support patterns as textures. The data type
for a texture is

.. class:: lhs

::

> data Texture = SC SomeColor | LG LGradient | RG RGradient

and `Prism` s `_SC`, `_LG`, `_RG` are provided for access.

Color
+++++

The color used to stroke the paths can be set with the `lc` (line color)
function and the color used to fill them with the `fc` (fill color) function.

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
transparent colors you can use `lcA` and `fcA`. The `palette`::pkg: package
provides additional sets of colors and algorithms for creating harmonious
color combinations.

.. class:: dia-lhs

::

> import Data.Colour (withOpacity)
>
> colors  = map (blue `withOpacity`) [0.1, 0.2 .. 1.0]
> example = hcat' (with & catMethod .~ Distrib & sep .~ 1 )
>                 (zipWith fcA colors (repeat (circle 1)))

Transparency can also be tweaked with the `Opacity` attribute, which
sets the opacity/transparency of a diagram as a whole. Applying
`opacity p` to a diagram, where `p` is a value between `0` and `1`,
results in a diagram `p` times as opaque.

.. class:: dia-lhs

::

> s c     = square 1 # fc c
> reds    = (s darkred ||| s red) === (s pink ||| s indianred)
> example = hcat' (with & sep .~ 1 ) . take 4 . iterate (opacity 0.7) $ reds

To "set the background color" of a diagram, use the `bg`
function---which does not actually set any attributes, but simply
superimposes the diagram on top of a bounding rectangle of the given
color.

.. class:: dia-lhs

::

> t = regPoly 3 1
>
> example = t ||| t # bg orange

Linear Gradients
++++++++++++++++

A linear gradient must have a list of color stops, a starting point, an ending point,
a transformation and a spread method. Color stops are pairs of (color, fraction) where
the fraction is usually between 0 and 1 that are mapped onto the start and end
points. The starting point and endping point are
specified in local coordinates. Typically the transformation starts as the identity
transform `mempty` and records any transformations that are applied to the object
using the gradient. The spread method defines how space beyond the starting and
ending points should be handled, `GradPad` will fill the space with the final stop
color, `GradRepeat` will restart the gradient, and `GradReflect` will restart the
gradient but with the stops reversed. This is the data type for a linear gradient.

.. class:: lhs

::

> data LGradient = LGradient
>   { _lGradStops        :: [GradientStop]
>   , _lGradStart        :: P2
>   , _lGradEnd          :: P2
>   , _lGradTrans        :: T2
>   , _lGradSpreadMethod :: SpreadMethod }

Lenses are provided to access the record fields. In addition the functions `mkStops` taking
a list of triples (color, fraction, opacity) and `mkLinearGradient` which takes a list of stops,
a start and end point, and a spread method and creates a `Texture` are provided for convenience.
In this example we demonstrate how to make linear gradients with the `mkLinearGradient`
functions and how to adjust it using the lenses and prisms.

.. class:: dia-lhs

::

> stops = mkStops [(gray, 0, 1), (white, 0.5, 1), (purple, 1, 1)]
> gradient = mkLinearGradient stops ((-0.5) ^& 0) (0.5 ^& 0) GradPad
> sq1 = square 1 # fillTexture  gradient
> sq2 = square 1 # fillTexture (gradient & _LG . lGradSpreadMethod .~ GradRepeat
>                                        & _LG . lGradStart .~ (-0.1) ^& 0
>                                        & _LG . lGradEnd .~ 0.1 ^& 0)
> sq3 = square 1 # fillTexture (gradient & _LG . lGradSpreadMethod .~ GradReflect
>                                        & _LG . lGradStart .~ (-0.1) ^& 0
>                                        & _LG . lGradEnd .~ 0.1 ^& 0)
>
> example = hcat' (with & sep .~ 0.25) [sq1, sq2, sq3]


Here we apply the gradient to the stroke only and give it starting and
ending points towards the corners.

.. class:: dia-lhs

::

> stops = mkStops [(teal, 0, 1), (orange, 1, 1)]
> gradient = mkLinearGradient stops ((-1) ^& (-1)) (1 ^& 1) GradPad
> example = rect 3 1 # lineTexture  gradient # lwO 15 # fc black # opacity 0.75

Radial Gradients
++++++++++++++++

Radial gradients are similar, only they begin at the perimeter of an inner cirlce and
end at the perimeter of an outer circle.

.. class:: lhs

::

> data RGradient = RGradient
>     { _rGradStops        :: [GradientStop]
>     , _rGradCenter0      :: P2
>     , _rGradRadius0      :: Double
>     , _rGradCenter1      :: P2
>     , _rGradRadius1      :: Double
>     , _rGradTrans        :: T2
>     , _rGradSpreadMethod :: SpreadMethod }

Where radius and center 0 are for the inner circle, and 1 for the outer circle.
In this example we place the inner circle off center and place a circle filled
with the radial gradient on top of a rectangle filled with a linear gradient
to create a 3D effect.

.. class:: dia-lhs

::

> radial = mkRadialGradient (mkStops [(white,0,1), (black,1,1)])
>                           ((-0.15) ^& (0.15)) 0.06 (0 ^& 0) 0.5
>                           GradPad
>
> linear = mkLinearGradient (mkStops [(black,0,1), (white,1,1)])
>                           (0 ^& (-0.5)) (0 ^& 0.5)
>                           GradPad
>
> example = circle 0.35 # fillTexture radial # lw none
>        <> rect 2 1 # fillTexture linear # lw none



Line width
++++++++++

Line width is actually more subtle than you might think.  Suppose you
create a diagram consisting of a square, and another square twice as
large next to it (using `scale 2`).  How should they be drawn?  Should
the lines be the same width, or should the larger square use a line
twice as thick?  (Note that similar questions also come up when
considering the dashing style used to draw some shapes---should the
size of the dashes scale with transformations applied to the shapes,
or not?) ``diagrams`` allows the user to decide, using `Measure R2`
values to specify things like line width (see `Measurement units`_).

In many situations, it is desirable to have lines drawn in a uniform
way, regardless of any scaling applied to shapes.  This is what
happens with line widths measured in `Global`, `Normalized` or
`Output` units, as in the following example:

.. class:: dia-lhs

::

> example = hcat
>   [ square 1
>   , square 1 # scale 2
>   , circle 1 # scaleX 3
>   ]
>   # dashingN [0.03,0.03] 0
>   # lwN 0.01

For line widths that scale along with a diagram, use `Local`; in this
case line widths will be scaled in proportion to the geometeric
average of the scaling transformatins applied to the diagram.

The `LineWidth` attribute is used to alter the *width* with which
paths are stroked. The most general functions that can be used to set
the line width are `lineWidth` and its synonym `lw`, which take an
argument of type `Measure R2`.  Since typing things like `lineWidth
(Normalized 0.01)` is cumbersome, there are also shortcuts provided:
`lwG`, `lwN`, `lwO`, and `lwL` all take an argument of type `Double`
and wrap it in `Global`, `Normalized`, `Ouput` and `Local`,
respectively.

There are also predefined `Measure R2` values with intuitive names,
namely, `ultraThin`, `veryThin`, `thin`, `medium`, `thick`,
`veryThick`, `ultraThick`, and `none` (the default is `medium`), which
should often suffice for setting the line width.

.. class:: dia-lhs

::

> line = strokeT . fromOffsets $ [unitX]
> example = vcat' (with & sep .~ 0.1)
>   [line # lw w | w <- [ultraThin, veryThin, thin,
>                        medium, thick, veryThick, ultraThick]]

In the above example, there is no discernible difference between
`ultraThin`, `veryThin`, and `thin`; these names all describe
`Normalized` measurements with a physical lower bound, so the physical
width of the resulting lines depends on the physical size of the
rendered diagram.  At larger rendering sizes the differences between
the smaller widths become apparent.

.. container:: todo

  Explain that they all have a minimum output size.  Should wait until
  we have a better idea what the heck output size actually means.

Note that line width does not affect the envelope of diagrams at all.
To stroke a line "internally", turning it into a `Path` value
enclosing the stroked area (which *does* contribute to the envelope),
you can use one of the functions described in the section `Offsets of
segments, trails, and paths`_.

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

> path = fromVertices (map p2 [(0,0), (1,0.3), (2,0), (2.2,0.3)]) # lwO 10
> example = center . vcat' (with & sep .~ 0.1 )
>           $ map (path #)
>             [ lineCap LineCapButt   . lineJoin LineJoinMiter
>             , lineCap LineCapRound  . lineJoin LineJoinRound
>             , lineCap LineCapSquare . lineJoin LineJoinBevel
>             , dashingN [0.03,0.06,0.09,0.03] 0
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

> foo = myFun (mempty # fontSize (Local 2) # lw none # fc green)

If the type `T` is an instance of `HasStyle`, then `[T]` is also.
This means that you can apply styles uniformly to entire lists of
diagrams at once, which occasionally comes in handy, for example, to
assign a default attribute to all diagrams in a list which do not
already have one:

.. class:: dia-lhs

::

> example = hcat $
>   [circle 1, square 2, triangle 2 # fc yellow, hexagon 1] # fc blue

Likewise, there are `HasStyle` instances for pairs, `Map`\s, `Set`\s,
and functions.

Static attributes
~~~~~~~~~~~~~~~~~

Diagrams can also have "static attributes" which are applied at a
specific node in the tree representing a diagram.  Currently, the only
static attribute is a hyperlink, which is supported only by the SVG
backend.  To turn a diagram into a hyperlink, use the `href`
function.

More static attributes (for example, node IDs and transparency
grouping) and wider backend support will be added in future versions.

2D Transformations
~~~~~~~~~~~~~~~~~~

Any diagram can be transformed by applying arbitrary affine
transformations to it. *Affine* transformations include *linear*
transformations (rotation, scaling, reflection, shears---anything
which leaves the origin fixed and sends lines to lines) as well as
translations.  In the simplified case of the real line, an affine
transformation is any function of the form `f(x) = mx + b`:math:.
Generalizing to `d`:math: dimensions, an affine transformation is a
vector function of the form `f(\mathbf{v}) = \mathbf{M}\mathbf{v} +
\mathbf{B}`:math:, where `\mathbf{M}`:math: and `\mathbf{B}`:math: are
`d \times d`:math: matrices.  More general, non-affine
transformations, including projective transformations, are referred to
in ``diagrams`` as `Deformations`_.

`Diagrams.TwoD.Transform`:mod: defines a number of common affine
transformations in two-dimensional space. (To construct
transformations more directly, see `Diagrams.Core.Transform`:mod:.)

Every transformation comes in two variants, a noun form and a verb
form.  For example, there are two functions for scaling along the
`x`:math:\-axis, `scalingX` and `scaleX`.  The noun form (*e.g.*
`scalingX`) constructs a `Transformation` value, which can then be
stored in a data structure, passed as an argument, combined with other
transformations, *etc.*, and ultimately applied to a diagram (or other
`Transformable` value) with the `transform` function.  The verb form
directly applies the transformation.  The verb form is much more
common (and the documentation below will only discuss verb forms), but
getting one's hands on a first-class `Transformation` value can
occasionally be useful.

.. container:: warning

   Both the verb and noun variants of transformations are monoids, and
   can be composed with `(<>)`. However, the results are quite distinct,
   as shown in this example.

   .. class:: dia-lhs

   ::

   > ell = text "L" <> square 1 # lw none
   > alpha = 45 @@ deg
   >
   > dia1 = ell # translateX 2 # rotate alpha
   > dia2 = ell # ( rotate alpha <> translateX 2 )
   > dia3 = ell # transform ( rotation alpha <> translationX 2 )
   >
   > example =
   >   hcat' (with & sep .~ 2)
   >     [ (dia1 <> orig)
   >     , vrule 4
   >     , (dia2 <> orig)
   >     , vrule 4
   >     , (dia3 <> orig)
   >     ]
   >   where
   >     orig = circle 0.05 # fc red # lw none

   `dia1` is the intended result: a character L translated along the X axis,
   and then rotated 45 degrees around the origin.

   `dia2` shows the result of naively composing the verb versions of
   the transformations: a superposition of a rotated L and a
   translated L.  To understand this, consider that `(rotate alpha)`
   is a *function*, and functions as monoid instances (`Monoid m =>
   Monoid (a -> m)`) are composed as `(f <> g) x = f x <> g x`.  To
   quote the Typeclassopedia_: if `a` is a Monoid, then so is the
   function type `e -> a` for any `e`; in particular, `g \`mappend\`
   h` is the function which applies both `g` and `h` to its argument
   and then combines the results using the underlying Monoid instance
   for `a`.

   Hence `ell # ( rotate alpha <> translateX 2 )` is
   the same as the superposition of two diagrams: `rotate alpha ell <>
   translateX 2 ell`.

   `dia3` shows how the noun versions can be composed (using the
   `Monoid` instance for `Transformation`) with the intended result.

.. _`typeclassopedia`: http://www.haskell.org/haskellwiki/Typeclassopedia#Instances_4

Affine transformations in general
+++++++++++++++++++++++++++++++++

Before looking at specific two-dimensional transformations, it's worth
saying a bit about transformations in general (a fuller treatment can
be found in the `core library reference`_).  The `Transformation`
type is defined in `Diagrams.Core.Transform`:mod:, from the
`diagrams-core`:pkg: package.  `Transformation` is parameterized by
the vector space over which it acts; recall that `T2` is provided as a
synonym for `Transformation R2`.

.. _`core library reference`: core.html

`Transformation v` is a `Monoid` for any vector space `v`:

* `mempty` is the identity transformation;
* `mappend` is composition of transformations: `t1 \`mappend\` t2`
  (also written `t1 <> t2`) performs first `t2`, then `t1`.

To invert a transformation, use `inv`.  For any transformation `t`,

`t <> inv t === inv t <> t === mempty`.

To apply a transformation, use `transform`.

Rotation
++++++++

Use `rotate` to rotate a diagram counterclockwise by a given angle__
about the origin.  Since `rotate` takes an `Angle`, you must specify an
angle unit, such as `rotate (80 @@ deg)`.  In the common case that you
wish to rotate by an angle specified as a certain fraction of a
circle, like `rotate (1/8 @@ turn)`, you can use `rotateBy`
instead. `rotateBy` takes a `Double` argument expressing the number of
turns, so in this example you would only have to write `rotateBy
(1/8)`.

You can also use `rotateAbout` in the case that you want to rotate
about some point other than the origin.

__ `Angles`_

.. class:: dia-lhs

::

> eff = text "F" <> square 1 # lw none
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

> eff = text "F" <> square 1 # lw none
> ts  = [ scale (1/2), id, scale 2,    scaleX 2,    scaleY 2
>       ,                  scale (-1), scaleX (-1), scaleY (-1)
>       ]
>
> example = hcat . map (eff #) $ ts

Scaling by zero is forbidden.  Let us never speak of it again.

For convenience, `reflectX` and `reflectY` perform reflection along
the `x`:math:\- and `y`:math:\-axes, respectively.  Their names can be
confusing (does `reflectX` reflect *along* the `x`:math:\-axis or
*across* the `x`:math:\-axis?) but you can just remember that
`reflectX = scaleX (-1)`, and similarly for `reflectY`; that is,
``reflectQ`` affects ``Q``-coordinates.

To reflect in some line other than an axis, use `reflectAbout`.

.. class:: dia-lhs

::

> eff = text "F" <> square 1 # lw none
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
arguments a function `f` as well as a transformation to conjugate by,
and produces a function which performs the transformation, then `f`,
then the inverse of the transformation.  For example, scaling by a
factor of 2 along the diagonal line `y = x`:math: can be accomplished
thus:

.. class:: dia-lhs

::

> eff = text "F" <> square 1 # lw none
> example = (scaleX 2 `under` rotation (-1/8 @@ turn)) eff

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

.. _`Deformations`:

Deformations
~~~~~~~~~~~~

The affine transformations represented by `Transformation` include the
most commonly used transformations, but occasionally other sorts are
useful.  Non-affine transformations are represented by the
`Deformation` type.  The design is quite similar to that of
`Transformation`.  A `Deformation` is parameterized by the vector space
over which it acts.  There is a `Deformable` type class, and `deform`
applies a `Deformation` to a `Deformable` type in the same vector
space, returning a value of the same type.

`Diagrams.TwoD.Deform`:mod: defines parallel and perspective
projections along the principal axes in 2 dimensions.

.. class:: dia-lhs

::

> sq = unitSquare # translate (5 ^& 3) :: Path R2
> marks = repeat . lw none $ circle 0.02
> dots c p = position $ zip (concat $ pathVertices p) (marks # fc c)
> example = stroke sq <> dots blue sq <> dots green (deform perspectiveX1 sq)

The example above projects a square onto the plane x=1.  In this
example, only the projected vertices are drawn, since the four
overlapping line segments corresponding to the edges of the square are
not interesting.  Note, though, that the `Path` is deformed, and then
the vertices are taken from the projected result.

`Deformation v` is a `Monoid` for any vector space `v`.  New
deformations can be formed by composing two deformations.  The
composition of an affine transformation with a `Deformation` is also a
`Deformation`.  `asDeformation` converts a `Transformation` to an
equivalent `Deformation`, "forgetting" the inverse and other extra
information which distinguishes affine transformations.

The very general nature of deformations prevents certain types
from being `Deformable`.  Because not every `Deformation` is
invertible, diagrams cannot be deformed.  In general, for two points
`p`:math: and `q`:math:, and a deformation `D`:math:, there is no
deformation `D_v`:math: such that, `Dp - Dq = D_v(p-q)`:math:.  For
this reason, only points and concretely located types are deformable.
Finally, segments are not deformable because the image of the segment
may not be representable by a single segment.  The `Deformable`
instances for trails and paths will approximate each segment by
several segments as necessary.  Points, `Located` trails, and paths
are all deformable.

Because approximation and subdivision are required for many
`Deformable` instances, the type class provides a function `deform'`,
which takes the approximation accuracy as its first argument.  For
trails and paths, `deform` (without a prime) calls `deform'` with an
error limit of 0.01 times the object's size.

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
its "boundary".  Here, boundary usually refers to the diagram's
envelope or trace, with envelope being the default (see `Envelopes`_
and `Traces`_ for more information). To this end, some general tools
are provided in `Diagrams.Align`:mod:, and specialized 2D-specific
ones by `Diagrams.TwoD.Align`:mod:.

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

To center an object along an axis we provide the functions `centerX`
and `centerY`. An object can be simultaneously centered along both axis
(actually along all of its basis vectors) using the `center` function.

The align functions have sister functions like `snugL` and `snugX`
that work the same way as `alignL` and `alignX`. The difference is
that the `snug` class of functions use the trace as the boundary
instead of the envelope. For example, here we want to snug a convex
shape (the orange triangle) next to a concave shape (the blue
polygon):

.. class:: dia-lhs

::

> import Diagrams.TwoD.Align
>
> concave = polygon ( with & polyType .~ PolyPolar [a, b, b, b]
>                   [ 0.25,1,1,1,1] & polyOrient .~ NoOrient )
>                   # fc blue # lw none
>   where
>     a = 1/8 @@ turn
>     b = 1/4 @@ turn
>
> convex = polygon (with & polyType .~ PolyPolar [a,b] [0.25, 1, 1]
>                        & polyOrient .~ NoOrient)
>                        # fc orange # lw none
>   where
>     a = 1/8 @@ turn
>     b = 3/4 @@ turn
>
> aligned = (concave # center # alignR # showOrigin)
>        <> (convex # center # alignL # showOrigin)
>
> snugged = (concave # center # snugR # showOrigin)
>        <> (convex # center # snugL # showOrigin)
>
> example = aligned ||| strutX 0.5 ||| snugged

The `snugR` function moves the origin of the blue polygon to the
rightmost edge of its trace in the diagram on the right, whereas in
the left diagram the `alignR` function puts it at the edge of the
envelope.

Trails and paths
----------------

Trails and paths are some of the most fundamental tools in
``diagrams``.  They can be used not only directly to draw things, but
also as guides to help create and position other diagrams.

For additional practice and a more "hands-on" experience learning
about trails and paths, see the `trails and paths tutorial`__.

__ paths.html

Segments
~~~~~~~~

The most basic component of trails and paths is a `Segment`, which is
some sort of primitive path from one point to another.  Segments are
*translationally invariant*; that is, they have no inherent location,
and applying a translation to a segment has no effect (however, other
sorts of transformations, such as rotations and scales, have the
effect you would expect). In other words, a segment is not a way to
get from some particular point A to another point B; it is a way to
get from *wherever you currently happen to be* to *somewhere else*.

Currently, ``diagrams`` supports two types of segment, defined in
`Diagrams.Segment`:mod:\:

* A *linear* segment is simply a straight line, defined by an offset
  from its beginning point to its end point; you can construct one
  using `straight`.

* A *Bézier* segment is a cubic curve defined by an offset from its
  beginning to its end, along with two control points; you can
  construct one using `bezier3` (or `bézier3`, if you are feeling
  snobby).  An example is shown below, with the endpoints shown in red
  and the control points in blue.  `Bézier curves`__ always start off
  from the beginning point heading towards the first control point,
  and end up at the final point heading away from the last control
  point.  That is, in any drawing of a Bézier curve like the one
  below, the curve will be tangent to the two dotted lines.

__ http://en.wikipedia.org/wiki/Bézier_curve

.. class:: dia-lhs

::

> illustrateBézier c1 c2 x2
>     =  endpt
>     <> endpt  # translate x2
>     <> ctrlpt # translate c1
>     <> ctrlpt # translate c2
>     <> l1
>     <> l2
>     <> fromSegments [bézier3 c1 c2 x2]
>   where
>     dashed  = dashingN [0.03,0.03] 0
>     endpt   = circle 0.05 # fc red  # lw none
>     ctrlpt  = circle 0.05 # fc blue # lw none
>     l1      = fromOffsets [c1] # dashed
>     l2      = fromOffsets [x2 ^-^ c2] # translate c2 # dashed
>
> x2      = r2 (3,-1) :: R2         -- endpoint
> [c1,c2] = map r2 [(1,2), (3,0)]   -- control points
>
> example = illustrateBézier c1 c2 x2

Independently of the two types of segments explained above, segments
can be either *closed* or *open*.  A *closed* segment has a fixed
endpoint relative to its start.  An *open* segment, on the other hand,
has an endpoint determined by its context; open segments are used to
implement loops (explained in the `Trails`_ section below).  Most
users should have no need to work with open segments.  (For that
matter, most users will have no need to work directly with segments at
all.)

If you look in the `Diagrams.Segment`:mod: module, you will see quite
a bit of other stuff related to the implementation of trails
(`SegMeasure` and so on); this is explained in more detail in the
section `Trail and path implementation details`_.

Trails
~~~~~~

Trails are defined in `Diagrams.Trail`:mod:.  Informally, you can
think of trails as lists of segments laid end-to-end.  Since segments
are translation-invariant, so are trails.  More formally, the
semantics of a trail is a continuous (though not necessarily
differentiable) function from the real interval `[0,1]`:math: to
vectors in some vector space.  This section serves as a reference on
trails; for a more hands-on introduction, refer to the `Trail and path
tutorial`__.

__ /doc/paths.html

There are two types of trail:

* A *loop*, with a type like `Trail' Loop v`, is a trail which forms
  a "closed loop", ending at the same place where it started.

  .. class:: dia

  ::

  > example = fromOffsets [1 ^& 1, 2 ^& (-1), (-1) ^& (-1), (-3) ^& 1]
  >         # closeLine # strokeLoop # fc blue

  Loops in 2D can be filled, as in the example above.

* A *line*, with a type like `Trail' Line v`, is a trail which does
  not form a closed loop, that is, it starts in one place and ends
  in another.

  .. class:: dia

  ::

  > example = fromOffsets [1 ^& 1, 2 ^& (-1), (-1) ^& (-1), (-3) ^& 1]
  >         # strokeLine

  Actually, a line can in fact happen to end in the same place where
  it starts, but even so it is still not considered closed.  Lines
  have no inside and outside, and are never filled.

  .. container:: warning

    Lines are never filled, even when they happen to start and end in
    the same place!

Finally, the type `Trail` can contain either a line or a loop.

The most important thing to understand about lines, loops, and trails
is how to convert between them.

* To convert from a line or a loop to a trail, use `wrapLine` or
  `wrapLoop` (or `wrapTrail`, if you don't know or care whether the
  parameter is a line or loop).
* To convert from a loop to a line, use `cutLoop`.  This results in a
  line which just so happens to end where it starts.
* To convert from a line to a loop, there are two choices:

  * `closeLine` adds a new linear segment from the end to the start of
    the line.

    .. class:: dia-lhs

    ::

    > almostClosed :: Trail' Line R2
    > almostClosed = fromOffsets $ (map r2
    >   [(2, -1), (-3, -0.5), (-2, 1), (1, 0.5)])
    >
    > example = pad 1.1 . center . fc orange . hcat' (with & sep .~ 1)
    >   $ [ almostClosed # strokeLine
    >     , almostClosed # closeLine # strokeLoop
    >     ]

  * `glueLine` simply modifies the endpoint of the final segment to be
    the start of the line.  This is most often useful if you have a
    line which you know just so happens to end where it starts;
    calling `closeLine` in such a case would result in the addition of
    a gratuitous length-zero segment.

Lines form a monoid under concatenation. For example, below we create
a two-segment line called ``spoke`` and then construct a starburst
path by concatenating a number of rotated copies.  Note how we call
`glueLine` to turn the starburst into a closed loop, so that we can
fill it (lines cannot be filled).  `strokeLoop` turns a loop into a
diagram, with the start of the loop at the local origin. (There are
also analogous functions `strokeLine` and `strokeTrail`.)

.. class:: dia-lhs

::

> spoke :: Trail' Line R2
> spoke = fromOffsets . map r2 $ [(1,3), (1,-3)]
>
> burst :: Trail' Loop R2
> burst = glueLine . mconcat . take 13 . iterate (rotateBy (-1/13)) $ spoke
>
> example = strokeLoop burst # fc yellow # lw thick # lc orange

For convenience, there is also a monoid instance for `Trail` based on
the instance for lines: any loops are first cut with `cutLine`, and
the results concatenated.  Typically this would be used in a situation
where you know that all your trails actually contain lines.

Loops, on the other hand, have no monoid instance.

To construct a line, loop, or trail, you can use one of the following:

* `fromOffsets` takes a list of vectors, and turns each one into a
  linear segment.

  .. class:: dia-lhs

  ::

  > theLine = fromOffsets (iterateN 5 (rotateBy (1/20)) unitX)
  > example = theLine # strokeLine
  >         # lc blue # lw thick # center # pad 1.1

* `fromVertices` takes a list of vertices, generating linear segments
  between them.

  .. class:: dia-lhs

  ::

  > vertices = map p2 $ [(x,y) | x <- [0,0.2 .. 2], y <- [0,1]]
  > example = fromVertices vertices # strokeLine
  >         # lc red # center # pad 1.1

* `(~~)` creates a simple linear trail between two points.
* `cubicSpline` creates a smooth curve passing through a given list of
  points; it is described in more detail in the section on `Splines`_.

  .. class:: dia-lhs

  ::

  > vertices = map p2 . init $ [(x,y) | x <- [0,0.5 .. 2], y <- [0,0.2]]
  > theLine = cubicSpline False vertices
  > example = mconcat (iterateN 6 (rotateBy (-1/6)) theLine)
  >         # glueLine # strokeLoop
  >         # lc green # lw veryThick # fc aqua # center # pad 1.1

* `fromSegments` takes an explicit list of `Segment`\s, which can
  occasionally be useful if, say, you want to generate some Bézier

All the above functions construct loops by first constructing a line
and then calling `glueLine` (see also the below section on
`TrailLike`_).

If you look at the types of these functions, you will note that they
do not, in fact, return just `Trail`\s: they actually return any type
which is an instance of `TrailLike`, which includes lines, loops,
`Trail`\s, `Path`\s (to be covered in an upcoming section), `Diagram`\s,
lists of points, and any of these wrapped in `Located` (see below).
See the `TrailLike`_ section for more on the `TrailLike` class.

For details on other functions provided for manipulating trails, see
the documentation for `Diagrams.Trail`:mod:.  One other function worth
mentioning is `explodeTrail`, which turns each segment in a trail into
its own individual `Path`.  This is useful when you want to construct
a trail but then do different things with its individual segments.
For example, we could construct the same starburst as above but color
the edges individually:

.. class:: dia-lhs

::

> spoke :: Trail R2
> spoke = fromOffsets . map r2 $ [(1,3), (1,-3)]
>
> burst = mconcat . take 13 . iterate (rotateBy (-1/13)) $ spoke
>
> colors = cycle [aqua, orange, deeppink, blueviolet, crimson, darkgreen]
>
> example = lw thick
>         . mconcat
>         . zipWith lc colors
>         . map strokeLocTrail . explodeTrail
>         $ burst `at` origin

(If we wanted to fill the starburst with yellow as before, we would
have to separately draw another copy of the trail with a line width of
zero and fill that; this is left as an exercise for the reader.)

Located
~~~~~~~

Something of type `Located a` consists, essentially, of a value of
type `a` paired with a point.  In this way, `Located` serves to
transform translation-invariant things (such as `Segment`\s or
`Trail`\s) into things with a fixed location.  A `Located Trail` is a
`Trail` where we have picked a concrete location for its starting
point, and so on.

The module `Diagrams.Located`:mod: defines the `Located` type and
utilities for working with it:

* `at` is used to construct `Located` values, and is designed to be
  used infix, like `someTrail \`at\` somePoint`.
* `viewLoc`, `unLoc`, and `loc` can be used to project out the
  components of a `Located` value.
* `mapLoc` can be used to apply a function to the value of type `a`
  inside a value of type `Located a`.  Note that `Located` is not a
  functor, since it is not possible to change the contained type
  arbitrarily: `mapLoc` does not change the location, and the vector
  space associated to the type `a` must therefore remain the same.

Much of the utility of having a concrete type for the `Located`
concept (rather than just passing around values paired with points)
lies in the type class instances we can give to `Located`:

* `HasOrigin`: translating a `Located a` simply translates the
  associated point, leaving the value of type `a` unaffected.
* `Transformable`: only the linear component of transformations are
  applied to the wrapped value (whereas the entire transformation is
  applied to the location).
* `Enveloped`: the envelope of a `Located a` is the envelope of the
  contained `a`, translated to the stored location (and similarly for
  `Traced`).
* The `TrailLike` instance is also useful; see TrailLike_.

Paths
~~~~~

A `Path`, also defined in `Diagrams.Path`:mod:, is a (possibly empty)
collection of `Located Trail`\s. Paths of a single trail can be
constructed using the same functions described in the previous
section: `fromSegments`, `fromOffsets`, `fromVertices`, `(~~)`, and
`cubicSpline`.

`Path`\s also form a `Monoid`\, but the binary operation is
*superposition* (just like that of diagrams).  Paths with
multiple components can be used, for example, to create shapes with
holes:

.. class:: dia-lhs

::

> ring :: Path R2
> ring = circle 3 <> (circle 2 # reversePath)
>
> example = stroke ring # fc purple

See the section on `Fill rules`_ for more information.

`stroke` turns a path into a diagram, just as `strokeTrail` turns a trail
into a diagram. (In fact, `strokeTrail` really works by first turning the
trail into a path and then calling `stroke` on the result.)

`explodePath`, similar to `explodeTrail`, turns the segments of a path
into individual paths.  Since a path is a collection of trails, each
of which is a sequence of segments, `explodePath` actually returns a
list of lists of paths.

For information on other path manipulation functions such as
`pathFromTrail`, `pathFromLocTrail`, `pathVertices`, `pathOffsets`,
`scalePath`, and `reversePath`, see the Haddock documentation in
`Diagrams.Path`:mod:.

Stroking trails and paths
~~~~~~~~~~~~~~~~~~~~~~~~~

The `strokeTrail` and `stroke` functions, which turn trails and paths into
diagrams respectively, have already been mentioned; they are defined
in `Diagrams.TwoD.Path`:mod:.  Both also have primed variants,
`strokeTrail'` and `stroke'`, which take a record of `StrokeOpts`.
Currently, `StrokeOpts` has two fields:

* `vertexNames` takes a list of lists of names, and zips each list
  with a component of the path, creating point subdiagrams (using
  `pointDiagram`) associated with the names.  This means that the
  names can be used to later refer to the locations of the path
  vertices (see `Named subdiagrams`_).  In the case of `strokeTrail'`,
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
``diagrams`` provides `decoratePath`, `decorateLocatedTrail`, and
`decorateTrail`, which position a list of objects at the vertices of a
given path or trail.

For example, suppose we want to create an equilateral triangular
arrangement of dots.  One possibility is to create horizontal rows of
dots, center them, and stack them vertically.  However, this is
annoying, because we must manually compute the proper vertical
stacking distance between rows. Whether you think this sounds easy or
not, it is certainly going to involve the `sqrt` function, or perhaps
some trig, or both, and we'd rather avoid all that.

Fortunately, there's an easier way: after creating the horizontal
rows, we create the path corresponding to the left-hand side of the
triangle (which can be done using `fromOffsets` and a simple
rotation), and then decorate it with the rows.

.. class:: dia-lhs

::

> dot       = circle 1 # fc black
> dotSep    = 0.5
> dotOffset = (width (dot :: D R2) + dotSep) *^ unitX
> mkRow n   = hcat' (with & sep .~ dotSep) (replicate n dot)
> mkTri n   = decoratePath
>               (fromOffsets (replicate (n-1) dotOffset) # rotateBy (-1/3))
>               (map mkRow [1..n])
> example   = mkTri 5

Offsets of segments, trails, and paths
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Given a segment and an offset radius `r`:math: we can make an *offset segment*
that is the distance `r`:math: from the original segment.  More specifically,
you can think of the offset as the curve traced by the end of a vector of
length `r`:math: perpendicular to the original curve.  This vector goes on the
right of the curve for a positive radius and on the left for a negative radius.

.. class:: dia-lhs

::

> import Diagrams.TwoD.Offset
>
> example :: Diagram B R2
> example = hcat' (with & sep .~ 1) $ map f
>         [ straight p
>         , bézier3 (r2 (0,0.5)) (r2 (1,0.5)) p
>         ]
>   where
>     p = r2 (1,1)
>     f :: Segment Closed R2 -> Diagram B R2
>     f s =  fromSegments [s]
>         <> offsetSegment 0.1 0.2 s # strokeLocTrail # lc blue

.. container:: todo

    Animate tracing an offset?

For a straight segment this will clearly be a parallel straight line with
`r`:math: as the distance between the lines.  For an counter-clockwise arc of
radius `R`:math: the offset will be an arc with the same center, start and end
angles, and radius `r+R`:math:.  Cubic segments present a problem, however.
The offset of a cubic Bézier curve could be a higher degree curve.  To
accommodate this we approximate the offset with a sequence of segments.  We
now have enough details to write the type for `offsetSegment`.

.. class:: lhs

::

> offsetSegment :: Double -> Double -> Segment Closed R2 -> Located (Trail R2)

The first parameter to `offsetSegment` is an epsilon factor `\epsilon`:math:.
When the radius is multiplied by `\epsilon`:math: we get the maximum allowed
distance a point on the approximate offset can differ from the true offset.
The final parameters are the radius and the segment.  The result is a located
trail.  It is located because the offset's start will be distance `r`:math:
away from the segment start which is the origin.

If we can offset a segment we naturally will want to extend this to offset a
trail.  A first approach might be to simply map `offsetSegment` over the
segments of a trail.  But we quickly notice that if the trail has any sharp
corners, the offset will be disconnected!

.. class:: dia-lhs

::

> import Diagrams.TwoD.Offset
>
> locatedTrailSegments t = zipWith at (trailSegments (unLoc t)) (trailVertices t)
>
> bindLoc f = join' . mapLoc f
>   where
>     join' x = let (p,a) = viewLoc x in translate (p .-. origin) a
>
> offsetTrailNaive :: Double -> Double -> Trail R2 -> Path R2
> offsetTrailNaive e r = mconcat . map (pathFromLocTrail . bindLoc (offsetSegment e r))
>                      . locatedTrailSegments . (`at` origin)
>
> example :: Diagram B R2
> example = (p # strokeTrail <> offsetTrailNaive 0.1 0.3 p # stroke # lc blue)
>         # lw thick
>   where p = fromVertices . map p2 $ [(0,0), (1,0.3), (2,0), (2.2,0.3)]

First let's consider the outside corner where the adjacent offset segments do
not cross.  If we consider sweeping a perpendicular vector along the original
trail we have a problem when we get to a corner.  It is not clear what
*perpendicular* means for that point.  One solution is to take all points
distance `r`:math: from the corner point.  This puts a circle around the corner
of radius `r`:math:.  Better is to just take the portion of that circle that
transitions from what is perpendicular at the end of the first segment to what
is perpendicular at the start of the next.  We could also choose to join together
offset segments in other sensible ways.  For the choice of join we have the
`_offsetJoin` field in the `OffsetOpts` record.

.. class:: dia-lhs

::

> import Diagrams.TwoD.Offset
>
> example :: Diagram B R2
> example = (p # strokeTrail <> o # strokeLocTrail # lc blue)
>         # lw thick
>   where
>     p = fromVertices . map p2 $ [(0,0), (1,0.3), (2,0), (2.2,0.3)]
>     o = offsetTrail' (with & offsetJoin .~ LineJoinRound) 0.3 p

Inside corners are handled in a way that is consistent with outside corners, but
this yields a result that is most likely undesirable.  Future versions of Diagrams
will include the ability to clip inside corners with several options for how to
do the clipping.

.. container:: todo

    Update after implementing clipping.

There are other interesting ways we can join segments.  We implement the standard
line join styles and will also in the future provide the ability to specify a custom
join.

.. class:: dia-lhs

::

> import Diagrams.TwoD.Offset
>
> example :: Diagram B R2
> example = hcat' (with & sep .~ 0.5) $ map f [LineJoinMiter, LineJoinRound, LineJoinBevel]
>   where
>     f s = p # strokeTrail <> (offsetTrail' (with & offsetJoin .~ s) 0.3 p # strokeLocTrail # lc blue)
>     p = fromVertices . map p2 $ [(0,0), (1,0), (0.5,0.7)]

The `LineJoinMiter` style in particular can use more information to dictate how
long a miter join can extend.  A sharp corner can have a miter join that is an
unbounded distance from the original corner.  Usually, however, this long join
is not desired.  Diagrams follows the practice of most graphics software and
provides a `_offsetMiterLimit` field in the `OffsetOpts` record.  When the join
would be beyond the miter limit, the join is instead done with a straight line
as in the `LineJoinBevel` style.  The `OffsetOpts` record then has three
parameters:

.. class:: lhs

::

> data OffsetOpts = OffsetOpts
>     { _offsetJoin       :: LineJoin
>     , _offsetMiterLimit :: Double
>     , _offsetEpsilon    :: Double
>     }

And the type for `offsetTrail'` is (`offsetTrail` simply uses the `Default`
instance for `OffsetOpts`):

.. class:: lhs

::

> offsetTrail  ::               Double -> Located (Trail R2) -> Located (Trail R2)
> offsetTrail' :: OffsetOpts -> Double -> Located (Trail R2) -> Located (Trail R2)
>
> offsetPath  ::               Double -> Path R2 -> Path R2
> offsetPath' :: OffsetOpts -> Double -> Path R2 -> Path R2

Notice this takes a `Trail R2` which means it works for both `Trail' Line R2`
and `Trail' Loop R2`.  The second parameter is the radius for the offset.  A
negative radius gives a `Line` on the right of the curve, or a `Loop` inside a
counter-clockwise `Loop`.  For `offsetPath` we can simply map `offsetTrail`
over the trails in the path in the most natural way.

Expand segments, trails, and paths
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Expanding is just like the offset, but instead of producing a curve that
follows one side we follow both sides and produce a `Loop` that can be filled
representing all the area within a radius `r`:math: of the original curve.

In addition to specifying how segments are joined, we now have to specify the
transition from the offset on one side of a curve to the other side of a curve.
This is given by the `LineCap`.

.. class:: lhs

::

> data ExpandOpts = ExpandOpts
>     { _expandJoin       :: LineJoin
>     , _expandMiterLimit :: Double
>     , _expandCap        :: LineCap
>     , _expandEpsilon    :: Double
>     }
>
> expandTrail  ::               Double -> Located (Trail R2) -> Path R2
> expandTrail' :: ExpandOpts -> Double -> Located (Trail R2) -> Path R2
>
> expandPath  ::               Double -> Path R2 -> Path R2
> expandPath' :: ExpandOpts -> Double -> Path R2 -> Path R2

The functionality follows closely to the offset functions, but notice that
the result of `expandTrail` is a `Path R2` where `offsetTrail` resulted in
a `Located (Trail R2)`.  This is because an expanded `Loop` will be a pair
of loops, one inside and one outside.  To express this we need a `Path`.

.. class:: dia-lhs

::

> import Diagrams.TwoD.Offset
>
> example :: Diagram B R2
> example = (p # strokeTrail # lw veryThick # lc white <> e # stroke # lw none # fc blue)
>   where
>     p = fromVertices . map p2 $ [(0,0), (1,0.3), (2,0), (2.2,0.3)]
>     e = expandTrail' opts 0.3 p
>     opts = with & expandJoin .~ LineJoinRound
>                 & expandCap  .~ LineCapRound

As long as the expanded path is filled with the winding fill rule we
do not need to worry about having clipping for inside corners.  It
works out that the extra loop in the rounded line join will match with
the outside corner.  We currently implement all the `LineCap` styles,
and plan to support custom styles in future releases.

.. class:: dia-lhs

::

> import Diagrams.TwoD.Offset
>
> example :: Diagram B R2
> example = hcat' (with & sep .~ 0.5) $ map f [LineCapButt, LineCapRound, LineCapSquare]
>   where
>     f s =  p # strokeTrail # lw veryThick # lc white
>         <> expandTrail' (opts s) 0.3 p # stroke # lw none # fc blue
>     p = fromVertices . map p2 $ [(0,0), (1,0), (0.5,0.7)]
>     opts s = with & expandJoin .~ LineJoinRound
>                   & expandCap  .~ s

.. _TrailLike:

The ``TrailLike`` class
~~~~~~~~~~~~~~~~~~~~~~~

As you may have noticed by now, a large class of functions in the
standard library---such as `square`, `polygon`, `fromVertices`, and so
on---generate not just diagrams, but *any* type which is an instance
of the `TrailLike` type class.

The `TrailLike` type class, defined in `Diagrams.TrailLike`:mod:, has
only a single method, `trailLike`:

.. class:: lhs

::

> trailLike :: Located (Trail (V t)) -> t

That is, a trail-like thing is anything which can be constructed from
a `Located Trail`.

There are quite a few instances of `TrailLike`:

* `Trail`: this instance simply throws away the location.
* `Trail' Line`: throw away the location, and perform `cutLoop` if
  necessary.  For example, `circle 3 :: Trail' Line R2` is an open `360^\circ`:math:
  circular arc.
* `Trail' Loop`: throw away the location, and perform `glueLine` if
  necessary.
* `Path`: construct a path with a single component.
* `Diagram b R2`: as long as the backend `b` knows how to render 2D
  paths, `trailLike` can construct a diagram by stroking the generated
  single-component path.
* `[Point v]`: this instance generates the vertices of the trail.
* `Located (Trail v)`, of course, has an instance which amounts to the
  identity function.  More generally, however, `Located a` is an
  instance of `TrailLike` for *any* type `a` which is also an
  instance.  In particular, the resulting `Located a` has the location
  of the input `Located Trail`, and a value of type `a` generated by
  another call to `trailLike`.  This is most useful for generating
  values of type `Located (Trail' Line v)` and `Located (Trail' Loop
  v)`.  For example, `circle 3 # translateX 2 :: Located (Trail' Line
  R2)` is an open `360^\circ`:math: circular arc centered at
  `(2,0)`:math:.

It is quite convenient to be able to use, say, `square 2` as a
diagram, path, trail, list of vertices, *etc.*, whichever suits one's
needs.  Otherwise, either a long list of functions would be needed for
each primitive (like ``square``, ``squarePath``, ``squareTrail``,
``squareVertices``, ``squareLine``, ``squareLocatedLine``, ... ugh!),
or else explicit conversion functions would have to be inserted when
you wanted something other than what the `square` function gave you by
default.

As an (admittedly contrived) example, the following diagram defines
`s` as an alias for `square 2` and then uses it at four different
instances of `TrailLike`:

.. class:: dia-lhs

::

> s = square 2  -- a squarish thingy.
>
> blueSquares = decoratePath s {- 1 -}
>                 (replicate 4 (s {- 2 -} # scale 0.5) # fc blue)
> paths       = lc purple . stroke $ star (StarSkip 2) s {- 3 -}
> aster       = center . lc green . strokeLine
>             . mconcat . take 5 . iterate (rotateBy (1/5))
>             . onLineSegments init
>             $ s {- 4 -}
> example = (blueSquares <> aster <> paths)

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

> ts = mconcat . iterateN 3 (rotateBy (1/9)) $ triangle 1
> example = (ts ||| stroke ts ||| strokeLine ts ||| fromVertices ts) # fc red

The above example defines `ts` by generating three equilateral
triangles offset by 1/9 rotations, then combining them with `mconcat`.
The sneaky thing about this is that `ts` can have the type of any
`TrailLike` instance, and it has completely different meanings
depending on which type is chosen.  The example uses `ts` at each of
four different monoidal `TrailLike` types:

* Since `example` is a diagram, the first `ts`, used by itself, is
  also a diagram; hence it is interpreted as three equilateral
  triangle diagrams superimposed on one another with `atop`.

* `stroke` turns `Path`\s into diagrams, so the second `ts` has type
  `Path R2`.  Hence it is interpreted as three closed triangular paths
  superimposed into one three-component path, which is then stroked.

* `strokeLine` turns `Trail' Line`\s into diagrams, so the third
  occurrence of `ts` has type `Trail' Line R2`.  It is thus
  interpreted as three open triangular trails sequenced end-to-end
  into one long open trail.  As a line (*i.e.* an open trail), it is
  not filled (in order to make it filled we could replace `strokeLine
  ts` with `strokeLoop (glueLine ts)`).

* The last occurrence of `ts` is a list of points, namely, the
  concatenation of the vertices of the three triangles.  Turning this
  into a diagram with `fromVertices` generates a single-component,
  open trail that visits each of the points in turn.

Of course, one way to avoid all this would be to give `ts` a specific
type signature, if you know which type you would like it to be.  Then
using it at a different type will result in a type error, rather than
confusing semantics.

Answers to the `square 2` type inference challenge:

#. `Path R2`
#. `Diagram b R2`
#. `[P2]`
#. `Trail' Line R2`

Segments and trails as parametric objects
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Both segments and trails, semantically, can be seen as *parametric
functions*: that is, for each value of a parameter within some given
range (usually `[0,1]`:math:, there is a corresponding vector value
(or point, for `Located` segments and trails).  The entire collection
of such vectors or points makes up the segment or trail.

The `Diagrams.Parametric`:mod: module provides tools for working with
segments and trails as parametric functions.

Parametric
++++++++++

As explained above, parametric objects can be viewed semantically as
functions.  In particular, parametric objects of type `p` can be seen
as functions of type `Scalar (V p) -> Codomain p`, where the type
family `Codomain` is defined in such a way as to make this true.  For
example, `Codomain (Trail R2) ~ R2`, because a trail can be thought of
as a function `Double -> R2`.

The `Parametric` class defines the single method `atParam` which
yields this parametric view of an object:

.. class:: lhs

::

> atParam :: Parametric p => p -> Scalar (V p) -> Codomain p

(Note that it is not possible to convert in the other
direction---every function of type `Scalar (V p) -> Codomain p` need
not correspond to something of type `p`.  For example, to convert from
a function to a trail one would need at the very least a guarantee of
continuity; segments are even more restricted.)

.. class:: dia-lhs

::

> spline :: Located (Trail R2)
> spline = cubicSpline False [origin, 0 ^& 1, 1 ^& 1, 1 ^& 0] # scale 3
> pts = map (spline `atParam`) [0, 0.1 .. 1]
> dot = circle 0.2 # fc blue
>
> example = mconcat (map (place dot) pts) <> strokeLocTrail spline

Instances of `Parametric` include:

* `Segment Closed`: The codomain is the type of vectors.  Note there
  is no instance for `Segment Open`, since additional context is
  needed to determine the endpoint, and hence the parametrization, of
  an open segment.
* `FixedSegment`: The codomain is the type of points.
* `Trail'`: The codomain is the vector space. Note that there is no
  difference between `Line` and `Loop`.
* `Trail`: same as the instance for `Trail'`.
* `Located a`: as long as `a` is also `Parametric` and the codomain of
  `a` is a vector space, `Located a` is parametric with points as the
  codomain.  For example, calling `atParam` on a `Located (Trail R2)`
  returns a `P2`.

`Path`\s are *not* `Parametric`, since they may have multiple trail
components and there is no canonical way to assign them a
parametrization.

DomainBounds
++++++++++++

The `domainLower` and `domainUpper` functions simply return the lower
and upper bounds for the parameter.  By default, these will be `0`:math: and
`1`:math:, respectively.  However, it is possible to have objects
parameterized over some interval other than `[0,1]`:math:.

EndValues
+++++++++

The `EndValues` class provides the functions `atStart` and `atEnd`,
which return the value at the start and end of the parameter interval,
respectively.  In other words, semantically we have `atStart x = x
\`atParam\` domainLower x`, but certain types may have more efficient
or accurate ways of computing their start and end values (for example,
Bézier segments explicitly store their endpoints, so there is no need
to evaluate the generic parametric form).

Sectionable
+++++++++++

The `Sectionable` class abstracts over parametric things which can be
split into multiple sections (for example, a trail can be split into
two trails laid end-to-end).  It provides three methods:

* `splitAtParam :: p -> Scalar (V p) -> (p, p)` splits something of
  type `p` at the given parameter into two things of type `p`.
  The resulting values will be linearly reparameterized to cover the
  same parameter space as the parent value.  For example, a segment
  with parameter values in `[0,1]`:math: will be split into two
  shorter segments which are also parameterized over `[0,1]`:math:.
* `section :: p -> Scalar (V p) -> Scalar (V p) -> p` extracts the
  subpart of the original lying between the given parameters, linearly
  reparameterized to the same domain as the original.
* `reverseDomain :: p -> p` reverses the parameterization.  It
  probably should not be in this class and is likely to move elsewhere
  in future versions.

HasArcLength
++++++++++++

`HasArcLength` abstracts over parametric things with a notion of arc
length.  It provides five methods:

* `arcLengthBounded` approximates the arc length of an object to
  within a given tolerance, returning an interval which is guaranteed
  to contain the true arc length.
* `arcLength` is similar to `arcLengthBounded`, but returns a single
  length value instead of an interval.
* `stdArcLength` approximates the arc length up to a standard
  accuracy of `\pm 10^{-6}`:math:.

* `arcLengthToParam` converts an arc length to a parameter, up to a
  given tolernace
* `stdArcLengthToParam` is like `arcLengthToParam`, but using a
  standard accuracy of `\pm 10^{-6}`:math:.

Adjusting length
++++++++++++++++

Anything which is an instance of `DomainBounds`, `Sectionable`, and
`HasArcLength` can be "adjusted" using the `adjust` function, which
provides a number of options for changing the length and extent.

Computing tangents and normals
++++++++++++++++++++++++++++++

The `Diagrams.Tangent`:mod: module contains functions for computing
tangent vectors and normal vectors to segments and trails, at an
arbitrary parametmer (`tangentAtParam`, `normalAtParam`) or at the
start or end (`tangentAtStart`, `tangentAtEnd`, `normalAtStart`,
`normalAtEnd`). (The start/end functions are provided because such
tangent and normal vectors may often be computed more quickly and
precisely than using the general formula with a parameter of 0 or 1.)

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
> dot = circle 0.2 # fc blue # lw none
> mkPath closed = position (zip pts (repeat dot))
>              <> cubicSpline closed pts
> example = mkPath False ||| strutX 2 ||| mkPath True

For more precise control over the generation of curved paths, see the
`Diagrams.TwoD.Path.Metafont`:mod: module from
`diagrams-contrib`:pkg:, which also has `its own tutorial`__.

__ metafont.html

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
  it results in a pattern of alternating filled and unfilled
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

You should be aware that queries (see `Using queries`_) use the
winding rule by default, and are not affected by the path fill rule
attribute.  Thus, if you apply the even-odd rule to a diagram, the
query may not match in the way you expect.  For this reason, if you
want to make a shape with holes in it, it is usually better to form
the holes from paths winding in the opposite direction (using
`reversePath` and the winding rule) than from the even-odd rule.  For
example, in the diagram below, the annulus on the left is formed using
the even-odd fill rule, and the one on the right with the default
winding rule and a reversed inner circle.  The dark blue points
indicate places where the associated query evaluates to true.

.. class:: dia-lhs

::

> points = [x ^& 0 | x <- [-2.3, -2.1 .. 2.3]]
> dia1 = (circle 2 <> circle 1) # stroke # fillRule EvenOdd
> dia2 = (circle 2 <> reversePath (circle 1)) # stroke
>
> illustrate d = ((d # fc grey) `beneath`) . mconcat . map drawPt $ points
>   where
>     drawPt p | getAny (sample d p) = circle 0.1 # fc blue # moveTo p
>              | otherwise           = circle 0.07 # fc lightblue # moveTo p
>
> example = illustrate dia1 ||| strutX 1 ||| illustrate dia2

If you do want to make a diagram whose query uses the even-odd rule,
you can use the `stroke'` function.

Clipping
~~~~~~~~

With backends that support clipping, paths can be used to *clip* other
diagrams.  Only the portion of a clipped diagram falling inside the
clipping path will be drawn.

.. class:: dia-lhs

::

> example = square 3
>         # fc green
>         # lw veryThick
>         # clipBy (square 3.2 # rotateBy (1/10))

Several functions are available, depending on what envelope and trace
you want the resulting diagram to have.  `clipBy` uses the envelope
and trace of the original diagram.  `clipped` uses the envelope and
trace of the clipping path.  `clipTo` uses the intersection of the two
envelopes, and a trace which matches the displayed outline of the
diagram.  Note that in general the intersection of envelopes is larger
than the envelope of an intersection.  Diagrams does not have a
function which returns the tight envelope of the intersection.

Altering a diagram's envelope can also be accomplished using `withEnvelope`
(see `Envelope-related functions`_).  The `view` function is also
provided for the special case of setting a diagram's envelope to some
rectangle, often used for the purpose of selecting only a part of a
diagram to be "viewed" in the final output.  It takes a point---the
lower-left corner of the viewing rectangle---and the vector from the
lower-left to upper-right corner.

.. class:: dia-lhs

::

> circles = (c ||| c) === (c ||| c) where c = circle 1 # fc fuchsia
> example = circles # center # view (p2 (-1,-1)) (r2 (1.3, 0.7))

Note in the above example how the actual portion of the diagram that
ends up being visible is larger than the specification given to
`view`---this is because the aspect ratio of the requested output
image does not match the aspect ratio of the rectangle given to
`view` (and also because of the use of `pad` by the framework which
renders the user manual examples).  If the aspect ratios matched the
viewed portion would be exactly that specified in the call to `view`.

Trail and path implementation details
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Trails are implemented using `finger trees`_: in particular, lines are
finger trees of closed segments, while loops consist of a finger tree
of closed segments plus a single final open segment.

.. _`finger trees`: http://apfelmus.nfshost.com/articles/monoid-fingertree.html

The benefit of using a finger tree (instead of just, say, a list, or
even a `Seq` structure from `Data.Sequence`:mod:) is that it allows
caching monoidal "measures" of the entire trail.  In particular, the
finger trees underlying trails cache

* the number of segments
* the total arc length (up to a standard error tolerance)
* the total offset (vector from start to end)
* the envelope

For more details, see the `Diagrams.Segment`:mod: and
`Diagrams.Trail`:mod: modules.

Another interesting aspect of the implementation is that upon stroking
a path to form a diagram, instead of simply putting the entire path
into a primitive, we separate out the lines and loops into two path
primitives.  This is helpful for backends because they often have to
do some active work to *avoid* filling lines, and if
`diagrams-lib`:pkg: did not do this separation, they would essentially
have to end up doing it themselves.

Arrows
------

`Diagrams.TwoD.Arrow`:mod: and `Diagrams.TwoD.Arrowheads`:mod: provide
specialized functionality for drawing arrows. Note that arrows are
drawn with scale-invariant heads and tails (see `Scale-invariance`_).
Arrows can be used to connect various things including literal points,
named subdiagrams, or their traces. For more detailed information,
examples, and exercises, see the `Arrows tutorial`__.

__ arrow.html

To create arrows, one may use the functions:

* `arrowBetween` to connect points;

* `connect` to connect diagrams;

* `connectOutside` to connect points on the boundary (trace) of
  diagrams (for an example, see the `symmetry cube`__ example in the
  gallery);

__ /gallery/SymmetryCube.html

* `connectPerim` to connect points on the traces of diagrams at
  particular external angles;

* `arrowAt` to place an arrow at a point.

* `arrowV` to create an arrow with the magnitude and direction of a given
  vector.

.. class:: dia-lhs

::

> sPt = 0.50 ^& 0.50
> ePt = 5.2 ^& 0.50
>
> -- Connect two points.
> ex1 = arrowBetween sPt ePt
>
> d = octagon 1 # lc blue # lw ultraThick # showOrigin
> ds = d # named "1" ||| strut 3 ||| d # named "2"
>
> -- Connect two diagrams and two points on their trails.
> ex23 = ds # connect "1" "2"
>           # connectPerim "1" "2" (15/16 @@ turn) (9/16 @@ turn)
>
> -- Place an arrow at (0,0) the size and direction of (0,1).
> ex4 = arrowAt (0 ^& 0) unit_Y
>
> example = (ex1
>           ===
>           strutY 0.5
>           ===
>           (ex23 <> ex4)) # center

Notice that the arrows in the above diagram all have the same dart
shaped head, no tail, and a straight shaft. All of these aspects, and
many others, can be customized using companion functions to the ones
above, whose names end with an apostrophe.  For example, the companion
to `connect` is `connect'`. These companion functions take an extra
`ArrowOpts` record, whose fields are:

* `arrowHead` and `arrowTail`, to specify the shape of the head and
  tail. The `Diagrams.TwoD.Arrowheads`:mod: module exports the
  arrowheads `tri`, `dart`, `spike`, `thorn`, `lineHead`,
  and `noHead`;
  the default is `dart`. For tails we have `quill`, `block`, `lineTail`, and
  `noTail`; `noTail` is the default. Addtionally, any head can be used
  as a tail by appending a `'` (e.g. `dart'`). There are also
  functions that can be used to create custom heads and tails (see
  `Diagrams.TwoD.Arrow`:mod:).

* `arrowShaft` is any `Trail R2`; it will be sized automatically to
  fit between the endpoints of the arrow.

* `headLength` and `tailLength` specify the size of the head and tail,
  defined as the length of the head or tail plus the joint connecting
  it to the shaft. Their value is of 
  type `Measure R2` (see  `Measurement units`_). The
  default value is `normal` which is a synonym for `Normalized 0.035`.
  A traversal called `lengths` sets both the `headLength` and `tailLength`
  at the same time. 

* `headGap` and `tailGap` both default to `none` and are used to indicate
  the amount of space between the end of the arrow and the location it
  is pointing at. They are also of type `Measure R2`.  
  A traversal called `gaps` is provided to set
  both the `headGap` and `tailGap` simultaneously.

* `headStyle`, `tailStyle` and `shaftStyle` are used to pass in style
  functions like `fc blue . opacity 0.75` to customize parts of the
  arrow. (By default, the entire
  arrow, including head and tail, is drawn using the current line
  texture.)

The Lenses `headTexture`, `tailTexture`, and `shaftTexture` are provided
for conveniently setting the texture of a head or tail. Addtionally, the
function `solid` converts a color to a texture. For example, 
`(with & headTexture .~ solid blue)` will set the head color to blue.

The following example demonstrates the use of various `ArrowOpts`.
See `Named subdiagrams`_ for the use of names and the `named`
function.

.. class:: dia-lhs

::

> c = circle 2 # fc lightgray # lw none # showOrigin
>
> x |-| y = x ||| strutX 3 ||| y
>
> row1 = (c # named "1") |-| (c # named "3")
>    |-| (c # named "5") |-| (c # named "7")
> row2 = (c # named "2") |-| (c # named "4")
>    |-| (c # named "6") |-| (c # named "8")
>
> d = row1 === strutY 5 === row2
>
> shaft1 = trailFromVertices (map p2 [(0, 0), (1, 0), (1, 0.2), (2, 0.2)])
> shaft2 = cubicSpline False (map p2 [(0, 0), (1, 0), (1, 0.2), (2, 0.2)])
> shaft3 = arc (0 @@ turn) (1/6 @@ turn)
>
> example = d
>    # connect' (with & arrowTail .~ quill & lengths .~ large
>                     & tailTexture .~ solid orange & headTexture .~ solid orange
>                     & arrowHead .~ spike 
>                     & shaftStyle %~ lw veryThick ) "1" "2"
>    # connect' (with & arrowTail .~ thorn' & lengths .~ large
>                     & arrowHead .~ thorn 
>                     & arrowShaft .~ shaft1 & shaftStyle %~ lw veryThick ) "3" "4"
>    # connect' (with & arrowTail .~ block & gaps .~ small
>                     & arrowHead .~ dart & headLength .~ large
>                     & arrowShaft .~ shaft2
>                     & headStyle %~ fc blue & tailStyle %~ fc blue
>                     & shaftStyle %~ lw veryThick . lc blue ) "5" "6"
>    # connect' (with & arrowShaft .~ shaft3
>                     & arrowHead .~ tri & headLength .~ large
>                     & headStyle %~ fc red . opacity 0.5 
>                     & shaftStyle %~ lw veryThick . lc black . opacity 0.5 ) "7" "8"

Text
----

.. container:: warning

  Note: this section describes the basic text support built into
  ``diagrams``, which suffices for simple applications.  However, for
  much nicer text support (with a tradeoff in complexity/efficiency),
  see the `SVGFonts`:pkg: package, described in the section `Native
  font support`_ below.

Text objects, defined in `Diagrams.TwoD.Text`:mod:, can be created
most simply with the `text` function, which turns a `String` into a
diagram with (centered) text:

.. class:: dia-lhs

::

> example = text "Hello world!" <> rect 8 1

Text with different alignments can be created using `topLeftText` or
`baselineText` (or, more generally, `alignedText`, though it is not
supported by all backends---the SVG backend in particular only
supports an approximation to `alignedText`):

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
objects is that they *take up no space*: they have a *point envelope*
at the origin, *i.e.* for the purposes of things like `beside`, they
have a width and height of zero. (Note, however, this is not the same
as having an *empty* envelope.  In particular, they still behave in an
intuitive manner when included as arguments to things like `hcat`.)
If we omitted the rectangle from the above example, there would be no
output.

.. container:: warning

   Text objects take up no space!

The main reason for this is that computing the size of some text in a
given font is rather complicated, and ``diagrams`` cannot (yet) do it
natively.  The cairo backend can do it (see below) but we don't want
to tie diagrams to a particular backend.

Note, however, that the cairo backend includes a module
`Diagrams.Backend.Cairo.Text`:mod: with functions for querying font
and text extents, and creating text diagrams that take up an
appropriate amount of space.  So it *is* possible to have
automatically-sized text objects, at the cost of being tied to the
cairo backend and bringing `IO` into the picture (or being at peace
with some probably-justified uses of `unsafePerformIO`).

Various attributes of text can be set using `font`, `bold` (or, more
generally, `fontWeight`), `italic`, and `oblique` (or, more generally,
`fontSlant`).  Text is colored with the current fill color (see
`Color`_).

.. class:: dia-lhs

::

> text' s t = text t # fontSize (Local s) <> strutY (s * 1.3)
> example = center $
>       text' 10 "Hello" # italic
>   === text' 5 "there"  # bold # font "freeserif"
>   === text' 3 "world"  # fc green

Font size
~~~~~~~~~

Font size is set using the `fontSize` function, and is specified by a
value of type `Measure R2` (see `Measurement units`_).

* Text with a `Local` font size is measured relative to its local
  vector space.  Such text is transformed normally by any
  transformations applied to it.  For example, in the diagram below,
  `fontSize (Local 1)` is specified (this is actually the default, so
  it could be omitted without changing the diagram). Note how the F's
  are the same size as a unit box, and scale, stretch, and rotate
  along with it.

  .. class:: dia-lhs

  ::

  > eff = text "F" <> square 1
  >
  > example = hcat
  >   [eff, eff # scale 2, eff # scaleX 2, eff # scaleY 2, eff # rotateBy (1/12)]
  >         # fontSize (Local 1)

* Text whose font size is specified in any measurement other than
  `Local` (that is, `Normalized`, `Global`, or `Output`) behaves
  differently.

  .. class:: dia-lhs

  ::

  > eff = text "F" <> square 1
  >
  > example = hcat
  >   [eff, eff # scale 2, eff # scaleX 2, eff # scaleY 2, eff # rotateBy (1/12)]
  >         # fontSize (Normalized 0.1)

  There are several things to notice about the above example
  diagram, which is identical to the previous one except for the
  fact that `Normalized 0.1` is used instead of `Local 1`:

  * The F's are 1/10th the size of the overall diagram.  If
    we added more copies of `eff` to the right, but kept the
    physical size of the rendered image the same, the F's would
    remain the same physical size on the screen, but would get
    bigger relative to the boxes (since the boxes would be smaller
    in absolute terms).

  * The F's are all about the same size---in particular, the uniform
    scaling applied to the second F has no effect, and the fourth F is
    not twice as tall as the others.  Note, however, that the final F
    rotates with the square as expected.  Note also that the third and
    fourth F's are squished, as one would expect from a non-uniform
    scaling.  The hand-wavy slogan is that non-`Local`-sized text is
    "affected by transformations, but without changing size".

    The technical specification is that applying a transformation
    `T`:math: to non-`Local`-sized text actually results in applying
    the transformation `T/|T|`:math:, where `|T|`:math: denotes the
    *average scaling factor* of the transformation `T`:math:, computed
    as the square root of the positive determinant of `T`:math:.  This
    behaves nicely: for example, the average scaling factor of `scale
    k` is `k`, so applying a uniform scaling to non-`Local`-sized text
    has no effect; it is also compositional, so applying `t` and then
    `s` to some text has exactly the same effect as applying `s <> t`.
    For more information, see the `avgScale` function and the comments
    associated with its source code.

Native font support
~~~~~~~~~~~~~~~~~~~

The `SVGFonts package`_ implements native text support for diagrams,
using fonts in the SVG format (note that it can be used with *any*
backend, not just the SVG backend). Among other things, it provides
its own `textSVG` function which can be used to convert text into a
*path* tracing the outline of the text.  Here is a simple example:

.. _`SVGFonts package`: http://hackage.haskell.org/package/SVGFonts

.. class:: dia-lhs

::

> text' d s = (stroke $ textSVG' (TextOpts s lin2 INSIDE_H KERN False d d))
>           # lw none
>
> example = text' 5 "Hello" # fc blue ||| text' 3 "world" # fc green

For more details and examples, see the `Haddock documentation`__.

__ http://hackage.haskell.org/package/SVGFonts

Images
------

The `Diagrams.TwoD.Image`:mod: module provides basic support for
including both external and embedded images in diagrams. Backend
support for images is currently somewhat limited, only the cairo
backend supports external images and only the rasterific backend
supports embedded images.

To create an embedded image use either `loadImageEmb` to read an
image from a file path using `JuicyPixels`:pkg: and return a
`DImage Embedded`. Then use `image` to convert the `DImage Embedded`
to a diagram. You can also create an a diagram with an embedded image
by supplying a function that maps pixel coordinates to `alphaColour` s
plus a width and a height to the`rasterDia` function.

The function `loadImageExt` checks to make sure the file exists, uses
`JuicyPixels`:pkg: to determine its size and returns a reference to
the image. On the otherhand `uncheckedImageRef` simply packages the
reference with a width and height to make a `DImage External`.

.. class:: dia-lhs

::

> no = (circle 1 <> hrule 2 # rotateBy (1/8))
>    # lwO 20 # lc red # frame 0.2
> example = do
>   res <- loadImageExt "doc/static/phone.png"
>   return $ case res of
>     Left err    -> mempty
>     Right phone -> no <> image phone # sized (Dims 1.5 1.5)

When using `loadImageEmb` and `loadImageExt` you do not need to
provide the width and height of the image as they will be calculated
by `JuicyPixels`:pkg:. Otherwise you must specify both a width and
a height for each image.  In this case you might hope to be able to
specify just a width or just a height, and have the other dimension
computed so as to preserve the image's aspect ratio.  However, there
is no way for ``diagrams`` to query an image's aspect ratio until
rendering time, but (until such time as a constraint solver is added)
it needs to know the size of the image when composing it with other
subdiagrams.  Hence, both dimensions must be specified, and for the
purposes of positioning relative to other diagrams, the image will
be assumed to occupy a rectangle of the given dimensions.

However, note that the image's aspect ratio will be preserved: if you
specify dimensions that do not match the actual aspect ratio of the
image, blank space will be left in one of the two dimensions to
compensate.  If you wish to alter an image's aspect ratio, you can do
so by scaling nonuniformly with `scaleX`, `scaleY`, or something
similar.

Currently, the cairo backend can only include images in ``png``
format, but hopefully this will be expanded in the future. The rasterific
backend will render many different image formats including, ``png``, ``jpg``,
``tif``, ``bmp`` and ``gif``.

Advanced tools for diagram creation
===================================

This section covers some of the more advanced tools provided by the
core and standard libraries for constructing diagrams.  Most of the
content in this section is applicable to diagrams in any vector space,
although 2D diagrams are used as illustrations.

Envelopes
---------

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

  > surround d = c === (c ||| d ||| c) # center === c
  >   where c = circle 0.5
  >
  > example = surround (square 1) ||| strutX 1
  >       ||| surround (square 1 # pad 1.2)

  However, the behavior of `pad` often trips up first-time users of
  ``diagrams``:

  .. container:: warning

     `pad` expands the envelope *relative to the local
     origin*.  So if you want the padding to be equal on all sides, use
     `center` first or use `frame` as described next.

* `frame` increases the envelope in all directions by a given amount measued
  in local coordinates.

  For example,

  .. class:: dia-lhs

  ::

  > surround d = c === d === c
  >   where c = circle 0.5
  >
  > s = square 1 # alignB
  >
  > p = s # pad 1.2 # showOrigin # center
  > q = s # frame 0.2 # showOrigin # center
  > r = s # center # showOrigin # pad 1.2
  >
  > example = surround p ||| strutX 0.5
  >       ||| surround q ||| strutX 0.5
  >       ||| surround r

* Envelopes can be "extruded"---like `pad`, but only in a certain
  direction---using `extrudeEnvelope`.  Likewise, `intrudeEnvelope`
  does the same but pushes the envelope inwards.

  .. class:: dia-lhs

  ::

  > {-# LANGUAGE ViewPatterns #-}
  > import Diagrams.TwoD.Vector
  > import Data.Maybe (fromJust)
  >
  > sampleEnvelope2D n d = foldr (flip atop) (d # lc red) bs
  >   where b  = fromJust $ appEnvelope (getEnvelope d)
  >         bs = [stroke $ mkLine (origin .+^ (s *^ v))
  >                               (5 *^ normalized (perp v))
  >              | v <- vs, let s = b v
  >              ]
  >         vs = map r2 [ (2 * cos t, 2 * sin t)
  >                     | i <- [0..n]
  >                     , let t = ((fromIntegral i) * 2.0 * pi)
  >                             / (fromIntegral n)
  >                     ]
  >         mkLine a v = moveTo a $ fromOffsets [v] # center
  >
  > example
  >   = square 2
  >   # extrudeEnvelope (2 ^& 1)
  >   # sampleEnvelope2D 100
  >   # center # pad 1.1

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
  > example = hrule 1 # sizedAs (shapes # scale 0.5 :: D R2)
  >        <> shapes # centerX

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
a pair ``(p,v)`` of a base point and a vector) and returns a sorted
list of parameters ``t`` such that ``p .+^ (t *^ v)`` is a point of
intersection between the ray and the boundary of the diagram.

Normally, a trace is accessed using one of the four functions
`rayTraceV`, `rayTraceP`, `maxRayTraceV`, and `maxRayTraceP`.

* `rayTraceV` takes as inputs a base point ``p``, a vector ``v``, and
  any instance of `Traced`.  It looks for intersection points with the
  given object along the ray determined by ``p`` and ``v``, and finds
  the smallest *positive* scalar ``t`` such that ``p .+^ (t *^ v)`` is
  a point of intersection between the ray and the boundary of the
  `Traced` object.  If such a ``t`` exists, it returns the vector from
  ``p`` to the intersection point, that is, ``t *^ v``.  If there is
  no such intersection, `rayTraceV` returns ``Nothing``.

  Intuitively, restricting to *positive* ``t``-values means that only
  intersection points "in front of" the point ``p`` (that is, in the
  direction of ``v``) are considered.  This tends to be the most
  intuitive behavior, and parallels the way raytracers work---think of
  ``p`` as the location of the "camera" and ``v`` as the direction the
  camera is pointing.  If you want to consider negative ``t``-values,
  see the `traceV` family of functions, described below, or use
  `getTrace` to access the list of all intersection parameters
  directly.

  .. class:: dia-lhs

  ::

  > import Data.Maybe (fromMaybe)
  >
  > drawV v = arrowAt origin v
  >
  > drawTraceV v d
  >   = lc green $
  >     fromMaybe mempty
  >       ((origin ~~) <$> rayTraceP origin v d)
  > illustrateTraceV v d = (d <> drawV v <> drawTraceV v d) # showOrigin
  >
  > example = hcat' (with & sep .~ 1)
  >         . map (illustrateTraceV (0.5 *^ (r2 (1, 1))))
  >         $ [ circle 1 # translate (r2 (-1.5, -1.5))
  >           , circle 1
  >           , circle 1 # translate (r2 (1.5, 1.5))
  >           ]

* `rayTraceP` works similarly, except that it returns the point of
  intersection itself, which lies on the boundary of the object, or
  ``Nothing`` if there is no such point.

  That is, ``rayTraceP p v x == Just p'`` if and only if ``rayTraceV p
  v x == Just (p' .-. p)``.

* `maxRayTraceV` and `maxRayTraceP` are similar to `rayTraceV` and `rayTraceP`,
  respectively, except that they look for the *largest* positive
  ``t``-value, that is, the *furthest* intersection point in the
  direction of ``v``.  Again, intersection points in the opposite
  direction from ``v`` are not considered.

* The `traceV`, `traceP`, `maxTraceV`, and `maxTraceP` functions work
  similarly, but are a bit more low-level: they look for the
  intersection point with the *smallest* (respectively *largest*)
  parameter, even if it is negative.

For even more low-level access, the `Traced` class provides the
`getTrace` method, which can be used to directly access the trace
function for an object.  Given inputs ``p`` and ``v``, it returns a
sorted list of scalars ``t`` such that ``p .+^ (t *^ v)`` is a point
of intersection between the ray ``(p,v)`` and the boundary of the
diagram.

The below diagram illustrates the use of the `rayTraceP` function to
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
>     basePt = p2 (0, -2)
>
> example
>   = hcat' (with & sep .~ 1)
>   . map illustrateTrace
>   $ [ square 1
>     , circle 1
>     , triangle 1 # rotateBy (-1/4) ||| triangle 1 # rotateBy (1/4)
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
    [("bob",[P (2.9999999999999996 ^& 0.0)]),("joe",[P (0.0 ^& 0.0)])]

Of course, there is in fact an entire subdiagram (or subdiagrams)
associated with each name, not just a point; but subdiagrams do not
have a `Show` instance.

Accessing names
~~~~~~~~~~~~~~~

Once we have given names to one or more diagrams, what can we do with
them?  The simplest tool for working with names is `lookupName`, which
has the type

.. class:: lhs

::

  lookupName :: IsName n
             => n -> QDiagram b v m -> Maybe (Subdiagram b v m)

This function takes a name and a diagram, and returns the first
subdiagram associated to that name if any are found, and `Nothing`
otherwise.  (Note that `lookupName` is implemented in terms of the
lower-level lookup functions `lookupSub` and `subMap`; occasionally it
may be useful to directly access these lower-level functions, but the
hope is that you shouldn't need to.)

A more sophisticated tool is `withName`, which has the (admittedly
scary-looking!) type

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
it also gets to use some extra information---namely, the `Subdiagram
v` associated with the name we pass as the first argument to
`withName`.

Finally, the return type of `withName` is itself a transformation of
diagrams.

So here's how `withName` works.  Suppose we call it with the arguments
`withName n f d`.  If some subdiagram of `d` has the name `n`, then
`f` is called with that subdiagram as its first argument, and `d`
itself as its second argument.  So we get to transform `d` based on
information about the given subdiagram, and its context within the
parent diagram `d` (for example, its location, attributes applied to
it, and so on).  And what if there is no subdiagram named `n` in `d`?
In that case `f` is ignored, and `d` is returned unmodified.

Here's a simple example making use of names to draw a line connecting
the centers of two subdiagrams (though for this particular task it is
probably more convenient to use the provided `connect`
function):

.. class:: dia-lhs

::

> data Foo = Baz | Bar | Wibble
>   deriving (Typeable, Eq, Ord, Show)
>
> instance IsName Foo
>
> attach n1 n2
>   = withName n1 $ \b1 ->
>     withName n2 $ \b2 ->
>       atop ((location b1 ~~ location b2) # lc red)
>
> example = (square 3 # named Baz ||| circle 2.3 # named Bar)
>         # attach Baz Bar

The `attach` function takes two names and returns a *function* from
diagrams to diagrams, which adds a red line connecting the locations
denoted by the two names.  Note how the two calls to `withName` are
chained, and how we have written the second arguments to `withName`
using lambda expressions (this is a common style).  Finally, we draw a
line between the two points (using the `location` function to access
the locations of the subdiagrams within the parent diagram), give it a
style, and specify that it should be layered on top of the diagram
given as the third argument to `attach`.

We then draw a square and a circle, give them names, and use `attach`
to draw a line between their centers.  Of course, in this example, it
would not be too hard to manually compute the endpoints of the line
(this is left as an exercise for the reader); but in more complex
examples such manual calculation can be quite out of the question.

`withName` also has two other useful variants:

* `withNameAll` takes a single name and makes available a list of
  *all* subdiagrams associated with that name.
  (`withName`, by contrast, returns only the most recent.)  This is
  useful when you want to work with a collection of named subdiagrams all
  at once.

* `withNames` takes a list of names, and makes available a list of the
  most recent located envelopes associated with each.  Instead of the
  two calls to `withName` in the example above, we could have written

  .. class:: lhs

  ::

  > attach n1 n2
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
> leaves = center
>        . hcat' (with & sep .~ 0.5)
>        $ map (\c -> circle 1 # named c) "abcde"
>
> parentToChild child
>   = withName "root" $ \rb ->
>     withName child  $ \cb ->
>       atop (boundaryFrom rb unit_Y ~~ boundaryFrom cb unitY)
>
> nodes  = root === strutY 2 === leaves
>
> example = nodes # applyAll (map parentToChild "abcde")

Note the use of the `boundaryFrom` function, which uses the traces of
the subdiagrams to compute suitable points on their boundary.

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
> attach n1 n2
>   = withName n1 $ \b1 ->
>     withName n2 $ \b2 ->
>       atop ((location b1 ~~ location b2) # lc red # lw thick)
>
> squares =  (s # named NW ||| s # named NE)
>        === (s # named SW ||| s # named SE)
>   where s = square 1
>
> d = hcat' (with & sep .~ 0.5) (zipWith (|>) [0::Int ..] (replicate 5 squares))
>
> pairs :: [(Name, Name)]
> pairs = [ ((0::Int) .> NE, (2::Int) .> SW)
>         , ((1::Int) .> SE, (4::Int) .> NE)
>         , ((3::Int) .> NW, (3::Int) .> SE)
>         , ((0::Int) .> SE, (1::Int) .> NW)
>         ]
>
> example = d # applyAll (map (uncurry attach) pairs)

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

Localizing names
~~~~~~~~~~~~~~~~

In some situations, giving globally unique names to everything (even
with the qualification mechanism) is a big pain.  The `localize`
function "localizes" the scope of names: any names within a call of
`localize` are not visible outside the call.

.. container:: todo

  Needs an example.

Using queries
-------------

Every diagram has an associated *query*, which assigns a value to
every point in the diagram.  These values must be taken from some
monoid (see `Semigroups and monoids`_).  Combining two diagrams
results in their queries being combined pointwise.

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

> import System.Random (randomRIO)
> import Control.Monad (replicateM)
>
> c :: Diagram B R2
> c = circle 5 # scaleX 2 # rotateBy (1/14)
>
> mkPoint p = (p, circle 0.3
>           	  # lw none
>           	  # fc (case sample c p of
>           	          Any True  -> red
>           	          Any False -> blue
>           	       )
>             )
>
> rand10 :: IO Double
> rand10 = randomRIO (-10,10)
>
> example = do
>   points <- replicateM 20 (mkP2 <$> rand10 <*> rand10)
>   return $ c <> position (map mkPoint points)

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

> import System.Random (randomRIO)
> import Control.Monad (replicateM)
>
> withCount = (# value (Sum 1))
>
> c :: QDiagram B R2 (Sum Int)
> c = (   circle 5 # scaleX 2 # rotateBy (1/14) # withCount
>      <> circle 2 # scaleX 5 # rotateBy (-4/14) # withCount
>     )
>
> mkPoint p = (p, circle (case sample c p of
>                           Sum n  -> 2 * fromIntegral n / 5 + 1/5)
>                 # fc black
>             )
>
> rand10 :: IO Double
> rand10 = randomRIO (-10,10)
>
> example = do
>   points <- replicateM 20 (mkP2 <$> rand10 <*> rand10)
>   return $ c # clearValue <> position (map mkPoint points)

Notice also the use of `clearValue` to get rid of the custom query;
the program that builds this documentation requires `example` to have
the type `QDiagram B R2 Any`.

As another interesting example, consider using a set monoid to keep
track of names or identifiers for the diagrams at a given point.  This
could be used, say, to identify which element(s) of a diagram have been
selected by the user after receiving the coordinates of a mouse click.

Queries and fill rules
~~~~~~~~~~~~~~~~~~~~~~

By default, queries use the winding rule (see `Fill rules`_).  You can
pass an extra option to the `stroke'` function to specify the even-odd
fill rule if you wish.  Be aware that queries are unaffected by
applications of the `fillRule` attribute, which only affects the way a
diagram is drawn.

Bounding boxes
--------------

Envelopes (see `Envelopes`_) are more flexible and
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
`Diagrams.TwoD.Transform.ScaleInv`:mod:.)  In the diagram below, the same
transformation is applied to each pair of arrows.

.. container:: warning

  Diagrams contains native support for drawing arrows (see `Arrows`_);
  the arrows in the example below are constructed manually in order to
  demonstrate scale-invariance.

The arrows on the right are wrapped in `ScaleInv` but the ones on the left are not.

.. class:: dia-lhs

::

> {-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
>
> import Diagrams.TwoD.Transform.ScaleInv
> import Control.Lens ((^.))
>
> class Drawable d where
>   draw :: d -> Diagram B R2
>
> instance Drawable (Diagram B R2) where
>   draw = id
>
> instance Drawable a => Drawable (ScaleInv a) where
>   draw = draw . (^. scaleInvObj)
>
> instance (Drawable a, Drawable b) => Drawable (a,b) where
>   draw (x,y) = draw x <> draw y
>
> arrowhead, shaft :: Diagram B R2
> arrowhead = triangle 0.5 # fc black # rotateBy (-1/4)
> shaft = origin ~~ p2 (3, 0)
>
> arrow1 = (shaft,          arrowhead       # translateX 3)
> arrow2 = (shaft, scaleInv arrowhead unitX # translateX 3)
>
> showT tr = draw (arrow1 # transform tr)
>        ||| strutX 1
>        ||| draw (arrow2 # transform tr)
>
> example = vcat' (with & sep .~ 0.5)
>             (map (centerX . showT)
>               [ scalingX (1/2)
>               , scalingY 2
>               , scalingX (1/2) <> rotation (-1/12 @@ turn)
>               ])

In addition, the `scaleInvPrim` function creates a scale-invariant
diagram from a primitive (such as a path).  At the moment it is not
possible to create a scale-invariant diagram from another *diagram*.

Measurement expressions
-----------------------

.. container:: todo

  Go through this section and update it if we merge physical units for
  `Output`.

There is more to `Measure`\s (see `Measurement units`_) than just the
four reference frames.  In fact, a small domain-specific language for
constructing measurements is provided, with the following features:

* `atLeast :: Measure v -> Measure v -> Measure v` finds the maximum
  of two measurements.  For example, `Normalized 0.2 \`atLeast\`
  Local 1` evaluates to whichever measurement ends up being larger,
  `Normalized 0.2` or `Local 1`.

  In fact, the standard line widths like `medium`, `thick`, *etc.*
  are defined as `Normalized w \`atLeast\` Output 0.5`, each with a
  different value of `w` (for example, for `medium`, `w = 0.004`).
* Similarly, `atMost` takes the minimum of two `Measure`\s.
* `Measure v` is an instance of `AdditiveGroup`, which provides `zeroV
  :: Measure v`, `negateV :: Measure v -> Measure v`, and `(^+^)` for
  adding measurements.  For example, `Normalized 0.1 ^+^ Output 1`
  represents 10% of the width or height of the diagram plus one output
  unit.
* `Measure v` is also an instance of `VectorSpace`, which provides
    `(*^) :: Scalar v -> Measure v -> Measure v` as a scaling operation.

The semantics of these expressions is what you would expect:
everything is first converted to compatible units, and then the
operations are interpreted in the obvious way.

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
* `juxtapose` can be used to position a diagram relative to
  something else without composing them; see `Juxtaposing without
  composing`_.
* `moveTo` can be used to position a single diagram absolutely.
* `place` is a flipped version of `moveTo` which is sometimes
  convenient.

Delayed composition
-------------------

Suppose we have four diagrams that we want to lay out relative to one
another.  For example:

.. class:: dia-lhs

::

> t = triangle   5   # fc orange
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
At this point, we are out of luck.  There is (currently) no way to
break apart a diagram into subdiagrams once it has been composed
together.  We could use `juxtapose` (which positions one diagram
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

> t = triangle   5   # fc orange
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
`TrailLike` class (see `TrailLike`_).  You can often
take advantage of this to do some custom processing of shapes by
creating a *trail* instead of a diagram, doing some processing, and
then turning the trail into a diagram.

In particular, assigning names to the vertices of a shape can be
accomplished as follows. Instead of writing just (say) `pentagon`, write

.. class:: lhs

::

> stroke' ( with & vertexNames .~ [[0..]] ) pentagon

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

For example, the solution to the problem with `width` is to annotate
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

> hcat' ( with & sep .~ 0.5 ) (zipWith (|>) [0 .. ] (replicate 5 squares))

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
        (with & sep .~ 0.5)) (zipWith (|>) [0 .. ] (replicate 5 squares))

The problem, again, is that GHC does not know what type to choose for
some polymorphic value.  Here, the polymorphic values in question are
the numbers `0`, `1`, ... Numeric literals are polymorphic in Haskell,
so GHC does not know whether they should be `Int`\s or `Integer`\s or
`Double`\s or... The solution is to annotate the `0` with the desired
type.

Creating 3D diagrams
====================

``diagrams``' support for three dimensions is growing: currently,
modules `Diagrams.ThreeD.Align`:mod:, `Diagrams.ThreeD.Camera`:mod:,
`Diagrams.ThreeD.Light`:mod:, `Diagrams.ThreeD.Shapes`:mod:,
`Diagrams.ThreeD.Transform`:mod:, `Diagrams.ThreeD.Types`:mod:, and
`Diagrams.ThreeD.Vector`:mod: are all included in `diagrams-lib`:pkg:
(though they are not exported from `Diagrams.Prelude`:mod:).  This
should still be considered a "feature preview"---in particular,
appropriate 3D backends are still under construction (see
`diagrams-povray`:repo: and `diagrams-opengl`_).  Look for fuller (and
more fully documented) support for 3D diagrams in an upcoming release!
In the meantime, consult the `3D tutorial`_ for a more detailed
feature preview.

.. _`diagrams-opengl`: https://github.com/bergey/diagrams-opengl
.. _`3D tutorial`: 3D.html

Animation
=========

Diagrams has experimental support for the creation of *animations*.
Animations are created with the help of a generic `Active`
abstraction, defined in the `active`:pkg: package. Additionally,
animated GIFs can be created using the cairo backend.

.. container:: warning

  The `active`:pkg: package is being completely rewritten based on a
  much improved semantics.  The rewritten version is slated for
  integration with an upcoming version of diagrams, Real Soon Now
  (tm).

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

> (translateX <$> ui <*> circle 2) <> (pure (square 6 # lw none))

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


Animated GIFs
-------------

Animated GIFs can be created directly using the cairo backend.  This
is done by calling `mainWith` with an argument of type `[(Diagram
Cairo R2, GifDelay)]` where `GifDelay` is a synonym for `Int`. Each
tuple is a diagram frame of the animation and a time in hundredths of
a second until the next frame.  This creates an executable which takes
an output file with the extension gif. The other command line options
which can be used are ``--dither`` (to turn on dithering), ``--looping-off``,
and ``--loop-repeat`` (to specify the number of times to repeat the loop
after the first time).


Rendering backends
==================

Diagrams has a system for "pluggable" rendering backends, so new
backends can be added by implementing instances of some type classes.
There are currently four "officially supported" backends, described
below, and several other unofficial or experimental backends.  New
backends are welcome!  To get started, take a look at the existing
backends for examples, read the section below on `Tools for
backends`_, and consult the `core library reference`__.

__ core.html

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

Gradient support is complete in this backend however, most browsers to not
handle the SVG spec correctly when it comes to reflect and repeat.
Apparently only Chrome and IE follow the spec correctly at this point, while
Safari does not handle reflect and repeat at all and Firefox gets it wrong.

The source code for the SVG backend can be found in the
`diagrams-svg`:repo: repository. Note the functions `renderDia` and
`renderSVG` for rendering diagrams directly.

The postscript backend
----------------------

The postscript backend, `diagrams-postscript`:pkg:, like the SVG
backend, is written purely in Haskell.  It outputs encapsulated
PostScript (EPS) files.  Note that by nature, EPS does not support
transparency.  The postscript backend also does not support embedded
images.  However, it is fairly complete in its support for other
features with the exception of gradients.

The source code for the postscript backend can be found in the
`diagrams-postscript`:repo: repository.

The cairo backend
-----------------

The cairo backend, `diagrams-cairo`:pkg:, is built on top of the
`cairo`:pkg: package, which contains bindings to the `cairo 2D
graphics library`_.  Although it is quite full-featured, the cairo
library itself can be unfortunately difficult to install on some
platforms, particularly OS X.

.. _`cairo 2D graphics library`: http://www.cairographics.org/

The cairo backend can produce PNG, SVG, PDF, postscript,
and animated GIF output. The cairo backend does support gradients
however, do to a bug in the cairo package it does not handle reflect
and repeat correctly for radial gradients,

.. _ `Extend`:http://hackage.haskell.org/package/cairo-0.12.5.3/docs/Graphics-Rendering-Cairo.html#t:Extend

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

* Direct output of animated GIFs.

The source code for the cairo backend can be found in the
`diagrams-cairo`:repo: repository.  The functions `renderDia` and
`renderCairo` provide an alternative to the
`Diagrams.Backend.Cairo.CmdLine` interface for more programmatic
control of the output.

The GTK backend
---------------

The GTK backend, `diagrams-gtk`:pkg:, used to be part of the cairo
backend (and is still built on top of it), but has been split out into
a separate package in order to reduce the dependencies of the cairo
backend, hence making it easier to install for those who don't need
GTK support.  You can install it at the same time as the rest of the
diagrams framework by passing the `-fgtk` flag: ``cabal install -fgtk
diagrams``, or it can be installed separately later with ``cabal
install diagrams-gtk``.

The GTK backend allows rendering diagrams directly to GTK windows
instead of to a file (`defaultRender` and `renderToGtk`).  Note that
it is possible to receive mouse clicks and then query the
corresponding location in a diagram to find out which part the user
clicked on (see `Using queries`_).

The source code for the GTK backend can be found in the
`diagrams-gtk`:repo: repository.

The Rasterific backend
----------------------

The Rasterific backend is built on top of the `Rasterific`:pkg: package, which
is a pure haskell rasterizer that uses `JuicyPixels`:pkg: and `FontyFruity`:pkg:.
This is a fully featured backend that supports almost everything that the cairo
backend does plus a few other things. It can produce PNG, JPG, BMP, TIF and
animated GIF images. It also supports embedded images (see `DImage`) and
although does not yet have the text handling capabilities of cairo, it does use
the exact text bounding box for alignment. Gradients are fully supported
including, repeat and reflect.

The Rasterific backend can be invoked via
`Diagrams.Backend.Rasterific.CmdLine`:mod: module, or via the
`renderDia`/`renderRasterific` functions.

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

Other tools
===========

There are several "extra" packages which are officially maintained but
do not automatically come bundled with the `diagrams` package.

diagrams-builder
----------------

The `diagrams-builder`:pkg: package provides a service for *dynamic*
rendering of diagrams---that is, you hand it a `String` at runtime
representing some diagrams code, and you get back the result of
rendering the code using whatever backend you like.  This could be
useful, for example, as part of a preprocessor tool for interpreting
diagrams code embedded in some other document.  Currently it is used
by the `BlogLiterately-diagrams`:pkg: package (for rendering diagrams
embedded in blog posts) as well as `diagrams-haddock`:pkg: (for
rendering diagrams embedded in Haddock comments).

diagrams-haddock
----------------

`diagrams-haddock`:pkg: is a tool for embedding diagrams in Haddock
documentation.  The idea is that you can add images (worth 1000+
words, of course) to your documentation simply by embedding diagrams
code in a special format, and then running `diagrams-haddock`:pkg:.
See the `README`__ for instructions on using it.

__ https://github.com/diagrams/diagrams-haddock/blob/master/README.md

SVGFonts
--------

The `SVGFonts`:pkg: provides support for reading fonts in SVG format
and rendering text to diagrams paths.  For more, see `Native font support`_.

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

Further reading: `Envelopes and local vector spaces`_; `Envelopes`_.

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

Further reading: `Attributes and styles`_; `Text`_.

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

Further reading: `Attributes and styles`_; `Text`_.

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

Classes for trails and paths
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

TrailLike
+++++++++

The `TrailLike` class, defined in `Diagrams.TrailLike`:mod:, abstracts
over things that are "trail-like", so that functions such as `square`
can be used to construct a diagram, a path, a trail, *etc.*.

.. class:: lhs

::

> class (InnerSpace (V t), OrderedField (Scalar (V t))) => TrailLike t where
>
>   trailLike
>     :: Located (Trail (V t))  -- ^ The concretely located trail.  Note
>                               --   that some trail-like things
>                               --   (e.g. 'Trail's) may ignore the
>                               --   location.
>     -> t

The `trailLike` method provides a generic way to build a "trail-like"
thing by specifying the low-level trail data.  Note that there should
usually not be any need for end users to call `trailLike` directly
(though there certainly may be some use cases).

Instances:

* `Trail`: this instance simply throws away the location.
* `Trail' Line`: throw away the location, and perform `cutLoop` if
  necessary.  For example, `circle 3 :: Trail' Line R2` is an open `360^\circ`:math:
  circular arc.
* `Trail' Loop`: throw away the location, and perform `glueLine` if
  necessary.
* `Path`: construct a path with a single component.
* `Diagram b R2`: as long as the backend `b` knows how to render 2D
  paths, `trailLike` can construct a diagram by stroking the generated
  single-component path.
* `[Point v]`: this instance generates the vertices of the trail.
* `Located (Trail v)`, of course, has an instance which amounts to the
  identity function.  More generally, however, `Located a` is an
  instance of `TrailLike` for *any* type `a` which is also an
  instance.  In particular, the resulting `Located a` has the location
  of the input `Located Trail`, and a value of type `a` generated by
  another call to `trailLike`.  This is most useful for generating
  values of type `Located (Trail' Line v)` and `Located (Trail' Loop
  v)`.  For example, `circle 3 # translateX 2 :: Located (Trail' Line
  R2)` is an open `360^\circ`:math: circular arc centered at
  `(2,0)`:math:.
* `Active t` (for any `TrailLike p`): creates a constant `Active`
  value.

Further reading: `Trails and paths`_; `Trails`_;
`Paths`_; `TrailLike`_.

Classes for parametric objects
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`Diagrams.Parametric`:mod: provides a set of classes for working with
objects which can be viewed as parametric functions, such as segments
and trails: `Parametric`, `DomainBounds`, `EndValues`, `Sectionable`,
and `HasArcLength`.  These classes are fairly specialized and do not
really show up anywhere else; see the section on `Segments and trails
as parametric objects`_ for more information.

Classes for backends
~~~~~~~~~~~~~~~~~~~~

Backend
+++++++

The `Backend` class, defined in `Diagrams.Core.Types`:mod:, defines
the primary interface for any diagrams rendering backend.  Unlike many
of the other type classes in diagrams, it is quite large.  For a full
discussion, see the `core library reference`__.

__ core.html

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

Further reading: `Rendering backends`_.

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

ToResult
++++++++

The `ToResult` class (from `Diagrams.Backend.CmdLine`:mod:)
essentially defines a very generic form of uncurrying.  It is used to
implement the general interface for building command-line-driven
diagram generation programs, and in particular to enable building
executables out of (curried) functions, which requires collecting up
all their arguments at once.  See the `command-line tutorial`__ for more.

__ cmdline.html

.. class:: lhs

::

> class ToResult d where
>     type Args d :: *
>     type ResultOf d :: *
>
>     toResult :: d -> Args d -> ResultOf d

The most interesting instance is the one for functions:

.. class:: lhs

::

> instance ToResult d => ToResult (a -> d) where
>     type Args (a -> d) = (a, Args d)
>     type ResultOf (a -> d) = ResultOf d
>
>     toResult f (a,args) = toResult (f a) args

Parseable
+++++++++

The `Parseable` class (`Diagrams.Backend.CmdLine`:mod:) contains just
one method, `parser :: Parser a`, which defines a command-line parser
for a given type.  Things with `Parseable` instances can be used in
conjunction with the command-line creation framework.  See the
`command-line tutorial`__ for more.

__ cmdline.html

Mainable
++++++++

The `Mainable` class (`Diagrams.Backend.CmdLine`:mod:) governs types
which can be used to build a command-line-driven diagram-generation
program.  For example, a diagram; but also animations, lists of
diagrams, association lists of strings and diagrams, and functions
from parseable things to any of the above.  See the `command-line
tutorial`__ for more.

.. class:: lhs

::

> class Mainable d where
>     type MainOpts d :: *
>
>     mainArgs   :: Parseable (MainOpts d) => d -> IO (MainOpts d)
>     mainRender :: MainOpts d -> d -> IO ()
>     mainWith   :: Parseable (MainOpts d) => d -> IO ()

__ cmdline.html

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

*Type families* are a GHC extension to Haskell enabling "type-level
functions".  You can `read about them in detail here`__, but
understanding them enough to use them as they arise in diagrams is not
anywhere near as complicated as that page might suggest.  Simply put,
type families are functions which can take types as input and produce
other types as output.  Of course, in one sense any polymorphic type
constructor already does this: for example, `Maybe` takes types as
input (say, `Int`) and produces types as output (say, `Maybe Int`).
The difference is that `Maybe` works *uniformly* for all input types
(it does not, indeed cannot, do anything "different" for `Char` than
it does for `Int`).  Type families, on the other hand, can have a
specific definition for each input type (much as type class methods
can have a different implementation for each instance type).  For
example, the following (admittedly contrived) example declares a type
family named `Foo`, with two definition clauses.

.. class:: lhs

::

> type family Foo a :: *
> type instance Foo Int  = Int
> type instance Foo Char = [String]

__ http://www.haskell.org/haskellwiki/GHC/Type_families

Diagrams only makes use of a few type families, though two of them
(`V` and `Scalar`) are used quite extensively.  The following sections
list each of the type families employed by diagrams

V
~

The `V` type family is defined in `Diagrams.Core.V`.  The idea is that
many types have an "associated" vector space, *i.e.* the vector space
in which they "live".  `V` simply maps from types to their associated
vector space.  For example, `V (Path R2) = R2` (two-dimensional paths
live in `R2`), and `V [a] = V a` (lists of `a`\'s live in whatever
vector space `a`\'s themselves live in).

Often, `V` shows up in a constraint on the left hand side of `=>`, as
in

.. class:: lhs

::

> alignT :: (Alignable a, V a ~ R2) => a -> a

This type says that `alignT` can be applied to values of any type `a`,
*as long as* `a` is an instance of `Alignable`, and `a` lives in the
vector space `R2`, that is, `V a ~ R2` (the tilde expresses a *type
equality constraint*).

Other times, `V` can show up on the right-hand side of `=>`, as in

.. class:: lhs

::

> decorateTrail :: (...) => Trail (V a) -> [a] -> a

This says that `decorateTrail` takes two arguments: a `Trail` and a
list of values of some type `a`.  However, `Trail`\s are parameterized
by a vector space; `Trail (V a)` means that the vector space of the
trail is the vector space associated to `a`.

Scalar
~~~~~~

The `Scalar` type family is defined in the `vector-space`:pkg:.  It
associates a scalar type to each instance of `VectorSpace`. A "scalar"
can be thought of as a distance, or scaling factor.  For example, you
can scale a vector by a scalar (using `(*^)`), and the `magnitude`
function takes a vector and returns a scalar.

Diff
~~~~

`Diff` is another associated type family from `vector-space`:pkg:,
which associates to each instance of `AffineSpace` a vector space
which is the type of the "difference" between two affine values (as
given by `(.-.)`).  For example, `Diff P2 ~ R2`, that is, the
difference between two points is a vector.

Render
~~~~~~

`Render` is an associated data family of the `Backend` class.  It
determines the type of rendering operations for a given backend.  For
more information, see the `core library reference`__.

__ core.html

Result
~~~~~~

`Result` is an associated type family of the `Backend` class.  It
determines the type of the final result obtained from the backend
after rendering a complete diagram.  For more information, see
the `core library reference`__.

__ core.html

Options
~~~~~~~

`Options` is an associated data family of the `Backend` class.  It
determines the type of options which can be passed to the backend when
initiating a rendering operation.  For more information, see the `core
library reference`__.

__ core.html

Codomain
~~~~~~~~

`Codomain` is a type family defined in `Diagrams.Parametric`:mod:.
Parametric objects of type `a` can be viewed as functions of type
`Scalar (V a) -> Codomain a`.  For more information, see `Segments and
trails as parametric objects`_.
