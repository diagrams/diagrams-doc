.. role:: pkg(literal)
.. role:: hs(literal)
.. role:: mod(literal)
.. role:: repo(literal)

.. default-role:: hs

=============================
Diagrams Quick Start Tutorial
=============================

.. contents::

Introduction
============

This tutorial will walk you through the basics of using the diagrams
DSL to create graphics in a powerful, modular, and declarative way.
There's enough here to get you started quickly; for more in-depth
information, see the `user manual`_.

.. _`user manual`: /doc/diagrams-manual.html

This is not a Haskell tutorial (although a
Haskell-tutorial-via-diagrams is a fun idea and may happen in the
future).  For now, we recommend `Learn You a Haskell`_ for a nice
introduction to Haskell; Chapters 1-6 should give you pretty much all
you need for working with diagrams.

.. _`Learn You a Haskell`: http://learnyouahaskell.com/

This tutorial is available in several formats:

* HTML_
* `Literate Haskell`_  (XXX is this true?!)

.. _HTML: /doc/diagrams-tutorial.html
.. _`Literate Haskell`: /doc/DiagramsTutorial.lhs

Whatever you do, don't just read it: download the ``.lhs`` version
so you can play around with the content of the tutorial interactively!

Resources
=========

Some resources that may be helpful to you as you learn about diagrams:

* The `user manual`_
* The `API documentation`_
* The `diagrams-discuss mailing list`_
* The ``#diagrams`` IRC channel on freenode.org

.. _`API documentation`: /haddock/index.html
.. _`diagrams-discuss mailing list`: http://groups.google.com/group/diagrams-discuss

Getting started
===============

Before getting on with generating beautiful diagrams, you'll need a
few things:

GHC/The Haskell Platform
------------------------

You'll need a recent version of the `Glasgow Haskell
Compiler`_ (7.4 or later), as well as some
standard libraries and tools.  There are several methods for obtaining
these:

* `The Haskell Platform`_ has everything you need in one convenient
  package. If you are unsure, you should use this.

* If you already have GHC and/or know what you are doing and want to
  install things yourself, just make sure you have `GHC`_ and a recent
  version of the `cabal-install tool`_.

.. _`Glasgow Haskell Compiler`: http://haskell.org/ghc
.. _`The Haskell Platform`: http://hackage.haskell.org/platform/
.. _`GHC`: http://haskell.org/ghc
.. _`cabal-install tool`: http://hackage.haskell.org/trac/hackage/wiki/CabalInstall

Installation
------------

Once you have the prerequisites, installing the diagrams libraries
themselves should be a snap:

::

    cabal install diagrams

`diagrams`:pkg: is just a wrapper package which pulls in the following
four packages:

* `diagrams-core`:pkg: contains the core data structures and definitions
  that form the abstract heart of the library.

* `diagrams-lib`:pkg: is a standard library of drawing primitives,
  attributes, and combinators built on top of the core library.

* `diagrams-contrib`:pkg: is a library of user-contributed extensions.

* `diagrams-svg`:pkg: is a backend which renders diagrams as SVG files.

There are other backends as well; see the `diagrams package
documentation`_ and the `diagrams wiki`_ for more information.

.. _`diagrams package documentation`: http://hackage.haskell.org/package/diagrams
.. _`diagrams wiki`: http://www.haskell.org/haskellwiki/Diagrams/Projects#Backends

Philosophy
==========

Before diving in to create some diagrams, it's worth taking a minute
to explain some of the philosophy that drove many of the design
decisions. (If you're particularly impatient, feel free to skip this
section for now---but you might want to come back and read it later!)

* Positioning and scaling are always *relative*.  There is never any
  global coordinate system to think about; everything is done relative
  to diagrams' *local* vector spaces.  This is not only easier to think
  about, it also increases modularity and compositionality, since diagrams
  can always be designed without thought for the context in which they
  will eventually be used.  Doing things this way is more work for the
  *library* and less work for the *user*, which is the way it should be.

* Almost everything is based around the concept of *monoids* (more on
  this later).

* The core library is as simple and elegant as possible---almost
  everything is built up from a very small set of primitive types and
  operations.  One consequence is that diagrams is optimized for
  simplicity and flexibility rather than for speed; if you are looking
  to do *real-time* graphics generation you will probably be best
  served by looking elsewhere! (With that said, however, we certainly
  are interested in making diagrams as fast as possible without
  sacrificing other features.)

Your first diagram
==================

Create a file called `DiagramsTutorial.hs`
with the following contents:

.. class:: lhs

::

> {-# LANGUAGE NoMonomorphismRestriction #-}
>
> import Diagrams.Prelude
> import Diagrams.Backend.SVG.CmdLine
>
> main = defaultMain (circle 1)

Turning off the Dreaded Monomorphism Restriction is quite important:
if you don't, you will almost certainly run into it (and be very
confused by the resulting error messages).

The first `import` statement brings into scope the entire diagrams DSL
and standard library.  The second `import` is so that we can use the
SVG backend for rendering diagrams.  Among other things, it provides
the function `defaultMain`, which takes a diagram as input (in this
case, a circle of radius 1) and creates a command-line-driven
application for rendering it.

Let's compile and run it:

::

    $ ghc --make DiagramsTutorial.lhs
    [1 of 1] Compiling Main             ( DiagramsTutorial.lhs, DiagramsTutorial.o )
    Linking DiagramsTutorial ...
    $ ./DiagramsTutorial -o circle.svg -w 400

If you now view `circle.svg` in your favorite web browser, you should
see an unfilled black circle on a white background (actually, it's on
a transparent background, but most browsers I know of use white):

.. class:: dia-lhs

::

> example = circle 1

Be careful not to omit the ``-w 400`` argument!  This specifies that the
width of the output file should be 400 units, and the height should
be determined automatically.  You can also specify just a height
(using `-h`), or both a width and a height if you know the exact
dimensions of the output image you want (note that the diagram will
not be stretched; extra padding will be added if the aspect ratios do
not match).  If you do not specify a width or a height, the absolute
scale of the diagram itself will be used, which in this case would be
rather tiny---only 2x2.

There are several more options besides `-o`, `-w`, and `-h`; you can
see what they are by typing `./DiagramsTutorial --help`.

Attributes
==========

Suppose we want our circle to be blue, with a thick dashed purple
outline (there's no accounting for taste!).  We can apply attributes to
the `circle` diagram with the `(#)` operator:

.. class:: dia-lhs

::

> blueCircle = circle 1 # fc blue
>                       # lw 0.05
>                       # lc purple
>                       # dashing [0.2,0.05] 0
>
> example = blueCircle

There's actually nothing special about the `(#)` operator: it's just
reverse function application, that is,

.. class:: lhs

::

> x # f = f x

Just to illustrate,

.. class:: dia-lhs

::

> example = dashing [0.2,0.05] 0 . lc purple . lw 0.05 . fc blue
>         $ circle 1

produces exactly the same diagram as `blueCircle`.  So why bother with
`(#)`?  First, it's often more natural to write (and easier to read)
what a diagram *is* first, and what it is *like* second.  Second,
`(#)` has a high precedence (namely, 8), making it more convenient to
combine diagrams with specified attributes.  For example,

.. class:: dia-lhs

::

> example = circle 1 # fc red # lw 0 ||| circle 1 # fc green # lw 0

places a red circle with no border next to a green circle with no
border (we'll see more about the `(|||)` operator shortly). Without
`(#)` we would have to write something with more parentheses, like

.. class:: lhs

::

> (fc red . lw 0 $ circle 1) ||| (fc green . lw 0 $ circle 1)

For information on other standard attributes, see
the `Diagrams.Attributes`:mod: module.

Combining diagrams
==================

OK, so we can draw a single circle: boring!  Much of the power of the
diagrams framework, of course, comes from the ability to build up
complex diagrams by *combining* simpler ones.

Let's start with the most basic way of combining two diagrams:
superimposing one diagram on top of another.  We can accomplish this
with `atop`:

.. class:: dia-lhs

::

> circleSq = square 1 # fc aqua `atop` circle 1
>
> example = circleSq

(Incidentally, these colors are coming from the
`Data.Colour.Names`:mod: module.)

"Putting one thing on top of another" sounds rather vague: how do we
know exactly where the circle and square will end up relative to one
another?  To answer this question, we must introduce the fundamental
notion of a *local origin*.

Local origins
-------------

Every diagram has a distinguished point called its *local origin*.
Many operations on diagrams---such as `atop`---work somehow with
respect to the local origin.  `atop` in particular works by
superimposing two diagrams so that their local origins coincide (and
this point becomes the local origin of the new, combined diagram).

The `showOrigin` function is provided for conveniently visualizing the
local origin of a diagram.

.. class:: dia-lhs

::

> circleWithO = circle 1 # showOrigin
>
> example = circleWithO

Not surprisingly, the local origin of `circle` is at its center.  So
is the local origin of `square`.  This is why ``square 1 `atop` circle 1``
produces a square centered on a circle.

Side-by-side
------------

Another fundamental way to combine two diagrams is by placing them
*next to* each other.  The `(|||)` and `(===)` operators let us
conveniently put two diagrams next to each other in the horizontal or
vertical directions, respectively.  For example:

.. class:: dia-lhs

::

> circleSqH = circle 1 ||| square 2
>
> example = circleSqH



.. class:: dia-lhs

::

> circleSqV = circle 1 === square 2
>
> example = circleSqV

The two diagrams are arranged next to each other so that their local
origins are on the same horizontal or vertical line.  As you can
ascertain for yourself with `showOrigin`, the local origin of the new,
combined diagram coincides with the local origin of the first diagram.

`(|||)` and `(===)` are actually just convenient specializations of
the more general `beside` combinator. `beside` takes as arguments a
*vector* and two diagrams, and places them next to each other "along
the vector"---that is, in such a way that the vector points from the
local origin of the first diagram to the local origin of the second.

.. class:: dia-lhs "foo bar"

::

> circleSqV1 = beside (r2 (1,1)) (circle 1) (square 2)
>
> circleSqV2 = beside (r2 (1,-2)) (circle 1) (square 2)
>
> example = hcat [circleSqV1, strutX 1, circleSqV2]

Notice how we use the `r2` function to create a 2D vector from a pair
of coordinates.

Envelopes
---------

How does the diagrams library figure out how to place two diagrams
"next to" each other?  And what exactly does "next to" mean?  There
are many possible definitions of "next to" that one could imagine
choosing, with varying degrees of flexibility, simplicity, and
tractability.  The definition of "next to" adopted by diagrams is as
follows:

To place two diagrams next to each other in the direction
of a vector *v*, place them as close as possible so that there is a
*separating line* perpendicular to *v*; that is, a line perpendicular
to *v* such that the first diagram lies completely on one side of the
line and the other diagram lies completely on the other side.

There are certainly some tradeoffs in this choice. The biggest
downside is that adjacent diagrams sometimes end up with undesired
space in between them.  For example, the two rotated ellipses in the
diagram below have some space between them. (Try adding a vertical
line between them with `vrule` and you will see why.)

.. class:: dia-lhs

::

> e2 = ell ||| ell
>   where ell = circle 1 # scaleX 0.5 # rotateBy (1/6)
>
> example = e2

If we want to position these ellipses next to each other horizontally
so that they are tangent, it is not clear how to accomplish this.
(However, it should be possible to create higher-level modules for
automatically accomplishing this in certain cases.)

However:

* This rule is very *simple*, in that it is easy to predict what will
  happen when placing two diagrams next to each other.

* It is also *tractable*.  Every diagram carries along with it an
  "envelope"---a function which takes as input a vector *v*, and returns
  the minimum distance to a separating line from the local origin in the
  direction of *v*.  When composing two diagrams with `atop` we take the
  pointwise maximum of their envelopes; to place two diagrams
  next to each other we use their envelopes to decide how to
  reposition their local origins before composing them with `atop`.

Transforming diagrams
=====================

As you would expect, there is a range of standard functions available
for transforming diagrams, such as:

* `scale` (scale uniformly)
* `scaleX` and `scaleY` (scale in the X or Y axis only)
* `rotate` (rotate by an angle in radians)
* `rotateBy` (rotate by a fraction of a circle)
* `reflectX` and `reflectY` for reflecting along the X and Y axes

For example:

.. class:: dia-lhs

::

> circleRect  = circle 1 # scale 0.5 ||| square 1 # scaleX 0.3
>
> circleRect2 = circle 1 # scale 0.5 ||| square 1 # scaleX 0.3
>                                                 # rotateBy (1/6)
>                                                 # scaleX 0.5
>
> example = hcat [circleRect, strutX 1, circleRect2]

(Of course, `circle 1 # scale 0.5` would be better written as just `circle 0.5`.)

Translation
-----------

Of course, there are also translation transformations like
`translate`, `translateX`, and `translateY`.  These operations
translate a diagram within its *local vector space*---that is,
relative to its local origin.

.. class:: dia-lhs

::

> circleT = circle 1 # translate (r2 (0.5, 0.3)) # showOrigin
>
> example = circleT

As `circleT` shows, translating a diagram by `(0.5, 0.3)` is the same
as moving its local origin by `(-0.5, -0.3)`.

Since diagrams are always composed with respect to their local
origins, translation can affect the way diagrams are composed.

.. class:: dia-lhs

::

> circleSqT   = square 1 `atop` circle 1 # translate (r2 (0.5, 0.3))
> circleSqHT  = square 1 ||| circle 1 # translate (r2 (0.5, 0.3))
> circleSqHT2 = square 1 ||| circle 1 # translate (r2 (19.5, 0.3))
>
> example = hcat [circleSqT, strutX 1, circleSqHT, strutX 1, circleSqHT2]

As `circleSqHT` and `circleSqHT2` demonstrate, when we place a
translated circle next to a square, it doesn't matter how much the
circle was translated in the *horizontal* direction---the square and
circle will always simply be placed next to each other.  The vertical
direction matters, though, since the local origins of the square and
circle are placed on the same horizontal line.

Aligning
--------

It's quite common to want to *align* some diagrams in a certain way
when placing them next to one another---for example, we might want a
horizontal row of diagrams aligned along their top edges.  The
*alignment* of a diagram simply refers to its position relative to its
local origin, and convenient alignment functions are provided for
aligning a diagram with respect to its envelope.  For example,
`alignT` translates a diagram in a vertical direction so that its
local origin ends up exactly on the edge of its envelope.

.. class:: dia-lhs

::

> circlesTop = hrule (2 * sum sizes) # lw 0.1 === circles # centerX
>   where circles = hcat . map alignT . zipWith scale sizes
>                 $ repeat (circle 1 # lw 0.1)
>         sizes   = [2,5,4,7,1,3]
>
> example = circlesTop

See `Diagrams.TwoD.Align`:mod: for other alignment combinators.

Diagrams as a monoid
====================

As you may have already suspected if you are familiar with monoids,
diagrams form a monoid under `atop`.  This means that you can use
`(<>)` instead of `atop` to superimpose two diagrams.  It also means
that `mempty` is available to construct the "empty diagram", which
takes up no space and produces no output.

Quite a few other things in the diagrams standard library are also
monoids (transformations, trails, paths, styles, colors, envelopes,
traces...).

Next steps
==========

This tutorial has really only scratched the surface of what is
possible! Included among the many things *not* covered by this
tutorial are trails, paths, splines, text, traces, a wide array of
predefined shapes, named subdiagrams, animation...  Here are pointers
to some resources for learning more:

* The diagrams `user manual`_ goes into much more depth on all the
  topics covered in this tutorial, plus many others, and includes lots
  of illustrative examples.  If there is anything in the manual that
  you find unclear, confusing, or omitted, please `report it as a
  bug`_!

* The diagrams-lib API is generally well-documented; start with the
  documentation for `Diagrams.Prelude`:mod:, and then drill down from
  there to learn about whatever you are interested in.  If there is
  anything in the API documentation that you find unclear or
  confusing, please `report it as a bug as well`_!

* If you run into difficulty or have any questions, join the
  ``#diagrams`` IRC channel on freenode.org, or the `diagrams-discuss
  mailing list`_.

.. _`report it as a bug`: http://github.com/diagrams/diagrams-doc/issues
.. _`report it as a bug as well`: http://github.com/diagrams/diagrams/issues
.. _`diagrams-discuss mailing list`: http://groups.google.com/group/diagrams-discuss
