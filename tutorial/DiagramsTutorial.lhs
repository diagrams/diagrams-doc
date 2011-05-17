Introduction
============

This tutorial will walk you through the basics of using the diagrams
DSL to create graphics in a powerful, modular, and declarative way.
It is still in the process of being written, but hopefully there's
enough here to get you started.

This is not a Haskell tutorial (although a
Haskell-tutorial-via-diagrams is a fun idea and may happen in the
future).  For now, we recommend [Learn You a
Haskell](http://learnyouahaskell.com/) for a nice introduction to
Haskell; Chapters 1-6 should give you pretty much all you need for
working with diagrams.

This tutorial is available in both
[HTML](http://projects.haskell.org/diagrams/tutorial/DiagramsTutorial.html)
and [literate
Haskell](http://projects.haskell.org/diagrams/tutorial/DiagramsTutorial.lhs)
formats.  Don't just read it: download the `.lhs` version so you can
play around with it interactively!

Resources
=========

Some resources that may be helpful to you as you learn about diagrams:

* The API documentation:
    * [diagrams-core](http://hackage.haskell.org/package/diagrams-core)
    * [diagrams-lib](http://hackage.haskell.org/package/diagrams-lib)
* The [diagrams-discuss mailing list](http://groups.google.com/group/diagrams-discuss)
* The `#diagrams` IRC channel on freenode.org

Getting started
===============

Before getting on with generating beautiful diagrams, you'll need a few things:

GHC/The Haskell Platform
------------------------

You'll need a recent version of the [Glasgow Haskell
Compiler](http://haskell.org/ghc) (6.12.3 *or* 7.0.2 or later; avoid
7.0.1 since it has a typechecking bug which some of the path drawing
functions run afoul of), as well as some standard libraries and tools.
There are several methods for obtaining these:

* [The Haskell Platform](http://hackage.haskell.org/platform/) has
everything you need in one convenient package. If you are unsure, you
should use this.

* If you already have GHC and/or know what you are doing and want to
install things yourself, just make sure you have
[GHC](http://haskell.org/ghc) and a recent version of the
[cabal-install
tool](http://hackage.haskell.org/trac/hackage/wiki/CabalInstall).

Cairo
-----

Unlike its previous incarnation, diagrams has been designed from the
ground up with modularity in mind, and makes it extremely easy to plug
in new rendering backends.  Eventually there will be many different
rendering backends; at the moment, however, the only seriously
supported one is [cairo](http://cairographics.org/).  If you don't
already have them, you will need to [install the cairo development
libraries](http://cairographics.org/download/).

If you are on Linux or MacOS, the Haskell bindings to cairo should be
automatically installed in the next step (Installation). If you are on
Windows, [here are some
instructions](http://code.google.com/p/diagrams/wiki/CairoOnWindows)
for installing the Haskell cairo bindings; unfortunately it is
something of a pain.

Installation
------------

Once you have the prerequisites, installing the diagrams libraries
themselves should be a snap:

    cabal install diagrams-core diagrams-lib diagrams-cairo

(Actually, in practice just `cabal install diagrams-cairo` is enough,
since the other two packages will be pulled in as dependencies.)

* `diagrams-core` contains the core data structures and definitions
  that form the abstract heart of the library.

* `diagrams-lib` is a standard library of drawing primitives,
  attributes, and combinators built on top of the core library.

* `diagrams-cairo` is a backend which renders diagrams using cairo.

Philosophy
==========

Before diving in to create some diagrams, it's worth taking a minute
to explain some of the philosophy that drove many of the design
decisions. (If you're impatient, feel free to skip this section for
now---but you might want to come back and read it later!)

* Positioning and scaling are always *relative*.  There is never any
global coordinate system to think about; everything is done relative
to diagrams' *local* vector spaces.  This is not only easier to think
about, it also increases modularity/compositionality, since diagrams
can always be designed without thought for the context in which they
will eventually be used.  Doing things this way is more work for the
*library* and less work for the *user*, which is the way it should be.

* 

Your first diagram
==================

Create a file called `DiagramsTutorial.lhs`
with the following contents (or you can simply edit this file itself):

> {-# LANGUAGE NoMonomorphismRestriction #-}
>
> import Diagrams.Prelude
> import Diagrams.Backend.Cairo.CmdLine
>
> main = defaultMain circle

Turning off the Dreaded Monomorphism Restriction is quite important:
if you don't, you will almost certainly run into it (and be very
confused by the resulting error messages).

The first `import` statement brings into scope the entire diagrams DSL
and standard library.  The second `import` is so that we can use the
cairo backend for rendering diagrams.  Among other things, it provides
the function `defaultMain`, which takes a diagram as input and creates
a command-line-driven application for rendering it.

Let's compile and run it:

    $ ghc --make DiagramsTutorial.lhs
    [1 of 1] Compiling Main             ( DiagramsTutorial.lhs, DiagramsTutorial.o )
    Linking DiagramsTutorial ...
    $ ./DiagramsTutorial -o circle.pdf

If you now view `circle.pdf` in your favorite PDF viewer, you should
see an unfilled black circle on a white background (actually, it's on
a transparent background, but most PDF viewers I know of use white).

A few things to note: in addition to PDF, the cairo backend can
generate `.png`, `.ps`, and `.svg` formats; the format to use is
determined automatically by the extension of the output file name.
There are several more options besides `-o`; you can see what they are
by typing `./DiagramsTutorial --help`.

Attributes
==========

Suppose we want our circle to be blue, with a thick dashed purple
outline (there's no accounting for taste).  We can apply attributes to
the `circle` diagram with the `(#)` operator:

> circle1 = circle # fc blue
>                  # lw 0.05
>                  # lc purple
>                  # dashing [0.2,0.05] 0

(To render this new diagram, just replace `defaultMain circle` with
`defaultMain circle1`.)

Note that the dashed purple border is cut off a bit at the edges of
the PDF. This is by design: diagrams' bounds are computed based on
their "abstract shapes", without taking into account how they are
actually drawn.  Future versions of diagrams may give you the option
of taking things such as thick borders into account when computing
boundaries.  For now, we can simply add a bit of "padding" to make the
whole drawing visible. 10% should do nicely:

> pCircle1 = circle1 # pad 1.1

There's actually nothing special about the `(#)` operator: it's just
reverse function application, that is,

~~~ {.haskell}
x # f = f x
~~~

Just to illustrate,

> pCircle1' = pad 1.1 . dashing [0.2,0.05] 0 . lc purple . lw 0.05 . fc blue $ circle

produces exactly the same diagram as `pCircle1`.  So why bother with `(#)`?
First, it's often more natural to write (and easier to read) what a
diagram *is* first, and what it is *like* second.  Second, `(#)` has a
high precedence (8), making it more convenient to combine diagrams
with specified attributes.  For example,

~~~ {.haskell}
circle # fc red # lw 0 ||| circle # fc green # lw 0
~~~

places a red circle with no border next to a green circle with no
border (we'll see more about the `(|||)` operator shortly). Without
`(#)` we would have to write something with more parentheses, like

~~~ {.haskell}
(fc red . lw 0 $ circle) ||| (fc green . lw 0 $ circle)
~~~

For information on other standard attributes, see
[Diagrams.Attributes](http://hackage.haskell.org/packages/archive/diagrams-lib/0.1/doc/html/Diagrams-Attributes.html).

Combining diagrams
==================

OK, so we can draw a single circle: boring!  Much of the power of the
diagrams framework, of course, comes from the ability to build up
complex diagrams by *combining* simpler ones.

Let's start with the most basic way of combining two diagrams:
superimposing one diagram on top of another.  We can accomplish this
with `atop`:

> circleSq = square # fc aqua `atop` pCircle1

(Incidentally, these colors are coming from [Data.Colour.Names](http://hackage.haskell.org/packages/archive/colour/2.3.1/doc/html/Data-Colour-Names.html).)

(Also, if you're wondering, the circle has a radius of 1, while the square
has sides of length 1.)

"Putting one thing on top of another" sounds rather vague: how do we
know exactly where the circle and square will end up relative to one
another?  To answer this question, we must introduce the fundamental
notion of a *local origin*.

Local origins
-------------

Every diagram has a distinguished point called its *local origin*.
Many operations on diagrams -- such as `atop` -- work somehow with
respect to the local origin.  `atop` in particular works by
superimposing two diagrams so that their local origins coincide (and
this point becomes the local origin of the new, combined diagram).

The `showOrigin` function is provided for conveniently visualizing the
local origin of a diagram.

> circleWithO = circle # showOrigin

Not surprisingly, the local origin of `circle` is at its center.  So
is the local origin of `square`.  This is why ``square `atop` circle``
produces a square centered on a circle.

> circleSqWithO = circleSq # showOrigin

Side-by-side
------------

Another fundamental way to combine two diagrams is by placing them
*next to* each other.  The `(|||)` and `(===)` operators let us
conveniently put two diagrams next to each other in the horizontal or
vertical directions, respectively.  For example:

> circleSqH = circle ||| square
>
> circleSqV = circle === square

The two diagrams are arranged next to each other so that their local
origins are on the same horizontal or vertical line.  As you can
ascertain for yourself with `showOrigin`, the local origin of the new,
combined diagram is at the point of tangency between the two
subdiagrams.

`(|||)` and `(===)` are actually just convenient specializations of
the more general `beside` combinator. `beside` takes as arguments a
*vector* and two diagrams, and places them next to each other "along
the vector" --- that is, in such a way that the vector points from the
local origin of the first diagram to the local origin of the second.

> circleSqV1 = beside (1,1) circle square
>
> circleSqV2 = beside (1,-2) circle square

Bounding functions
------------------

How does the diagrams library figure out how to place two diagrams
"next to" each other?  And what exactly does "next to" mean?  There
are many possible definitions of "next to" that one could imagine
choosing, with varying degrees of flexibility, simplicity, and
tractability.  The definition of "next to" adopted by diagrams is as follows:

To place two diagrams next to each other in the direction
of a vector *v*, place them as close as possible so that there is a
*separating line* perpendicular to *v*, that is, a line perpendicular
to *v* such that the first diagram lies completely on one side of the
line and the other diagrams lies completely on the other side.

There are certainly some tradeoffs in this choice. The biggest
downside is that adjacent diagrams sometimes end up with undesired
space in between them.  For example, the two rotated ellipses in the
diagram below have some space between them. (Try adding a vertical
line between them with `vrule` and you will see why.)

> e2 = ell ||| ell
>   where ell = circle # scaleX 0.5 # rotateBy (1/6)

If we want to position these ellipses next to each other horizontally
so that they are tangent, it is not clear how to accomplish this.
(However, it should be possible to create higher-level modules for
automatically accomplishing this in certain cases.)

However:

* This rule is very *simple*, in that it is easy to predict what will
  happen when placing two diagrams next to each other.

* It is also *tractable*.  Every diagram carries along with it a
*bounding function* which takes as input a vector *v*, and returns the
minimum distance to a separating line from the local origin in the
direction of *v*.  When composing two diagrams with 'atop' we take the
pointwise maximum of their bounding functions; to place two diagrams
next to each other we use their bounding functions to decide how to
reposition their local origins before composing them with 'atop'.

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

> circleRect = circle # scale 0.5 ||| square # scaleX 0.3
>
> circleRect2 = circle # scale 0.5 ||| square # scaleX 0.3 
>                                             # rotateBy (1/6) 
>                                             # scaleX 0.5

Freezing
--------

Note that the transformed circles and squares in the examples above
were all drawn with the same uniform lines.  This is because by
default, transformations operate on the abstract geometric ideal of a
diagram, and not on its attributes.  Often this is what you want; but
occasionally you want scaling a diagram to have an effect on the width
of its lines, and so on.  This can be accomplished with the `freeze`
combinator: whereas transformations normally do not affect a diagram's
attributes, transformations *do* affect the attributes of a frozen diagram.

Here is an example. On the left is an untransformed circle drawn with
a line 0.1 units thick.  The next circle is a scaled version of the
first: notice how the line thickness is the same.  The third circle
was produced by first freezing, then scaling the first circle,
resulting in a line twice as thick.  The last two circles illustrate a
non-uniform scale applied to an unfrozen circle (which is drawn with a
uniform line) and to a frozen one (in which the line gets thicker and
thinner according to the non-uniform scale).

> c = circle # lw 0.1
>
> circles = hcat' with {sep = 0.5} 
>           [ c 
>
>           , c # scale 2
>           , c # freeze # scale 2
>
>           , c # scaleX 0.2
>           , c # freeze # scaleX 0.2
>           ]
>           # centerXY
>           # pad 1.1

This example also illustrates the `hcat'` function, which takes a list
of diagrams and lays them out horizontally, here with a separation of
0.5 units between each one.  For more information on `hcat'` and
similar combinators, see the
[Diagrams.TwoD.Combinators](http://hackage.haskell.org/packages/archive/diagrams-lib/0.1/doc/html/Diagrams-TwoD-Combinators.html)
documentation.

Translation
-----------

Of course, there are also translation transformations like
`translate`, `translateX`, and `translateY`.  These operations
translate a diagram within its *local vector space* --- that is,
relative to their local origin.

> circleT = circle # translate (0.5, 0.3) # showOrigin

As `circleT` shows, translating a diagram by `(0.5, 0.3)` is the same
as moving its local origin by `(-0.5, -0.3)`.

Since diagrams are always composed with respect to their local
origins, translation can affect the way diagrams are composed.

> circleSqT   = square `atop` circle # translate (0.5, 0.3)
> circleSqHT  = square ||| circle # translate (0.5, 0.3)
> circleSqHT2 = square ||| circle # translate (19.5, 0.3)

As `circleSqHT` and `circleSqHT2` demonstrate, when we place a
translated circle next to a square, it doesn't matter how much the
circle was translated in the *horizontal* direction --- the square and
circle will always simply be placed next to each other.  The vertical
direction matters, though, since the local origins of the square and
circle are placed on the same horizontal line.

Aligning
--------

It's quite common to want to *align* some diagrams in a certain way
when placing them next to one another --- for example, we might want a
horizontal row of diagrams aligned along their top edges.  The
*alignment* of a diagram simply refers to its position relative to its
local origin, and convenient alignment functions are provided for
aligning a diagram with respect to its bounding region.  For example,
`alignT` translates a diagram in a vertical direction so that its
local origin ends up exactly on the edge of its bounding region.

> circlesTop = hrule (2 * sum sizes) # lw 0.1 === circles # centerX
>   where circles = hcat . map alignT . zipWith scale sizes 
>                 $ repeat (circle # lw 0.1)
>         sizes   = [2,5,4,7,1,3]

See [Diagrams.TwoD.Align](http://hackage.haskell.org/packages/archive/diagrams-lib/0.1/doc/html/Diagrams-TwoD-Align.html) for other alignment combinators.

Diagrams as a monoid
====================

As you may have already suspected if you are familiar with monoids,
diagrams form a monoid under `atop`.  The diagrams standard library
provides `(<>)` as a convenient synonym for `mappend`, so `(<>)` can
also be used to superimpose diagrams.  This also means that `mempty`
is available to construct the "empty diagram", which takes up no space
and produces no output.

Quite a few other things in the diagrams standard library are also
monoids (transformations, trails, paths, styles, and colors).

Next steps
==========

Eventually this tutorial will include additional material.  For now,
here are pointers to some resources for learning more:

* The diagrams-lib API is generally well-documented; start with the
documentation for
[Diagrams.Prelude](http://hackage.haskell.org/packages/archive/diagrams-lib/0.1/doc/html/Diagrams-Prelude.html),
and then drill down from there to learn about whatever you are
interested in.  If there is anything in the API documentation that you find
unclear or confusing, please
[http://code.google.com/p/diagrams/issues/list report it as a bug]!

* The `diagrams-cairo` package includes a number of examples.
Download the source tarball with `cabal unpack diagrams-cairo` and
look in the `examples/` directory.

* If you run into difficulty or have any questions, join the #diagrams
IRC channel on freenode.org, or the [diagrams-discuss mailing
list](http://groups.google.com/group/diagrams-discuss).