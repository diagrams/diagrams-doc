---
title: Diagrams manual
...

Preliminaries
=============

Introduction
------------

`diagrams` is a flexible, powerful embedded domain-specific language
(EDSL) for creating vector graphics and animations. The `diagrams`
framework is:

-   **Declarative**: you specify *what* a diagram is, not *how* to draw
    it. `diagrams` takes care of the how.
-   **Compositional**: diagrams can be easily *combined* in many ways to
    produce more complex diagrams.
-   **Embedded**: the full power of [Haskell](http://haskell.org/),
    including every library on [Hackage](http://hackage.haskell.org/),
    is available to help construct and manipulate graphics.
-   **Extensible**: extending diagrams with additional or higher-level
    functionality is as simple as writing a Haskell module.
-   **Flexible**: diagrams is designed from the ground up to be as
    generic and flexible as possible, with support for pluggable
    rendering backends, arbitrary graphics primitives, multiple numeric
    types, and multiple vector spaces (2D, 3D, ...).

About this document
-------------------

This document attempts to explain all major aspects of using the
`diagrams` core and standard libraries, organized by topic to make it
easy to find what you are looking for. It is not, however, a complete
reference of every single function in the standard library: for that,
see the API documentation listed under `Other resources`{.hs}\_. Most
sections contain links to relevant modules you can follow to read about
other functions not covered in the text.

Module names in the text are typeset like this:
`Diagrams.Prelude`{.mod}. Click on a module name to visit its
documentation. You can also click on any function or operator name in
code examples to take you to its documentation. Try it:

``` {.haskell}
example = circle 2 ||| pentagon 3
```

Mathematical equations are typeset using
[MathJax](http://www.mathjax.org/):

$\sum_{k=1}^\infty \frac{1}{k^2} = \frac{\pi^2}{6}$

Right-click on any equation to access MathJax options, like displaying
the LaTeX source, switching between MathML and HTML/CSS for display,
zoom settings, and so on.

Occasionally content may be missing or incomplete; this is noted by a
light blue box with a "document" icon on the right hand side, like this:

<div class="todo">

-   Explain zygohistomorphic prepromorphisms
-   Essay on postmodernist critiques of `diagrams` vis-a-vis Kant

</div>

If you see a box like this in the place of something you would really
like to know about, please bug the developers (using the `#diagrams` IRC
channel on Freenode, or the [diagrams mailing
list](http://groups.google.com/group/diagrams-discuss?pli=1)) so they
can prioritize it!

Warnings, "gotchas", and other important asides are in a yellow box with
a "warning" icon, like this:

<div class="warning">

Diagrams is extremely addictive and may be hazardous to your health!

</div>

You would do well to pay special attention to the contents of such
boxes.

Other resources
---------------

Here are some other resources that may be helpful to you as you learn
about `diagrams`:

-   The API reference documentation for all the `diagrams` packages is
    intended to be high-quality and up-to-date, and is available [from
    the diagrams
    website](http://projects.haskell.org/diagrams/reference.html). If
    you find an omission, error, or something confusing, please [report
    it as a bug](https://github.com/diagrams/diagrams-doc/issues)!
-   The `diagrams` [website](http://projects.haskell.org/diagrams) has a
    [gallery of
    examples](http://projects.haskell.org/diagrams/gallery.html) and a
    [list of
    tutorials](http://projects.haskell.org/diagrams/documentation.html),
    as well as links to blog posts and other documentation.
-   The [diagrams wiki](http://haskell.org/haskellwiki/Diagrams) is a
    good place to find tips and tricks, examples, answers to frequently
    asked questions, and more.
-   The `#diagrams` IRC channel on Freenode is a friendly place where
    you can get help from other `diagrams` users and developers.
-   Consider joining the [diagrams mailing
    list](http://groups.google.com/group/diagrams-discuss?pli=1) for
    discussions and announcements about `diagrams`.
-   See the issue trackers in the
    `diagrams organization on github`{.hs}\_ for a list of open tickets.
    If you find a bug or would like to request a feature, please file a
    ticket!

Installation
------------

Before installing `diagrams`, you will need the following:

-   The [Glasgow Haskell Compiler](http://www.haskell.org/ghc/) (GHC),
    version 7.4.x or later (7.8.3 is recommended).
-   It is recommended (but not required) to have the latest release of
    the [Haskell Platform](http://hackage.haskell.org/platform/)
    (currently 2014.2.0.0). At the very least you will want the
    [cabal-install](http://hackage.haskell.org/trac/hackage/wiki/CabalInstall)
    tool. Diagrams is always tested on at least two versions of the
    Haskell Platform (the current and previous releases), and may work
    on earlier HP releases as well.

If you are on OS X or Windows, GHC itself comes with the Haskell
Platform; if you are on Linux, you will have to install GHC first.

Once you have successfully installed the Haskell platform, installing
`diagrams` should be as easy as issuing the command:

    cabal install diagrams -jN

where `N` is the number of cores you wish to use for compilation.

The `diagrams`{.pkg} package is a convenience wrapper that simply pulls
in (by default) four other packages:

-   `diagrams-core`{.pkg} (core data type definitions and utilities),
-   `diagrams-lib`{.pkg} (standard primitives and combinators),
-   `diagrams-contrib`{.pkg} (user-contributed extensions), and
-   `diagrams-svg`{.pkg} (Haskell-native backend generating SVG files).

There is also a Haskell-native [postscript
backend](http://hackage.haskell.org/package/diagrams-postscript/), which
supports all features except transparency, and a Haskell-native [raster
backend](http://hackage.haskell.org/package/diagrams-rasterific/) (based
on the excellent
[Rasterific](http://hackage.haskell.org/package/Rasterific/) package).
To get them, add the `-fps` or `-frasterific` flags, respectively:

    cabal install -fps diagrams
      OR
    cabal install -frasterific diagrams

There is also a backend based on the [cairo graphics
library](http://www.cairographics.org/); it has support for more
features than the SVG backend and additional output formats (PNG, PS,
PDF), but can be much more difficult to install on some platforms
(notably OS X). If you want the cairo backend, you can issue the command

    cabal install gtk2hs-buildtools
    cabal install -fcairo diagrams

(You can omit `gtk2hs-buildtools` if you have already installed it
previously, though note that you may need to reinstall it if you are
building under GHC 7.6 and the last time you installed
`gtk2hs-buildtools` was sufficiently long ago---otherwise you may get
FFI-related errors when building the `cairo`{.pkg} package.) Add `-fgtk`
to also get a GTK backend (based on the cairo backend) which can render
diagrams directly to GTK windows.

You can also mix and match all the above flags to get multiple backends.
Note, if you don't want the SVG backend at all, you must add the
`-f-svg` flag to disable it.

[See the wiki for the most up-to-date
information](http://www.haskell.org/haskellwiki/Diagrams/Install)
regarding installation. If you have trouble installing diagrams, feel
free to send email to the [diagrams mailing
list](http://groups.google.com/group/diagrams-discuss?pli=1); we would
like to collect reports of problems and solutions on various platforms.

Getting started
---------------

Create a file called `TestDiagram.hs` (or whatever you like) with the
following contents:

``` {.haskell}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
-- or:
-- import Diagrams.Backend.Cairo.CmdLine
-- or:
-- import Diagrams.Backend.Postscript.CmdLine
-- or:
-- import Diagrams.Backend.Rasterific.CmdLine

myCircle :: Diagram B
myCircle = circle 1

main = mainWith myCircle
```

The first line turns off the [dreaded monomorphism
restriction](http://www.haskell.org/haskellwiki/Monomorphism_restriction),
which is quite important when using `diagrams`: otherwise you will
probably run into lots of crazy error messages.

`Diagrams.Prelude`{.mod} re-exports almost everything from the standard
library; `Diagrams.Backend.SVG.CmdLine`{.mod} provides a command-line
interface to the SVG rendering backend. We then declare `myCircle`{.hs}
to have the type `Diagram B`{.hs} the `B`{.hs} is an alias representing
the particular backend. This also fixes the vector space and numerical
field for the diagram. All the backends export `B`{.hs} as an alias for
themselves, so you can switch backends just by changing an import,
without having to change type annotations on your diagrams; `B`{.hs}
simply refers to whichever backend is in scope. Finally, `mainWith`{.hs}
takes a diagram and creates a command-line-driven executable for
rendering it.

To compile your program, type

    $ ghc --make TestDiagram

(Note that the `$` indicates a command prompt and should not actually be
typed.) Then execute `TestDiagram` with some appropriate options:

    $ ./TestDiagram -w 100 -h 100 -o TestDiagram.svg

The above will generate a 100x100 SVG that should look like this:

``` {.diagram}
example = circle 1
```

If you are using the cairo backend you can also request a `.png`, `.ps`,
or `.pdf` file (the format is automatically determined by the
extension), or an `.eps` file if using the postscript backend. The
rasterific backend allows `.png`, `.jpg`, `.tif`, and `.bmp`.

Try typing

    $ ./TestDiagram --help

to see the other options that are supported.

To get started quickly, you may wish to continue by reading the [quick
start tutorial](/doc/quickstart.html); or you can continue reading the
rest of this user manual.

Note that `Diagrams.Backend.SVG.CmdLine`{.hs} is provided for
convenience; it's not the only interface to the backend though. If you
want to roll your own code, e.g. as a component of another program, use
the `renderDia`{.hs} function, or see the related section under
`Rendering
backends`{.hs}\_ for additional backend specific entry points.

Contributing
------------

`diagrams` is an open-source project, and contributions are encouraged!
All diagrams-related repositories are in the [diagrams
organization](http://github.com/diagrams) on github. The [Contributing
page](http://www.haskell.org/haskellwiki/Diagrams/Contributing) on the
diagrams wiki explains how to get the repositories and make
contributions. To find out about the latest developments, join the
`#diagrams` IRC channel on Freenode, and check out the [diagrams Trello
board](https://trello.com/b/pL6YdKgz/diagrams).

Essential concepts
==================

Before we jump into the main content of the manual, this chapter
explains a number of general ideas and central concepts that will recur
throughought. If you're eager to skip right to the good stuff, feel free
to skip this section at first, and come back to it when necessary; there
are many links to this chapter from elsewhere in the manual.

Semigroups and monoids
----------------------

A *semigroup* consists of

-   A set of elements $S$
-   An *associative binary operation* on the set, that is, some
    operation

    $\oplus \colon S \to S \to S$

    for which

    $(x \oplus y) \oplus z = x \oplus (y \oplus z).$

A *monoid* is a semigroup with the addition of

-   An *identity element* $i \in S$ which is the identity for $\oplus$,
    that is,

    $x \oplus i = i \oplus x = x.$

In Haskell, semigroups are expressed using the `Semigroup`{.hs} type
class from the `semigroups`{.pkg} package:

``` {.haskell}
class Semigroup s where
  (<>) :: s -> s -> s
```

and monoids are expressed using the `Monoid`{.hs} type class, defined in
`Data.Monoid`:

``` {.haskell}
class Monoid m where
  mempty  :: m
  mappend :: m -> m -> m
```

The `mappend`{.hs} function represents the associative binary operation,
and `mempty`{.hs} is the identity element. (`mappend`{.hs} and
`(<>)`{.hs} should always be the same; there are two different functions
for historical reasons.) A function

``` {.haskell}
mconcat :: Monoid m => [m] -> m
```

is also provided as a shorthand for the common operation of combining a
whole list of elements with `(<>)`{.hs}/`mappend`{.hs}.

Semigroups and monoids are used extensively in `diagrams`: diagrams,
transformations, envelopes, traces, trails, paths, styles, colors, and
queries are all instances of both `Semigroup`{.hs} and `Monoid`{.hs}.

Faking optional named arguments
-------------------------------

Many diagram-related operations can be customized in a wide variety of
ways. For example, when creating a regular polygon, one can customize
the number of sides, the radius, the orientation, and so on. However, to
have a single function that takes all of these options as separate
arguments would be a real pain: it's hard to remember what the arguments
are and what order they should go in, and often one wants to use default
values for many of the options and only override a few. Some languages
(such as Python) support *optional, named* function arguments, which are
ideal for this sort of situation. Sadly, Haskell does not. However, we
can fake it!

Any function which should take some optional, named arguments instead
takes a single argument which is a record of options. The record type is
declared to be an instance of the `Default`{.hs} type class:

``` {.haskell}
class Default d where
  def :: d
```

That is, types which have a `Default`{.hs} instance have some default
value called `def`{.hs}. For option records, `def`{.hs} is declared to
be the record containing all the default arguments. The idea is that you
can pass `def`{.hs} as an argument to a function which takes a record of
options, and override only the fields you want, like this:

``` {.haskell}
foo (def & arg1 .~ someValue & arg6 .~ blah)
```

This is using machinery from the `lens`{.pkg} package; but you don't
have to understand `lens`{.pkg}, or know anything beyond the above
syntax in order to use diagrams (for convenience, diagrams re-exports
the `(&)`{.hs} and `(.~)`{.hs} operators from `lens`{.pkg}). In fact, in
most cases, you can also use record update syntax instead (note the
underscores):

    foo (def { _arg1 = someValue, _arg6 = blah })

In some cases, however, the lens library is used to provide convenient
"virtual" fields which do not correspond to real record fields; for
example, `headColor`{.hs} can be used to set the color of an arrowhead,
even though the arrow options record actually contains a general style
instead of just a color.

Finally, note that `diagrams` also defines `with`{.hs} as a synonym for
`def`{.hs}, which can read a bit more nicely. So, instead of the above,
you could write

    foo (with & arg1 .~ someValue & arg6 .~ blah)

Most functions which take an optional arguments record have two
variants: one named `foo`{.hs} which uses all default arguments, and one
named `foo'`{.hs} (with a trailing prime) which takes an options record.

Vectors and points
------------------

Although much of this user manual focuses on constructing
two-dimensional diagrams, the definitions in the core library in fact
work for *any* vector space. Vector spaces are defined in the\`
Linear.Vector\`:mod: module from Edward Kmett's `linear`{.pkg} package.

Many objects (diagrams, paths, backends...) inherently live in some
particular vector space. The vector space in which a given type "lives"
can be computed by the type function `Vn`{.hs}. So, for example, the
type

    Foo d => Vn d -> d -> d

is the type of a two-argument function whose first argument is a vector
in whatever vector space corresponds to the type `d`{.hs} (which must be
an instance of `Foo`{.hs}).

Each vector space has a *dimension* and a type of *scalars*. The type
`V2 Double`{.hs} specifies that the dimension is 2 and the scalar type
is `Double`{.hs} (64-bit floating point values). A vector represents a
direction and magnitude, whereas a scalar represents only a magnitude.
Useful operations on vectors and scalars include:

-   Adding and subtracting vectors with `(^+^)`{.hs} and `(^-^)`{.hs}
-   Multiplying a vector by a scalar with `(*^)`{.hs}
-   Linearly interpolating between two vectors with `lerp`{.hs}
-   Finding the `norm`{.hs} (length) of a vector
-   Projecting one vector onto another with `project`{.hs}.

Functions and types which are parametric in the vector space have two
type parameters, `v`{.hs} representing the dimension and `n`{.hs} the
scalar type. Occasionally `v`{.hs} or `n`{.hs} appears alone in a type
signature, with the same meaning. `n`{.hs} is most commonly
`Double`{.hs}, or some other type approximating the real numbers, but
this is not required. Many functions require than `n`{.hs} be an
instance of `Num`{.hs}, or one of the narrower classes
`Fractional`{.hs}, `Floating`{.hs}, or `Real`{.hs}.

See [this tutorial for a more in-depth introduction to working with
vectors and points](http://tauday.com). ] \_\_ vector.html

One might think we could also identify *points* in a space with vectors
having one end at the origin. However, this turns out to be a poor idea.
There is a very important difference between vectors and points: namely,
vectors are translationally invariant whereas points are not. A vector
represents a direction and magnitude, not a location. Translating a
vector has no effect. Points, on the other hand, represent a specific
location. Translating a point results in a different point.

Although it is a bad idea to *conflate* vectors and points, we can
certainly *represent* points using vectors. The `linear`{.pkg} package
defines a newtype wrapper around vectors called `Point`{.hs}. The most
important connection between points and vectors is given by
`(.-.)`{.hs}, defined in `Linear.Affine`{.mod}. If `p`{.hs} and `q`{.hs}
are points, `p .-. q`{.hs} is the vector giving the direction and
distance from `p`{.hs} to `q`{.hs}. Offsetting a point by a vector
(resulting in a new point) is accomplished with `(.+^)`{.hs}.

Envelopes and local vector spaces
---------------------------------

In order to be able to position diagrams relative to one another, each
diagram must keep track of some bounds information. Rather than use a
bounding box (which is neither general nor compositional) or even a more
general bounding *path* (which is rather complicated to deal with), each
diagram has an associated bounding *function*, called the *envelope*.
Given some direction (represented by a vector) as input, the envelope
answers the question: "how far in this direction must one go before
reaching a perpendicular (hyper)plane that completely encloses the
diagram on one side of it?"

That's a bit of a mouthful, so hopefully the below illustration will
help clarify things if you found the above description confusing. (For
completeness, the code used to generate the illustration is included,
although you certainly aren't expected to understand it yet if you are
just reading this manual for the first time!)

``` {.diagram-code}
illustrateEnvelope v d
  = mconcat
    [arrowAt' (with & arrowHead .~ tri) origin v
    , origin ~~ b
      # lc green # lw veryThick
    , p1 ~~ p2
      # lc red
    ]
    where
      b  = envelopeP v d
      v' = signorm v
      p1 = b .+^ (rotateBy (1/4) v')
      p2 = b .+^ (rotateBy (-1/4) v')

d1 :: Path V2 Double
d1 = circle 1

d2 :: Path V2 Double
d2 = (pentagon 1 === roundedRect 1.5 0.7 0.3)

example = (strokePath d1 # showOrigin <> illustrateEnvelope (r2 (-0.5,0.3)) d1)
      ||| (strokePath d2 # showOrigin <> illustrateEnvelope (r2 (0.5, 0.2)) d2)
```

The black arrows represent inputs to the envelopes for the two diagrams;
the envelopes' outputs are the distances represented by the thick green
lines. The red lines illustrate the enclosing (hyper)planes (which are
really to be thought of as extending infinitely to either side): notice
how they are as close as possible to the diagrams without intersecting
them at all.

Of course, the *base point* from which the envelope is measuring matters
quite a lot! If there were no base point, questions of the form "*how
far do you have to go...*" would be meaningless---how far *from where*?
This base point (indicated by the red dots in the diagram above) is
called the *local origin* of a diagram. Every diagram has its own
intrinsic *local vector space*; operations on diagrams are always with
respect to their local origin, and you can affect the way diagrams are
combined with one another by moving their local origins. The
`showOrigin`{.hs} function is provided as a quick way of visualizing the
local origin of a diagram (also illustrated above).

Measurement units
-----------------

Certain attributes (such as line width, dashing size, arrowhead size,
and font size) can be specified with respect to several different
reference frames. For example, the lines used to draw a certain square
can be specified as an absolute two pixels wide, or as a certain
percentage of the size of the final diagram, or in units relative to the
size of the square. More specifically, values of type `Measure n`{.hs}
represent `n`{.hs} values, interpreted in one of four "reference
frames": `local`{.hs}, `global`{.hs}, `normalized`{.hs}, or
`output`{.hs}, described below in turn.

In addition to the four reference frames described here, it is possible
to combine them into more complex expressions using a small DSL for
specifying measurements; see `Measurement expressions`{.hs}\_.

### Local units

`local`{.hs} units are the most straightforward to explain. Values in
`local`{.hs} units are interpreted in the context of the *local* vector
space, just as most other length measurements (*e.g.* arguments to
functions like `circle`{.hs} and `square`{.hs}). For example,
`square 1 # lwL
0.2`{.hs} specifies a square which is drawn with lines one fifth as wide
as its sides are long---and will *always* be, even if it is scaled: the
line width scales right along with the square. (Note that `lwL`{.hs}
specifies the line width using `local`{.hs} units, and is a synonym for
`lw
. local`{.hs}.),

``` {.diagram-code}
localSq = square 1 # lwL 0.2
example =
  hcat' (with & sep .~ 0.5)
  [localSq, localSq # scale 2, localSq # scaleX 2]
```

It's important to note that---as illustrated by the third figure in the
above picture---line width always scales uniformly, even when a
non-uniform scaling is applied. Previous versions of diagrams had a
`freeze`{.hs} operation which could be used to apply non-uniform scaling
to lines; to achieve such an effect, you can first turn a stroked line
into a closed path, as described in `Offsets of segments, trails, and
paths`{.hs}\_.

A important consequence of `local`{.hs} units having the *current*
vector space as their reference is that attribute-setting functions such
as `lwL`{.hs} do *not* commute with transformations.

``` {.diagram-code}
example =
  hcat' (with & sep .~ 0.5)
  [ square 1 # lwL 0.2 # scale 2
  , square 1 # scale 2 # lwL 0.2
  ]
  # frame 0.5
```

### Global units

Whereas `local`{.hs} values are interpreted in the current, "local"
vector space, `global`{.hs} values are interpreted in the final,
"global" vector space of the diagram that is rendered. In the following
example, `theSq`{.hs} is specified as having a `global`{.hs} line width
of `1`{.hs}; five differently-scaled copies of the square are laid out,
so that the entire scaled diagram has a width of around `6`{.hs} units.
The lines, having a line width of `global 0.05`{.hs}, are thus about
0.8% of the width of the entire diagram.

``` {.diagram-code}
theSq = square 1 # lwG 0.05

example =
  hcat' (with & sep .~ 0.2)
    (map (\s -> theSq # scale s) [0.5, 0.8, 1, 1.5, 2])
```

Versions of `diagrams` prior to `1.2`{.hs} actually had a semantics for
`lw`{.hs} equivalent to `lwG`{.hs}. One advantage, as can be seen from
the above example, is that different shapes having the same
`global`{.hs} line width, even when differently scaled, will all be
drawn with the same apparent line width. However, `normalized`{.hs} and
`output`{.hs} have that property as well, and are probably more useful;
the problem with `global`{.hs} units is that in order to decide on
values, one has to know the final size of the diagram, which is not
typically something one knows in advance. In particular, note that
applying something like `scale 20`{.hs} to the `example`{.hs} above---a
seemingly innocuous change---would result in extremely thin lines (or
even invisible, depending on the backend), as shown below. Making this
look reasonable again would require changing the argument to `lwG`{.hs}.

``` {.diagram-code}
theSq = square 1 # lwG 0.05

example =
  hcat' (with & sep .~ 0.2)
    (map (\s -> theSq # scale s) [0.5, 0.8, 1, 1.5, 2])
  # scale 20
```

In short, `global`{.hs} units tend to go against `diagrams` emphasis on
local, scale-invariant thinking. They were left in for backwards
compatibility, and because they can occasionaly be useful in special
situations where you do already have some absolute, global coordinate
system in mind: for example, if you know you want to construct a 100x100
diagram using lines that are 1 unit wide.

### Normalized units

`normalized`{.hs} units, like `global`{.hs} units, are measured with
respect to the final size of a diagram. However, for the purposes of
interpreting `normalized`{.hs} units, the diagram is considered to be
one "normalized unit" in both height and width. For example, a
`normalized`{.hs} value of `0.1`{.hs} means "10% of the height/width of
the final diagram". Thus, scaling the diagram has no effect on the
relative size of the lines (just as with `local`{.hs}), but lines look
consistent even across shapes that have been scaled differently (as with
`global`{.hs}).

``` {.diagram-code}
theSq = square 1 # lwN 0.01

example =
  hcat' (with & sep .~ 0.2)
    (map (\s -> theSq # scale s) [0.5, 0.8, 1, 1.5, 2])
  # scale 20
```

Note that the `scale 20`{.hs} threatened in the `global`{.hs} example
has been applied here, but makes no difference: changing the `20`{.hs}
to any other nonzero value has no effect on the appearance of the
rendered diagram.

### Output units

Values measured in `output`{.hs} units are interpreted with respect to
the *requested output size* of a diagram. Sometimes you really do know
that you want your lines to be exactly 1/2 inch wide when printed. In
this case, scaling a diagram will preserve its appearance, but
requesting a different output size might not.

One situation in which `output`{.hs} units can be particularly useful is
when preparing a document (paper, blog post, *etc.*) with multiple
embedded diagrams of various physical sizes. Using the same
`output`{.hs} value for the line width (or arrowhead length, arrow gap,
font size, *etc.*) of every diagram ensures that the diagrams will all
look consistent. On the other hand, if the diagrams all have the same
physical size (*e.g.* they are all $300 \times 200$ pixels), then they
will also look consistent if the same `normalized`{.hs} value is used
for all of them (which is the default for line width).

<div class="todo">

Expand on this. Show some examples. Need a better story about physical
units.

</div>

Postfix transformation
----------------------

You will often see idiomatic `diagrams` code that looks like this:

    foobar # attr1
           # attr2
           # attr3
           # transform1

There is nothing magical about `(#)`{.hs}, and it is not required in
order to apply attributes or transformations. In fact, it is nothing
more than reverse function application with a high precedence (namely,
8):

    x # f = f x

`(#)`{.hs} is provided simply because it often reads better to first
write down what a diagram *is*, and then afterwards write down
attributes and modifications. Additionally, `(#)`{.hs} has a high
precedence so it can be used to make "local" modifications without
requiring lots of parentheses:

``` {.haskell}
example =     square 2 # fc red # rotateBy (1/3)
          ||| circle 1 # lc blue # fc green
```

Note how the modifiers `fc red`{.hs} and `rotateBy (1/3)`{.hs} apply
only to the square, and `lc blue`{.hs} and `fc green`{.hs} only to the
circle (`(|||)`{.hs} has a precedence of 6, lower than that of
`(#)`{.hs}).

Types and type classes
----------------------

*Flexibility*, *power*, *simplicity*: in general, you can have any two
of these but not all three. Diagrams chooses *flexibility* and *power*,
at the expense of *simplicity*. (In comparison, the excellent
`gloss`{.pkg} library instead chooses *flexibility* and *simplicity*.)
In particular, the types in the diagrams library can be quite
intimidating at first. For example, `hcat`{.hs} is a function which
takes a list of diagrams and lays them out in a horizontal row. So one
might expect its type to be something like `[Diagram] -> Diagram`{.hs}.
In actuality, its type is

``` {.haskell}
hcat :: (Juxtaposable a, HasOrigin a, Monoid' a, V a ~ V2, N a ~ n, TypeableFloat n)
   => [a] -> a
```

which may indeed be intimidating at first glance, and at any rate takes
a bit of time and practice to understand! The essential idea is to
realize that `hcat`{.hs} is actually quite a bit more general than
previously described: it can lay out not just diagrams, but any
two-dimensional things (`V a ~ V2` and the constrants on `N a`) which
can be positioned "next to" one another (`Juxtaposable`{.hs}), can be
translated (`HasOrigin`{.hs}), and are an instance of `Monoid`{.hs}
(`Monoid'`{.hs} is actually a synonym for the combination of
`Monoid`{.hs} and `Semigroup`{.hs}). This certainly includes diagrams,
but it also includes other things like paths, envelopes, animations, and
even tuples, lists, sets, or maps containing any of these things.

At first, you may want to just try working through some examples
intuitively, without worrying too much about the types involved.
However, at some point you will of course want to dig deeper into
understanding the types, either to understand an error message (though
for help interpreting some common error messages, see `Deciphering
error messages`{.hs}\_) or to wield diagrams like a true type ninja.
When that point comes, you should refer to
`Understanding diagrams types`{.hs}\_ and the
`Type class reference`{.hs}\_.

Creating 2D diagrams
====================

The main purpose of `diagrams` is to construct two-dimensional vector
graphics (although it can be used for more general purposes as well).
This section explains the building blocks provided by
`diagrams-core`{.pkg} and `diagrams-lib`{.pkg} for constructing
two-dimensional diagrams.

All 2D-specific things can be found in `Diagrams.TwoD`{.mod}, which
re-exports most of the contents of `Diagrams.TwoD.*` modules. This
section also covers many things which are not specific to two
dimensions; later sections will make clear which are which.

Basic 2D types
--------------

`Diagrams.TwoD.Types`{.mod} defines types for working with
two-dimensional Euclidean space.

### Euclidean 2-space

There are three main type synonyms defined for referring to
two-dimensional space:

-   `V2 n`{.hs} is the type of the two-dimensional Euclidean vector
    space (`n`{.hs} is usually `Double`{.hs}). Standard `diagrams`
    backends render images with the positive $x$-axis extending to the
    right, and the positive $y$-axis extending *upwards*. This is
    consistent with standard mathematical practice, but upside-down with
    respect to many common graphics systems. This is intentional: the
    goal is to provide an elegant interface which is abstracted as much
    as possible from implementation details.

    `unitX`{.hs} and `unitY`{.hs} are unit vectors in the positive $x$-
    and $y$-directions, respectively. Their negated counterparts are
    `unit_X`{.hs} and `unit_Y`{.hs}.

    Vectors of type `V2 Double`{.hs} can be created by passing a pair of
    type `(Double, Double)`{.hs} to the function `r2`{.hs}; vectors can
    likewise be converted back into pairs using `unr2`{.hs}.

    Vectors can also be constructed and pattern-matched using the
    utilities defined in `Diagrams.Coordinates`{.mod}, which provides a
    uniform interface for constructing points and vectors of any
    dimension. Vectors can be created using the syntax `(x ^& y)`{.hs}
    and pattern-matched by calling `coords`{.hs} and then matching on
    the pattern `(x :& y)`{.hs}.

    For more in-depth information on working with `V2 Double`{.hs}, [see
    this tutorial](`2D%20Transformations`_).

    \_\_ vector.html

-   `P2 n`{.hs} is the type of points in two-dimensional space. It is a
    synonym for `Point V2 n`{.hs}. The distinction between points and
    vectors is important; see `Vectors and points`{.hs}\_.

    Points can be created from pairs of coordinates using `p2`{.hs} and
    converted back using `unp2`{.hs}. They can also be constructed and
    destructed using the same syntax as for vectors, as defined in
    `Diagrams.Coordinates`{.mod}.

    For more in-depth information on working with `P2`{.hs}, [see this
    tutorial](`Angles`_).

    \_\_ vector.html

-   `T2 n`{.hs} is the type of two-dimensional affine transformations.
    It is a synonym for `Transformation V2 n`{.hs}.

### Angles

The type `Angle n`{.hs} represents two-dimensional angles. Angles can be
expressed in radians, degrees, or fractions of a circle. Isomorphisms
`turn`{.hs}, `rad`{.hs}, and `deg`{.hs} are provided (represented using
the `Iso`{.hs} type from the `lens`{.pkg} package), which convert
between abstract `Angle n`{.hs} values and `n`{.hs} values with various
units. To construct an `Angle`{.hs}, use the `(@@)`{.hs} operator, as in
`(3 @@ deg)`{.hs} or `(3 @@
rad)`{.hs}. To project an `Angle`{.hs} back to a scalar, use the
`(^.)`{.hs} operator, as in `someAngle ^. rad`{.hs}.

-   `turn`{.hs} represents fractions of a circle. A value of
    `1 @@ turn`{.hs} represents a full turn, `1/4 @@ turn`{.hs}
    constructs a right angle, and so on. The measure of an Angle `a` in
    turns (represented with `Double`{.hs}) can be obtained using
    `a ^. turn`{.hs}.
-   `rad`{.hs} represents angles measured in radians. A value of
    `tau`{.hs} (that is, $\tau = 2 \pi$) represents a full turn. (If you
    haven't heard of $\tau$, see [The Tau Manifesto](core.html).)
-   `deg`{.hs} represents angles measured in degrees. A value of
    `360`{.hs} represents a full turn.

`fullTurn :: Angle`{.hs} represents one full turn, equivalent to `1 @@
turn`{.hs}, `tau @@ rad`{.hs}, or `360 @@ deg`{.hs}.

In two dimensions, the direction of a vector can be represented by an
angle measured counterclockwise from the positive $x$-axis (shown in
green below). For some vector u, this angle can be found by
`u ^. _theta`{.hs}.

``` {.diagram}
example = mconcat
  [ exampleVector
  , angleArrow
  , axes
  ]
  # (<> rect 12 6 # alignB # lw none)
  # center # frame 1

axes = (arrowV (6 *^ unitX) # centerX <> arrowV (6 *^ unitY) # centerY)
theAngle = 200 @@ deg
theV = 3 *^ rotate theAngle unitX
exampleVector = arrowV theV
  # lc blue
angleArrow = arrowBetween' (with & arrowShaft .~ arc xDir theAngle)
  (origin .+^ (1 *^ unitX))
  (origin .+^ (theV # signorm))
  # dashingG [0.05,0.05] 0
  # lc green
```

### Directions

Whereas a vector is described by a direction and a magnitude, some
functions only depend on the direction. The `Direction`{.hs} type is
used in these cases to make the relationship clear. The `direction`{.hs}
function converts a vector to its `Direction`{.hs}; `fromDirection`{.hs}
creates a unit (length 1) vector in the given direction.

Primitive shapes
----------------

`diagrams-lib`{.pkg} provides many standard two-dimensional shapes for
use in constructing diagrams.

### Circles and ellipses

Circles can be created with the `unitCircle`{.hs} and `circle`{.hs}
functions, defined in `Diagrams.TwoD.Ellipse`{.mod}.

For example,

``` {.diagram-code}
example = circle 0.5 <> unitCircle
```

`unitCircle`{.hs} creates a circle of radius 1 centered at the origin;
`circle`{.hs} takes the desired radius as an argument.

Every ellipse is the image of the unit circle under some affine
transformation, so ellipses can be created by appropriately [scaling and
rotating](core.html) circles.

``` {.diagram-code}
example = unitCircle # scaleX 0.5 # rotateBy (1/6)
```

For convenience the standard library also provides `ellipse`{.hs}, for
creating an ellipse with a given eccentricity, and `ellipseXY`{.hs}, for
creating an axis-aligned ellipse with specified radii in the x and y
directions.

### Arcs

`Diagrams.TwoD.Arc`{.mod} provides a function `arc`{.hs}, which
constructs a radius-one circular arc starting at a first direction and
extending through a given [angle](core.html) , as well as `wedge`{.hs}
which constructs a wedge shape, `annularWedge`{.hs} (an arc plus two
radii) and various other functions for conveniently constructing arcs.

``` {.diagram-code}
example = hcat' (with & sep .~ 0.5) [arc d a, wedge 1 d a, annularWedge 1 0.6 d a]
  where
    d = rotateBy (1/4) xDir
    a = 4 * tau / 7 - tau / 4 @@ rad
```

### Pre-defined shapes

`Diagrams.TwoD.Shapes`{.mod} provides a number of pre-defined polygons
and other path-based shapes. For example:

-   `triangle`{.hs} constructs an equilateral triangle with sides of a
    given length.
-   `square`{.hs} constructs a square with a given side length;
    `unitSquare`{.hs} constructs a square with sides of length `1`{.hs}.
-   `pentagon`{.hs}, `hexagon`{.hs}, ..., `dodecagon`{.hs} construct
    other regular polygons with sides of a given length. (For
    constructing polygons with a given *radius*, see
    `General polygons`{.hs}\_.)
-   In general, `regPoly`{.hs} constructs a regular polygon with any
    number of sides.
-   `rect`{.hs} constructs a rectangle of a given width and height.
-   `roundedRect`{.hs} constructs a rectangle with circular rounded
    corners.
-   `roundedRect'`{.hs} works like `roundedRect`{.hs} but allowing a
    different radius to be set for each corner, using
    `RoundedRectOpts`{.hs}.

``` {.diagram-code}
example = square 1
      ||| rect 0.3 0.5
      ||| triangle 1
      ||| roundedRect  0.5 0.4 0.1
      ||| roundedRect  0.5 0.4 (-0.1)
      ||| roundedRect' 0.7 0.4 (with & radiusTL .~ 0.2
                                     & radiusTR .~ -0.2
                                     & radiusBR .~ 0.1)
```

Completing the hodgepodge in `Diagrams.TwoD.Shapes`{.mod} for now, the
functions `hrule`{.hs} and `vrule`{.hs} create horizontal and vertical
lines, respectively.

``` {.diagram-code}
example = c ||| hrule 1 ||| c
  where c = circle 1 <> vrule 2
```

### General polygons

The `polygon`{.hs} function from `Diagrams.TwoD.Polygons`{.mod} can be
used to construct a wide variety of polygons. Its argument is a record
of optional parameters that control the generated polygon:

-   `polyType`{.hs} specifies one of several methods for determining the
    vertices of the polygon:
    -   `PolyRegular`{.hs} indicates a regular polygon with a certain
        number of sides and a given *radius*.

        ``` {.diagram-code}
        example = strutX 1 ||| p 6 ||| p 24 ||| strutX 1
          where p n = polygon (with
                        & polyType .~ PolyRegular n 1 )
        ```

    -   `PolySides`{.hs} specifies the vertices using a list of external
        angles between edges, and a list of edge lengths. More
        precisely, the first edge length is between the first and second
        vertex, while the first external angle is between the first and
        second edge. In the example below, the first vertex is on the
        bottom right.

        ``` {.diagram-code}
        example = polygon ( with
          & polyType .~ PolySides
              [ 20 @@ deg, 90 @@ deg, 40 @@ deg, 100 @@ deg ]
              [ 1        , 5        , 2        , 4          ]
          )
        ```

    -   `PolyPolar`{.hs} specifies the vertices using polar coordinates:
        a list of central angles between vertices, and a list of vertex
        radii.

-   `polyOrient`{.hs} specifies the \`PolyOrientation\`: the polygon can
    be oriented with an edge parallel to the $x$-axis. with an edge
    parallel to the $y$-axis, or with an edge perpendicular to any given
    vector. You may also specify that no special orientation should be
    applied, in which case the first vertex of the polygon will be
    located along the positive $x$-axis.
-   Additionally, a center other than the origin can be specified using
    `polyCenter`{.hs}.

``` {.diagram-code}
poly1 = polygon ( with & polyType  .~ PolyRegular 13 5
                       & polyOrient .~ OrientV )
poly2 = polygon ( with & polyType  .~ PolyPolar (repeat (1/40 @@ turn))
                                                (take 40 $ cycle [2,7,4,6]) )
example = (poly1 ||| strutX 1 ||| poly2)
```

Notice the idiom of using `with`{.hs} to construct a record of default
options and selectively overriding particular options by name.
`with`{.hs} is a synonym for `def`{.hs} from the type class
`Default`{.hs}, which specifies a default value for types which are
instances. You can read more about this idiom in the section
`Faking optional named arguments`{.hs}\_.

### Star polygons

A "star polygon" is a polygon where the edges do not connect consecutive
vertices; for example:

``` {.diagram-code}
example = star (StarSkip 3) (regPoly 13 1) # stroke
```

`Diagrams.TwoD.Polygons`{.mod} provides the `star`{.hs} function for
creating star polygons of this sort, although it is actually quite a bit
more general.

As its second argument, `star`{.hs} expects a list of points. One way to
generate a list of points is with polygon-generating functions such as
`polygon`{.hs} or `regPoly`{.hs}, or indeed, any function which can
output any `TrailLike`{.hs} type (see the section about
`TrailLike`{.hs}\_), since a list of points is an instance of the
`TrailLike`{.hs} class. But of course, you are free to construct the
list of points using whatever method you like.

As its first argument, `star`{.hs} takes a value of type
`StarOpts`{.hs}, for which there are two possibilities:

-   `StarSkip`{.hs} specifies that every $n$ th vertex should be
    connected by an edge.

    ``` {.diagram-code}
    example = stroke (star (StarSkip 2) (regPoly 8 1))
          ||| strutX 1
          ||| stroke (star (StarSkip 3) (regPoly 8 1))
    ```

    As you can see, `star`{.hs} may result in a path with multiple
    components, if the argument to `StarSkip`{.hs} and the number of
    vertices have a nontrivial common divisor.

-   `StarFun`{.hs} takes as an argument a function of type
    `(Int -> Int)`{.hs}, which specifies which vertices should be
    connected to which other vertices. Given the function $f$, vertex
    $i$ is connected to vertex $j$ if and only if $f(i) \equiv j \pmod
    n$, where $n$ is the number of vertices. This can be used as a
    compact, precise way of specifying how to connect a set of points
    (or as a fun way to visualize functions in $Z_n$!).

    ``` {.diagram-code}
    funs          = map (flip (^)) [2..6]
    visualize :: (Int -> Int) -> Diagram B
    visualize f   = strokePath' (with & vertexNames .~ [[0 .. 6 :: Int]] )
                        (regPoly 7 1)
                      # lw none
                      # showLabels
                      # fontSize (local 0.6)
                 <> star (StarFun f) (regPoly 7 1)
                      # strokePath # lw thick # lc red
    example       = center . hcat' (with & sep .~ 0.5) $ map visualize funs
    ```

You may notice that all the above examples need to call `stroke`{.hs}
(or `stroke'`{.hs}), which converts a path into a diagram. Many
functions similar to `star`{.hs} are polymorphic in their return type
over any `TrailLike`{.hs}, but `star`{.hs} is not. As we have seen,
`star`{.hs} may need to construct a path with multiple components, which
is not supported by the `TrailLike`{.hs} class.

Composing diagrams
------------------

The `diagrams` framework is fundamentally *compositional*: complex
diagrams are created by combining simpler diagrams in various ways. Many
of the combination methods discussed in this section are defined in
`Diagrams.Combinators`{.mod}.

### Superimposing diagrams with `atop`

The most fundamental way to combine two diagrams is to place one on top
of the other with `atop`{.hs}. The diagram `d1 \`{.hs}atop\` d2\` is
formed by placing `d1`{.hs}'s local origin on top of `d2`{.hs}'s local
origin; that is, by identifying their local vector spaces.

``` {.diagram-code}
example = circle 1 `atop` square (sqrt 2)
```

As noted before, diagrams form a [monoid](`Semigroups%20and%20monoids`_)
with composition given by superposition. `atop`{.hs} is simply a synonym
for `mappend`{.hs} (or `(<>)`{.hs}), specialized to two dimensions.

This also means that a list of diagrams can be stacked with
`mconcat`{.hs}; that is, `mconcat [d1, d2, d3, ...]`{.hs} is the diagram
with `d1`{.hs} on top of `d2`{.hs} on top of `d3`{.hs} on top of...

``` {.diagram-code}
example = mconcat [ circle 0.1 # fc green
                  , triangle 1 # scale 0.4 # fc yellow
                  , square 1   # fc blue
                  , circle 1   # fc red
                  ]
```

### Juxtaposing diagrams

Fundamentally, `atop`{.hs} is actually the *only* way to compose
diagrams; however, there are a number of other combining methods (all
ultimately implemented in terms of `atop`{.hs}) provided for
convenience.

Two diagrams can be placed *next to* each other using `beside`{.hs}. The
first argument to `beside`{.hs} is a vector specifying a direction. The
second and third arguments are diagrams, which are placed next to each
other so that the vector points from the first diagram to the second.

``` {.diagram-code}
example = beside (r2 (20,30))
                 (circle 1 # fc orange)
                 (circle 1.5 # fc purple)
          # showOrigin
```

As can be seen from the above example, the *length* of the vector makes
no difference, only its *direction* is taken into account. (To place
diagrams at a certain fixed distance from each other, see `cat'`{.hs}.)
As can also be seen, the local origin of the new, combined diagram is
the same as the local origin of the first diagram. This makes
`beside v`{.hs} associative, so diagrams under `beside v`{.hs} form a
semigroup. In fact, they form a monoid, since `mempty`{.hs} is a left
and right identity for `beside v`{.hs}, as can be seen in the example
below:

``` {.diagram-code}
example = hcat' (with & sep .~ 1) . map showOrigin
        $ [ d, mempty ||| d, d ||| mempty ]
  where d = square 1
```

In older versions of `diagrams`, the local origin of the combined
diagram was at the point of tangency between the two diagrams. To
recover the old behavior, simply perform an alignment on the first
diagram in the same direction as the argument to `beside`{.hs} before
combining (see `Alignment`{.hs}\_):

``` {.diagram-code}
example = beside (r2 (20,30))
                 (circle 1   # fc orange # align (r2 (20,30)))
                 (circle 1.5 # fc purple)
          # showOrigin
```

If you want to place two diagrams next to each other using the local
origin of the *second* diagram, you can use something like `beside' =
flip . beside . negated`{.hs}, that is, use a vector in the opposite
direction and give the diagrams in the other order.

Since placing diagrams next to one another horizontally and vertically
is quite common, special combinators are provided for convenience.
`(|||)`{.hs} and `(===)`{.hs} are specializations of `beside`{.hs} which
juxtapose diagrams in the $x$- and $y$-directions, respectively.

``` {.diagram-code}
d1 = circle 1 # fc red
d2 = square 1 # fc blue
example = (d1 ||| d2) ||| strutX 3 ||| ( d1
                                         ===
                                         d2  )
```

### Juxtaposing without composing

Sometimes, one may wish to *position* a diagram next to another diagram
without actually composing them. This can be accomplished with the
`juxtapose`{.hs} function. In particular, `juxtapose v d1 d2`{.hs}
returns a modified version of `d2`{.hs} which has been translated to be
next to `d1`{.hs} in the direction of `v`{.hs}. (In fact, `beside`{.hs}
itself is implemented as a call to `juxtapose`{.hs} followed by a call
to `(<>)`{.hs}.)

``` {.diagram-code}
d1 = juxtapose unitX             (square 1) (circle 1 # fc red)
d2 = juxtapose (unitX ^+^ unitY) (square 1) (circle 1 # fc green)
d3 = juxtapose unitY             (square 1) (circle 1 # fc blue)
example = circles ||| strutX 1 ||| (circles <> square 1)
  where circles = mconcat [d1, d2, d3]
```

See `envelopes and local vector spaces`{.hs}\_ for more information on
what "next to" means, and `Envelopes`{.hs}\_ for information on
functions available for manipulating envelopes. To learn about how
envelopes are implemented, see the [core library reference](`Angles`_).

### Concatenating diagrams

We have already seen one way to combine a list of diagrams, using
`mconcat`{.hs} to stack them. Several other methods for combining lists
of diagrams are also provided in `Diagrams.Combinators`{.mod}.

The simplest method of combining multiple diagrams is `position`{.hs},
which takes a list of diagrams paired with points, and places the local
origin of each diagram at the indicated point.

``` {.diagram-code}
example = position (zip (map mkPoint [-3, -2.8 .. 3]) (repeat spot))
  where spot       = circle 0.2 # fc black
        mkPoint x = p2 (x,x*x)
```

`cat`{.hs} is an iterated version of `beside`{.hs}, which takes a
direction vector and a list of diagrams, laying out the diagrams beside
one another in a row. The local origins of the subdiagrams will be
placed along a straight line in the direction of the given vector, and
the local origin of the first diagram in the list will be used as the
local origin of the final result.

``` {.diagram-code}
example = cat (r2 (2, -1)) (map p [3..8]) # showOrigin
  where p n = regPoly n 1
```

Semantically, `cat v === foldr (beside v) mempty`{.hs}, although the
actual implementation of `cat`{.hs} uses a more efficient balanced fold.

For more control over the way in which the diagrams are laid out, use
`cat'`{.hs}, a variant of `cat`{.hs} which also takes a `CatOpts`{.hs}
record. See the documentation for `cat'`{.hs} and `CatOpts`{.hs} to
learn about the various possibilities.

``` {.diagram-code}
example = cat' (r2 (2,-1)) (with & catMethod .~ Distrib & sep .~ 2 ) (map p [3..8])
  where p n = regPoly n 1 # scale (1 + fromIntegral n/4)
                          # showOrigin
```

For convenience, `Diagrams.TwoD.Combinators`{.mod} also provides
`hcat`{.hs}, `hcat'`{.hs}, `vcat`{.hs}, and `vcat'`{.hs}, variants of
`cat`{.hs} and `cat'`{.hs} which concatenate diagrams horizontally and
vertically. In addition, since using `hcat'`{.hs} or `vcat'`{.hs} with
some separation tends to be common, `hsep`{.hs} and `vsep`{.hs} are
provided as short synonyms; that is,
`hsep s = hcat' (with & sep .~ s)`{.hs}, and similarly for `vsep`{.hs}.

``` {.diagram-code}
example = hsep 0.2 (map square [0.3, 0.7 .. 2])
```

Finally, `appends`{.hs} is like an iterated variant of `beside`{.hs},
with the important difference that multiple diagrams are placed next to
a single central diagram without reference to one another; simply
iterating `beside`{.hs} causes each of the previously appended diagrams
to be taken into account when deciding where to place the next one. Of
course, `appends`{.hs} is implemented in terms of `juxtapose`{.hs} (see
`Juxtaposing without composing`{.hs}\_).

``` {.diagram-code}
c        = circle 1
dirs     = iterate (rotateBy (1/7)) unitX
cdirs    = zip dirs (replicate 7 c)
example1 = appends c cdirs
example2 = foldl (\a (v,b) -> beside v a b) c cdirs
example  = example1 ||| strutX 3 ||| example2
```

Modifying diagrams
------------------

### Attributes and styles

Every diagram has a *style* which is an arbitrary collection of
*attributes*. This section will describe some of the default attributes
which are provided by the `diagrams` library and recognized by most
backends. However, you can easily create your own attributes as well;
for details, see the [core library reference](paths.html).

In many examples, you will see attributes applied to diagrams using the
`(#)`{.hs} operator. Keep in mind that there is nothing special about
this operator as far as attributes are concerned. It is merely backwards
function application, which is used for attributes since it often reads
better to have the main diagram come first, followed by modifications to
its attributes. See `Postfix transformation`{.hs}\_.

In general, inner attributes (that is, attributes applied earlier)
override outer ones. Note, however, that this is not a requirement. Each
attribute may define its own specific method for combining multiple
values. Again, see the [core library
reference](http://en.wikipedia.org/wiki/Bézier_curve) for more details.

Most of the attributes discussed in this section are defined in
`Diagrams.TwoD.Attributes`{.mod}.

### Texture

Two-dimensional diagrams can be filled and stroked with a
`Texture`{.hs}. A `Texture`{.hs} can be either a solid color, a linear
gradient or a radial gradient. Not all backends support gradients, in
particular gradients are supported by the SVG, Cairo, and Rasterific
backends (see `Rendering backends`{.hs}\_). Future releases should also
support patterns as textures. The data type for a texture is

``` {.haskell}
data Texture = SC SomeColor | LG LGradient | RG RGradient
```

and `Prism`{.hs} s `_SC`{.hs}, `_LG`{.hs}, `_RG`{.hs} are provided for
access.

#### Color

The color used to stroke the paths can be set with the `lc`{.hs} (line
color) function and the color used to fill them with the `fc`{.hs} (fill
color) function.

``` {.diagram-code}
example = circle 0.2 # lc purple # fc yellow
```

By default, diagrams use a black line color and a completely transparent
fill color.

Colors themselves are handled by the `colour`{.pkg} package, which
provides a large set of predefined color names as well as many more
sophisticated color operations; see its documentation for more
information. The `colour`{.pkg} package uses a different type for colors
with an alpha channel (*i.e.* transparency). To make use of transparent
colors you can use `lcA`{.hs} and `fcA`{.hs}. The `palette`{.pkg}
package provides additional sets of colors and algorithms for creating
harmonious color combinations.

``` {.diagram-code}
import Data.Colour (withOpacity)

colors  = map (blue `withOpacity`) [0.1, 0.2 .. 1.0]
example = hcat' (with & catMethod .~ Distrib & sep .~ 1 )
                (zipWith fcA colors (repeat (circle 1)))
```

Transparency can also be tweaked with the `Opacity`{.hs} attribute,
which sets the opacity/transparency of a diagram as a whole. Applying
`opacity p`{.hs} to a diagram, where `p`{.hs} is a value between
`0`{.hs} and `1`{.hs}, results in a diagram `p`{.hs} times as opaque.

``` {.diagram-code}
s c     = square 1 # fc c
reds    = (s darkred ||| s red) === (s pink ||| s indianred)
example = hcat' (with & sep .~ 1 ) . take 4 . iterate (opacity 0.7) $ reds
```

To "set the background color" of a diagram, use the `bg`{.hs}
function---which does not actually set any attributes, but simply
superimposes the diagram on top of a bounding rectangle of the given
color. The `bgFrame`{.hs} function is similar but the background is
expanded to frame the diagram by a specified amount.

``` {.diagram-code}
t = regPoly 3 1

example = hsep 0.2 [t, t # bg orange, t # bgFrame 0.1 orange]
```

#### Linear Gradients

A linear gradient must have a list of color stops, a starting point, an
ending point, a transformation and a spread method. Color stops are
pairs of (color, fraction) where the fraction is usually between 0 and 1
that are mapped onto the start and end points. The starting point and
endping point are specified in local coordinates. Typically the
transformation starts as the identity transform `mempty`{.hs} and
records any transformations that are applied to the object using the
gradient. The spread method defines how space beyond the starting and
ending points should be handled, `GradPad`{.hs} will fill the space with
the final stop color, `GradRepeat`{.hs} will restart the gradient, and
`GradReflect`{.hs} will restart the gradient but with the stops
reversed. This is the data type for a linear gradient.

``` {.haskell}
data LGradient = LGradient
  { _lGradStops        :: [GradientStop]
  , _lGradStart        :: Point V2 n
  , _lGradEnd          :: Point V2 n
  , _lGradTrans        :: T2
  , _lGradSpreadMethod :: SpreadMethod }
```

Lenses are provided to access the record fields. In addition the
functions `mkStops`{.hs} taking a list of triples (color, fraction,
opacity) and `mkLinearGradient`{.hs} which takes a list of stops, a
start and end point, and a spread method and creates a `Texture`{.hs}
are provided for convenience. In this example we demonstrate how to make
linear gradients with the `mkLinearGradient`{.hs} functions and how to
adjust it using the lenses and prisms.

``` {.diagram-code}
stops = mkStops [(gray, 0, 1), (white, 0.5, 1), (purple, 1, 1)]
gradient = mkLinearGradient stops ((-0.5) ^& 0) (0.5 ^& 0) GradPad
sq1 = square 1 # fillTexture  gradient
sq2 = square 1 # fillTexture (gradient & _LG . lGradSpreadMethod .~ GradRepeat
                                       & _LG . lGradStart .~ (-0.1) ^& 0
                                       & _LG . lGradEnd .~ 0.1 ^& 0)
sq3 = square 1 # fillTexture (gradient & _LG . lGradSpreadMethod .~ GradReflect
                                       & _LG . lGradStart .~ (-0.1) ^& 0
                                       & _LG . lGradEnd .~ 0.1 ^& 0)

example = hcat' (with & sep .~ 0.25) [sq1, sq2, sq3]
```

Here we apply the gradient to the stroke only and give it starting and
ending points towards the corners.

``` {.diagram-code}
stops = mkStops [(teal, 0, 1), (orange, 1, 1)]
gradient = mkLinearGradient stops ((-1) ^& (-1)) (1 ^& 1) GradPad
example = rect 3 1 # lineTexture  gradient # lwO 15 # fc black # opacity 0.75
```

#### Radial Gradients

Radial gradients are similar, only they begin at the perimeter of an
inner cirlce and end at the perimeter of an outer circle.

``` {.haskell}
data RGradient = RGradient
    { _rGradStops        :: [GradientStop]
    , _rGradCenter0      :: Point V2 n
    , _rGradRadius0      :: Double
    , _rGradCenter1      :: Point V2 n
    , _rGradRadius1      :: Double
    , _rGradTrans        :: T2
    , _rGradSpreadMethod :: SpreadMethod }
```

Where radius and center 0 are for the inner circle, and 1 for the outer
circle. In this example we place the inner circle off center and place a
circle filled with the radial gradient on top of a rectangle filled with
a linear gradient to create a 3D effect.

``` {.diagram-code}
radial = mkRadialGradient (mkStops [(white,0,1), (black,1,1)])
                          ((-0.15) ^& (0.15)) 0.06 (0 ^& 0) 0.5
                          GradPad

linear = mkLinearGradient (mkStops [(black,0,1), (white,1,1)])
                          (0 ^& (-0.5)) (0 ^& 0.5)
                          GradPad

example = circle 0.35 # fillTexture radial # lw none
       <> rect 2 1 # fillTexture linear # lw none
```

#### Line width

Line width is actually more subtle than you might think. Suppose you
create a diagram consisting of a square, and another square twice as
large next to it (using `scale 2`{.hs}). How should they be drawn?
Should the lines be the same width, or should the larger square use a
line twice as thick? (Note that similar questions also come up when
considering the dashing style used to draw some shapes---should the size
of the dashes scale with transformations applied to the shapes, or not?)
`diagrams` allows the user to decide, using `Measure Double`{.hs} values
to specify things like line width (see `Measurement units`{.hs}\_).

In many situations, it is desirable to have lines drawn in a uniform
way, regardless of any scaling applied to shapes. This is what happens
with line widths measured in `global`{.hs}, `normalized`{.hs} or
`output`{.hs} units, as in the following example:

``` {.diagram-code}
example = hcat
  [ square 1
  , square 1 # scale 2
  , circle 1 # scaleX 3
  ]
  # dashingN [0.03,0.03] 0
  # lwN 0.01
```

For line widths that scale along with a diagram, use `local`{.hs}; in
this case line widths will be scaled in proportion to the geometeric
average of the scaling transformatins applied to the diagram.

The `LinemkWidth`{.hs} attribute is used to alter the *width* with which
paths are stroked. The most general functions that can be used to set
the line width are `linemkWidth`{.hs} and its synonym `lw`{.hs}, which
take an argument of type `Measure V2 n`{.hs}. Since typing things like
`linemkWidth
(normalized 0.01)`{.hs} is cumbersome, there are also shortcuts
provided: `lwG`{.hs}, `lwN`{.hs}, `lwO`{.hs}, and `lwL`{.hs} all take an
argument of type `Double`{.hs} and wrap it in `global`{.hs},
`normalized`{.hs}, `Ouput`{.hs} and `local`{.hs}, respectively.

There are also predefined `Measure n`{.hs} values with intuitive names,
namely, `ultraThin`{.hs}, `veryThin`{.hs}, `thin`{.hs}, `medium`{.hs},
`thick`{.hs}, `veryThick`{.hs}, `ultraThick`{.hs}, and `none`{.hs} (the
default is `medium`{.hs}), which should often suffice for setting the
line width.

``` {.diagram-code}
line = strokeT . fromOffsets $ [unitX]
example = vcat' (with & sep .~ 0.1)
  [line # lw w | w <- [ultraThin, veryThin, thin,
                       medium, thick, veryThick, ultraThick]]
```

In the above example, there is no discernible difference between
`ultraThin`{.hs}, `veryThin`{.hs}, and `thin`{.hs}; these names all
describe `normalized`{.hs} measurements with a physical lower bound, so
the physical width of the resulting lines depends on the physical size
of the rendered diagram. At larger rendering sizes the differences
between the smaller widths become apparent.

<div class="todo">

Explain that they all have a minimum output size. Should wait until we
have a better idea what the heck output size actually means.

</div>

Note that line width does not affect the envelope of diagrams at all. To
stroke a line "internally", turning it into a `Path`{.hs} value
enclosing the stroked area (which *does* contribute to the envelope),
you can use one of the functions described in the section `Offsets of
segments, trails, and paths`{.hs}\_.

#### Other line parameters

Many rendering backends provide some control over the particular way in
which lines are drawn. Currently, `diagrams` provides support for three
aspects of line drawing:

-   `lineCap`{.hs} sets the `LineCap`{.hs} style.
-   `lineJoin`{.hs} sets the `LineJoin`{.hs} style.
-   `dashing`{.hs} allows for drawing dashed lines with arbitrary
    dashing patterns.

``` {.diagram-code}
path = fromVertices (map p2 [(0,0), (1,0.3), (2,0), (2.2,0.3)]) # lwO 10
example = center . vcat' (with & sep .~ 0.1 )
          $ map (path #)
            [ lineCap LineCapButt   . lineJoin LineJoinMiter
            , lineCap LineCapRound  . lineJoin LineJoinRound
            , lineCap LineCapSquare . lineJoin LineJoinBevel
            , dashingN [0.03,0.06,0.09,0.03] 0
            ]
```

#### The `HasStyle` class

Functions such as `fc`{.hs}, `lc`{.hs}, `lw`{.hs}, and `lineCap`{.hs} do
not take only diagrams as arguments. They take any type which is an
instance of the `HasStyle`{.hs} type class. Of course, diagrams
themselves are an instance.

However, the `Style`{.hs} type is also an instance. This is useful in
writing functions which offer the caller flexible control over the style
of generated diagrams. The general pattern is to take a `Style`{.hs} (or
several) as an argument, then apply it to a diagram along with some
default attributes:

``` {.haskell}
myFun style = d # applyStyle style # lc red # ...
  where d = ...
```

This way, any attributes provided by the user in the `style`{.hs}
argument will override the default attributes specified afterwards.

To call `myFun`{.hs}, a user can construct a `Style`{.hs} by starting
with an empty style (`mempty`{.hs}, since `Style`{.hs} is an instance of
`Monoid`{.hs}) and applying the desired attributes:

``` {.haskell}
foo = myFun (mempty # fontSize (local 2) # lw none # fc green)
```

If the type `T`{.hs} is an instance of `HasStyle`{.hs}, then `[T]`{.hs}
is also. This means that you can apply styles uniformly to entire lists
of diagrams at once, which occasionally comes in handy, for example, to
assign a default attribute to all diagrams in a list which do not
already have one:

``` {.diagram-code}
example = hcat $
  [circle 1, square 2, triangle 2 # fc yellow, hexagon 1] # fc blue
```

Likewise, there are `HasStyle`{.hs} instances for pairs, `Map`{.hs}s,
`Set`{.hs}s, and functions.

### Static attributes

Diagrams can also have "static attributes" which are applied at a
specific node in the tree representing a diagram. Currently, the only
static attribute is a hyperlink, which is supported only by the SVG
backend. To turn a diagram into a hyperlink, use the `href`{.hs}
function.

More static attributes (for example, node IDs and transparency grouping)
and wider backend support will be added in future versions.

### 2D Transformations

Any diagram can be transformed by applying arbitrary affine
transformations to it. *Affine* transformations include *linear*
transformations (rotation, scaling, reflection, shears---anything which
leaves the origin fixed and sends lines to lines) as well as
translations. In the simplified case of the real line, an affine
transformation is any function of the form $f(x) = mx + b$. Generalizing
to $d$ dimensions, an affine transformation is a vector function of the
form $f(\mathbf{v}) = \mathbf{M}\mathbf{v} +
\mathbf{b}$, where $\mathbf{M}$ is a $d \times d$ matrix. More general,
non-affine transformations, including projective transformations, are
referred to in `diagrams` as `Deformations`{.hs}\_.

`Diagrams.TwoD.Transform`{.mod} defines a number of common affine
transformations in two-dimensional space. (To construct transformations
more directly, see `Diagrams.Core.Transform`{.mod}.)

Every transformation comes in two variants, a noun form and a verb form.
For example, there are two functions for scaling along the $x$-axis,
`scalingX`{.hs} and `scaleX`{.hs}. The noun form (*e.g.*
`scalingX`{.hs}) constructs a `Transformation`{.hs} value, which can
then be stored in a data structure, passed as an argument, combined with
other transformations, *etc.*, and ultimately applied to a diagram (or
other `Transformable`{.hs} value) with the `transform`{.hs} function.
The verb form directly applies the transformation. The verb form is much
more common (and the documentation below will only discuss verb forms),
but getting one's hands on a first-class `Transformation`{.hs} value can
occasionally be useful.

<div class="warning">

Both the verb and noun variants of transformations are monoids, and can
be composed with `(<>)`{.hs}. However, the results are quite distinct,
as shown in this example.

``` {.diagram-code}
ell = text "L" <> square 1 # lw none
alpha = 45 @@ deg

dia1 = ell # translateX 2 # rotate alpha
dia2 = ell # ( rotate alpha <> translateX 2 )
dia3 = ell # transform ( rotation alpha <> translationX 2 )

example =
  hcat' (with & sep .~ 2)
    [ (dia1 <> orig)
    , vrule 4
    , (dia2 <> orig)
    , vrule 4
    , (dia3 <> orig)
    ]
  where
    orig = circle 0.05 # fc red # lw none
```

`dia1`{.hs} is the intended result: a character L translated along the X
axis, and then rotated 45 degrees around the origin.

`dia2`{.hs} shows the result of naively composing the verb versions of
the transformations: a superposition of a rotated L and a translated L.
To understand this, consider that `(rotate alpha)`{.hs} is a *function*,
and functions as monoid instances (`Monoid m =>
Monoid (a -> m)`{.hs}) are composed as `(f <> g) x = f x <> g x`{.hs}.
To quote the
[Typeclassopedia](http://www.haskell.org/haskellwiki/Typeclassopedia#Instances_4):
if `a`{.hs} is a Monoid, then so is the function type `e -> a`{.hs} for
any `e`{.hs}; in particular, `g \`{.hs}mappend\` h\` is the function
which applies both `g`{.hs} and `h`{.hs} to its argument and then
combines the results using the underlying Monoid instance for `a`{.hs}.

Hence `ell # ( rotate alpha <> translateX 2 )`{.hs} is the same as the
superposition of two diagrams: `rotate alpha ell <>
translateX 2 ell`{.hs}.

`dia3`{.hs} shows how the noun versions can be composed (using the
`Monoid`{.hs} instance for `Transformation`{.hs}) with the intended
result.

</div>

#### Affine transformations in general

Before looking at specific two-dimensional transformations, it's worth
saying a bit about transformations in general (a fuller treatment can be
found in the [core library reference](core.html)). The
`Transformation`{.hs} type is defined in
`Diagrams.Core.Transform`{.mod}, from the `diagrams-core`{.pkg} package.
`Transformation`{.hs} is parameterized by the vector space over which it
acts; recall that `T2 n`{.hs} is provided as a synonym for
`Transformation V2 n`{.hs}.

`Transformation v`{.hs} is a `Monoid`{.hs} for any vector space \`v\`:

-   `mempty`{.hs} is the identity transformation;
-   `mappend`{.hs} is composition of transformations:
    `t1 \`{.hs}mappend\` t2\` (also written `t1 <> t2`{.hs}) performs
    first `t2`{.hs}, then `t1`{.hs}.

To invert a transformation, use `inv`{.hs}. For any transformation
`t`{.hs},

`t <> inv t === inv t <> t === mempty`{.hs}.

To apply a transformation, use `transform`{.hs}.

#### Rotation

Use `rotate`{.hs} to rotate a diagram counterclockwise by a given
[angle](/doc/paths.html) about the origin. Since `rotate`{.hs} takes an
`Angle n`{.hs}, you must specify an angle unit, such as
`rotate (80 @@ deg)`{.hs}. In the common case that you wish to rotate by
an angle specified as a certain fraction of a circle, like
`rotate (1/8 @@ turn)`{.hs}, you can use `rotateBy`{.hs} instead.
`rotateBy`{.hs} takes a `Double`{.hs} argument expressing the number of
turns, so in this example you would only have to write `rotateBy
(1/8)`{.hs}.

You can also use `rotateAbout`{.hs} in the case that you want to rotate
about some point other than the origin.

``` {.diagram-code}
eff = text "F" <> square 1 # lw none
rs  = map rotateBy [1/7, 2/7 .. 6/7]
example = hcat . map (eff #) $ rs
```

#### Scaling and reflection

Scaling by a given factor is accomplished with `scale`{.hs} (which
scales uniformly in all directions), `scaleX`{.hs} (which scales along
the $x$-axis only), or `scaleY`{.hs} (which scales along the $y$-axis
only). All of these can be used both for enlarging (with a factor
greater than one) and shrinking (with a factor less than one). Using a
negative factor results in a reflection (in the case of `scaleX`{.hs}
and `scaleY`{.hs}) or a 180-degree rotation (in the case of
`scale`{.hs}).

``` {.diagram-code}
eff = text "F" <> square 1 # lw none
ts  = [ scale (1/2), id, scale 2,    scaleX 2,    scaleY 2
      ,                  scale (-1), scaleX (-1), scaleY (-1)
      ]

example = hcat . map (eff #) $ ts
```

Scaling by zero is forbidden. Let us never speak of it again.

For convenience, `reflectX`{.hs} and `reflectY`{.hs} perform reflection
along the $x$- and $y$-axes, respectively. Their names can be confusing
(does `reflectX`{.hs} reflect *along* the $x$-axis or *across* the
$x$-axis?) but you can just remember that `reflectX = scaleX (-1)`{.hs},
and similarly for `reflectY`{.hs}; that is, `reflectQ` affects
`Q`-coordinates.

To reflect in some line other than an axis, use `reflectAbout`{.hs}.

``` {.diagram-code}
eff = text "F" <> square 1 # lw none
example = eff
       <> reflectAbout (p2 (0.2,0.2)) (rotateBy (-1/10) unitX) eff
```

#### Translation

Translation is achieved with `translate`{.hs}, `translateX`{.hs}, and
`translateY`{.hs}, which should be self-explanatory.

#### Conjugation

`Diagrams.Transform`{.mod} exports useful transformation utilities which
are not specific to two dimensions. At the moment there are only two:
`conjugate`{.hs} and `under`{.hs}. The first simply performs
conjugation: `conjugate t1 t2 == inv t1 <> t2 <> t1`{.hs}, that is,
performs `t1`{.hs}, then `t2`{.hs}, then undoes `t1`{.hs}.

`under`{.hs} performs a transformation using conjugation. It takes as
arguments a function `f`{.hs} as well as a transformation to conjugate
by, and produces a function which performs the transformation, then
`f`{.hs}, then the inverse of the transformation. For example, scaling
by a factor of 2 along the diagonal line $y = x$ can be accomplished
thus:

``` {.diagram-code}
eff = text "F" <> square 1 # lw none
example = (scaleX 2 `under` rotation (-1/8 @@ turn)) eff
```

The letter F is first rotated so that the desired scaling axis lies
along the $x$-axis; then `scaleX`{.hs} is performed; then it is rotated
back to its original position.

Note that `reflectAbout`{.hs} and `rotateAbout`{.hs} are implemented
using `under`{.hs}.

#### The `Transformable` class

Transformations can be applied not just to diagrams, but values of any
type which is an instance of the `Transformable`{.hs} type class.
Instances of `Transformable`{.hs} include vectors, points, trails,
paths, envelopes, and `Transformations`{.hs} themselves. In addition,
tuples, lists, maps, or sets of `Transformable`{.hs} things are also
`Transformable`{.hs} in the obvious way.

### Deformations

The affine transformations represented by `Transformation`{.hs} include
the most commonly used transformations, but occasionally other sorts are
useful. Non-affine transformations are represented by the
`Deformation`{.hs} type. The design is quite similar to that of
`Transformation`{.hs}. A `Deformation`{.hs} is parameterized by the
vector space over which it acts. There is a `Deformable`{.hs} type
class, and `deform`{.hs} applies a `Deformation`{.hs} to a
`Deformable`{.hs} type in the same vector space, returning a value of
the same type.

`Diagrams.TwoD.Deform`{.mod} defines parallel and perspective
projections along the principal axes in 2 dimensions.

``` {.diagram-code}
sq = unitSquare # translate (5 ^& 3) :: Path V2 Double
marks = repeat . lw none $ circle 0.02
spots c p = position $ zip (concat $ pathVertices p) (marks # fc c)
example = stroke sq <> spots blue sq <> spots green (deform perspectiveX1 sq)
```

The example above projects a square onto the plane x=1. In this example,
only the projected vertices are drawn, since the four overlapping line
segments corresponding to the edges of the square are not interesting.
Note, though, that the `Path`{.hs} is deformed, and then the vertices
are taken from the projected result.

`Deformation v`{.hs} is a `Monoid`{.hs} for any vector space `v n`{.hs}.
New deformations can be formed by composing two deformations. The
composition of an affine transformation with a `Deformation`{.hs} is
also a `Deformation`{.hs}. `asDeformation`{.hs} converts a
`Transformation`{.hs} to an equivalent `Deformation`{.hs}, "forgetting"
the inverse and other extra information which distinguishes affine
transformations.

The very general nature of deformations prevents certain types from
being `Deformable`{.hs}. Because not every `Deformation`{.hs} is
invertible, diagrams cannot be deformed. In general, for two points $p$
and $q$, and a deformation $D$, there is no deformation $D_v$ such that,
$Dp - Dq = D_v(p-q)$. For this reason, only points and concretely
located types are deformable. Finally, segments are not deformable
because the image of the segment may not be representable by a single
segment. The `Deformable`{.hs} instances for trails and paths will
approximate each segment by several segments as necessary. Points,
`Located`{.hs} trails, and paths are all deformable.

Because approximation and subdivision are required for many
`Deformable`{.hs} instances, the type class provides a function
`deform'`{.hs}, which takes the approximation accuracy as its first
argument. For trails and paths, `deform`{.hs} (without a prime) calls
`deform'`{.hs} with an error limit of 0.01 times the object's size.

### Alignment

Since diagrams are always combined with respect to their local origins,
moving a diagram's local origin affects the way it combines with others.
The position of a diagram's local origin is referred to as its
*alignment*.

The functions `moveOriginBy`{.hs} and `moveOriginTo`{.hs} are provided
for explicitly moving a diagram's origin, by an absolute amount and to
an absolute location, respectively. `moveOriginBy`{.hs} and
`translate`{.hs} are actually dual, in the sense that

``` {.law}
moveOriginBy v === translate (negated v).
```

This duality comes about since `translate`{.hs} moves a diagram with
respect to its origin, whereas `moveOriginBy`{.hs} moves the *origin*
with respect to the *diagram*. Both are provided so that you can use
whichever one corresponds to the most natural point of view in a given
situation, without having to worry about inserting calls to
`negated`{.hs}.

Often, however, one wishes to move a diagram's origin with respect to
its "boundary". Here, boundary usually refers to the diagram's envelope
or trace, with envelope being the default (see `Envelopes`{.hs}\_ and
`Traces`{.hs}\_ for more information). To this end, some general tools
are provided in `Diagrams.Align`{.mod}, and specialized 2D-specific ones
by `Diagrams.TwoD.Align`{.mod}.

Functions like `alignT`{.hs} (align Top) and `alignBR`{.hs} (align
Bottom Right) move the local origin to the edge of the envelope:

``` {.diagram-code}
s = square 1 # fc yellow
x |-| y = x ||| strutX 0.5 ||| y
example =  (s # showOrigin)
       |-| (s # alignT  # showOrigin)
       |-| (s # alignBR # showOrigin)
```

There are two things to note about the above example. First, notice how
`alignT`{.hs} and `alignBR`{.hs} move the local origin of the square in
the way you would expect. Second, notice that when placed "next to" each
other using the `(|||)`{.hs} operator, the squares are placed so that
their local origins fall on a horizontal line.

Functions like `alignY`{.hs} allow finer control over the alignment. In
the below example, the origin is moved to a series of locations
interpolating between the bottom and top of the square:

``` {.diagram-code}
s = square 1 # fc yellow
example = hcat . map showOrigin
        $ zipWith alignY [-1, -0.8 .. 1] (repeat s)
```

To center an object along an axis we provide the functions
`centerX`{.hs} and `centerY`{.hs}. An object can be simultaneously
centered along both axis (actually along all of its basis vectors) using
the `center`{.hs} function.

The align functions have sister functions like `snugL`{.hs} and
`snugX`{.hs} that work the same way as `alignL`{.hs} and `alignX`{.hs}.
The difference is that the `snug`{.hs} class of functions use the trace
as the boundary instead of the envelope. For example, here we want to
snug a convex shape (the orange triangle) next to a concave shape (the
blue polygon):

``` {.diagram-code}
import Diagrams.TwoD.Align

concave = polygon ( with & polyType .~ PolyPolar [a, b, b, b]
                  [ 0.25,1,1,1,1] & polyOrient .~ NoOrient )
                  # fc blue # lw none
  where
    a = 1/8 @@ turn
    b = 1/4 @@ turn

convex = polygon (with & polyType .~ PolyPolar [a,b] [0.25, 1, 1]
                       & polyOrient .~ NoOrient)
                       # fc orange # lw none
  where
    a = 1/8 @@ turn
    b = 3/4 @@ turn

aligned = (concave # center # alignR # showOrigin)
       <> (convex # center # alignL # showOrigin)

snugged = (concave # center # snugR # showOrigin)
       <> (convex # center # snugL # showOrigin)

example = aligned ||| strutX 0.5 ||| snugged
```

The `snugR`{.hs} function moves the origin of the blue polygon to the
rightmost edge of its trace in the diagram on the right, whereas in the
left diagram the `alignR`{.hs} function puts it at the edge of the
envelope.

Trails and paths
----------------

Trails and paths are some of the most fundamental tools in `diagrams`.
They can be used not only directly to draw things, but also as guides to
help create and position other diagrams.

For additional practice and a more "hands-on" experience learning about
trails and paths, see the [trails and paths tutorial](metafont.html).

### Segments

The most basic component of trails and paths is a `Segment`{.hs}, which
is some sort of primitive path from one point to another. Segments are
*translationally invariant*; that is, they have no inherent location,
and applying a translation to a segment has no effect (however, other
sorts of transformations, such as rotations and scales, have the effect
you would expect). In other words, a segment is not a way to get from
some particular point A to another point B; it is a way to get from
*wherever you currently happen to be* to *somewhere else*.

Currently, `diagrams` supports two types of segment, defined in
`Diagrams.Segment`{.mod}:

-   A *linear* segment is simply a straight line, defined by an offset
    from its beginning point to its end point; you can construct one
    using `straight`{.hs}.
-   A *Bézier* segment is a cubic curve defined by an offset from its
    beginning to its end, along with two control points; you can
    construct one using `bezier3`{.hs} (or `bézier3`{.hs}, if you are
    feeling snobby). An example is shown below, with the endpoints shown
    in red and the control points in blue. [Bézier curves](arrow.html)
    always start off from the beginning point heading towards the first
    control point, and end up at the final point heading away from the
    last control point. That is, in any drawing of a Bézier curve like
    the one below, the curve will be tangent to the two dotted lines.

``` {.diagram-code}
illustrateBézier c1 c2 x2
    =  endpt
    <> endpt  # translate x2
    <> ctrlpt # translate c1
    <> ctrlpt # translate c2
    <> l1
    <> l2
    <> fromSegments [bézier3 c1 c2 x2]
  where
    dashed  = dashingN [0.03,0.03] 0
    endpt   = circle 0.05 # fc red  # lw none
    ctrlpt  = circle 0.05 # fc blue # lw none
    l1      = fromOffsets [c1] # dashed
    l2      = fromOffsets [x2 ^-^ c2] # translate c2 # dashed

x2      = r2 (3,-1) :: V2 Double         -- endpoint
[c1,c2] = map r2 [(1,2), (3,0)]   -- control points

example = illustrateBézier c1 c2 x2
```

Independently of the two types of segments explained above, segments can
be either *closed* or *open*. A *closed* segment has a fixed endpoint
relative to its start. An *open* segment, on the other hand, has an
endpoint determined by its context; open segments are used to implement
loops (explained in the `Trails`{.hs}\_ section below). Most users
should have no need to work with open segments. (For that matter, most
users will have no need to work directly with segments at all.)

If you look in the `Diagrams.Segment`{.mod} module, you will see quite a
bit of other stuff related to the implementation of trails
(`SegMeasure`{.hs} and so on); this is explained in more detail in the
section `Trail and path implementation details`{.hs}\_.

### Trails

Trails are defined in `Diagrams.Trail`{.mod}. Informally, you can think
of trails as lists of segments laid end-to-end. Since segments are
translation-invariant, so are trails. More formally, the semantics of a
trail is a continuous (though not necessarily differentiable) function
from the real interval $[0,1]$ to vectors in some vector space. This
section serves as a reference on trails; for a more hands-on
introduction, refer to the [Trail and path
tutorial](/gallery/SymmetryCube.html).

There are two types of trail:

-   A *loop*, with a type like `Trail' Loop v`{.hs}, is a trail which
    forms a "closed loop", ending at the same place where it started.

    ``` {.diagram}
    example = fromOffsets [1 ^& 1, 2 ^& (-1), (-1) ^& (-1), (-3) ^& 1]
            # closeLine # strokeLoop # fc blue
    ```

    Loops in 2D can be filled, as in the example above.

-   A *line*, with a type like `Trail' Line v`{.hs}, is a trail which
    does not form a closed loop, that is, it starts in one place and
    ends in another.

    ``` {.diagram}
    example = fromOffsets [1 ^& 1, 2 ^& (-1), (-1) ^& (-1), (-3) ^& 1]
            # strokeLine
    ```

    Actually, a line can in fact happen to end in the same place where
    it starts, but even so it is still not considered closed. Lines have
    no inside and outside, and are never filled.

    <div class="warning">

    Lines are never filled, even when they happen to start and end in
    the same place!

    </div>

Finally, the type `Trail`{.hs} can contain either a line or a loop.

The most important thing to understand about lines, loops, and trails is
how to convert between them.

-   To convert from a line or a loop to a trail, use `wrapLine`{.hs} or
    `wrapLoop`{.hs} (or `wrapTrail`{.hs}, if you don't know or care
    whether the parameter is a line or loop).
-   To convert from a loop to a line, use `cutLoop`{.hs}. This results
    in a line which just so happens to end where it starts.
-   To convert from a line to a loop, there are two choices:
    -   `closeLine`{.hs} adds a new linear segment from the end to the
        start of the line.

        ``` {.diagram-code}
        almostClosed :: Trail' Line V2 Double
        almostClosed = fromOffsets $ (map r2
          [(2, -1), (-3, -0.5), (-2, 1), (1, 0.5)])

        example = pad 1.1 . center . fc orange . hcat' (with & sep .~ 1)
          $ [ almostClosed # strokeLine
            , almostClosed # closeLine # strokeLoop
            ]
        ```

    -   `glueLine`{.hs} simply modifies the endpoint of the final
        segment to be the start of the line. This is most often useful
        if you have a line which you know just so happens to end where
        it starts; calling `closeLine`{.hs} in such a case would result
        in the addition of a gratuitous length-zero segment.

Lines form a monoid under concatenation. For example, below we create a
two-segment line called `spoke` and then construct a starburst path by
concatenating a number of rotated copies. Note how we call
`glueLine`{.hs} to turn the starburst into a closed loop, so that we can
fill it (lines cannot be filled). `strokeLoop`{.hs} turns a loop into a
diagram, with the start of the loop at the local origin. (There are also
analogous functions `strokeLine`{.hs} and `strokeTrail`{.hs}.)

``` {.diagram-code}
spoke :: Trail' Line V2 Double
spoke = fromOffsets . map r2 $ [(1,3), (1,-3)]

burst :: Trail' Loop V2 Double
burst = glueLine . mconcat . take 13 . iterate (rotateBy (-1/13)) $ spoke

example = strokeLoop burst # fc yellow # lw thick # lc orange
```

For convenience, there is also a monoid instance for `Trail`{.hs} based
on the instance for lines: any loops are first cut with `cutLine`{.hs},
and the results concatenated. Typically this would be used in a
situation where you know that all your trails actually contain lines.

Loops, on the other hand, have no monoid instance.

To construct a line, loop, or trail, you can use one of the following:

-   `fromOffsets`{.hs} takes a list of vectors, and turns each one into
    a linear segment.

    ``` {.diagram-code}
    theLine = fromOffsets (iterateN 5 (rotateBy (1/20)) unitX)
    example = theLine # strokeLine
            # lc blue # lw thick # center # pad 1.1
    ```

-   `fromVertices`{.hs} takes a list of vertices, generating linear
    segments between them.

    ``` {.diagram-code}
    vertices = map p2 $ [(x,y) | x <- [0,0.2 .. 2], y <- [0,1]]
    example = fromVertices vertices # strokeLine
            # lc red # center # pad 1.1
    ```

-   `(~~)`{.hs} creates a simple linear trail between two points.
-   `cubicSpline`{.hs} creates a smooth curve passing through a given
    list of points; it is described in more detail in the section on
    `Splines`{.hs}\_.

    ``` {.diagram-code}
    vertices = map p2 . init $ [(x,y) | x <- [0,0.5 .. 2], y <- [0,0.2]]
    theLine = cubicSpline False vertices
    example = mconcat (iterateN 6 (rotateBy (-1/6)) theLine)
            # glueLine # strokeLoop
            # lc green # lw veryThick # fc aqua # center # pad 1.1
    ```

-   `fromSegments`{.hs} takes an explicit list of `Segment`{.hs}s, which
    can occasionally be useful if, say, you want to generate some Bézier

All the above functions construct loops by first constructing a line and
then calling `glueLine`{.hs} (see also the below section on
`TrailLike`{.hs}\_).

If you look at the types of these functions, you will note that they do
not, in fact, return just `Trail`{.hs}s: they actually return any type
which is an instance of `TrailLike`{.hs}, which includes lines, loops,
`Trail`{.hs}s, `Path`{.hs}s (to be covered in an upcoming section),
`Diagram`{.hs}s, lists of points, and any of these wrapped in
`Located`{.hs} (see below). See the `TrailLike`{.hs}\_ section for more
on the `TrailLike`{.hs} class.

For details on other functions provided for manipulating trails, see the
documentation for `Diagrams.Trail`{.mod}. One other function worth
mentioning is `explodeTrail`{.hs}, which turns each segment in a trail
into its own individual `Path`{.hs}. This is useful when you want to
construct a trail but then do different things with its individual
segments. For example, we could construct the same starburst as above
but color the edges individually:

``` {.diagram-code}
spoke :: Trail V2 Double
spoke = fromOffsets . map r2 $ [(1,3), (1,-3)]

burst = mconcat . take 13 . iterate (rotateBy (-1/13)) $ spoke

colors = cycle [aqua, orange, deeppink, blueviolet, crimson, darkgreen]

example = lw thick
        . mconcat
        . zipWith lc colors
        . map strokeLocTrail . explodeTrail
        $ burst `at` origin
```

(If we wanted to fill the starburst with yellow as before, we would have
to separately draw another copy of the trail with a line width of zero
and fill that; this is left as an exercise for the reader.)

### Located

Something of type `Located a`{.hs} consists, essentially, of a value of
type `a`{.hs} paired with a point. In this way, `Located`{.hs} serves to
transform translation-invariant things (such as `Segment`{.hs}s or
`Trail`{.hs}s) into things with a fixed location. A `Located Trail`{.hs}
is a `Trail`{.hs} where we have picked a concrete location for its
starting point, and so on.

The module `Diagrams.Located`{.mod} defines the `Located`{.hs} type and
utilities for working with it:

-   `at`{.hs} is used to construct `Located`{.hs} values, and is
    designed to be used infix, like `someTrail \`{.hs}at\` somePoint\`.
-   `viewLoc`{.hs}, `unLoc`{.hs}, and `loc`{.hs} can be used to project
    out the components of a `Located`{.hs} value.
-   `mapLoc`{.hs} can be used to apply a function to the value of type
    `a`{.hs} inside a value of type `Located a`{.hs}. Note that
    `Located`{.hs} is not a functor, since it is not possible to change
    the contained type arbitrarily: `mapLoc`{.hs} does not change the
    location, and the vector space associated to the type `a`{.hs} must
    therefore remain the same.

Much of the utility of having a concrete type for the `Located`{.hs}
concept (rather than just passing around values paired with points) lies
in the type class instances we can give to \`Located\`:

-   \`HasOrigin\`: translating a `Located a`{.hs} simply translates the
    associated point, leaving the value of type `a`{.hs} unaffected.
-   \`Transformable\`: only the linear component of transformations are
    applied to the wrapped value (whereas the entire transformation is
    applied to the location).
-   \`Enveloped\`: the envelope of a `Located a`{.hs} is the envelope of
    the contained `a`{.hs}, translated to the stored location (and
    similarly for `Traced`{.hs}).
-   The `TrailLike`{.hs} instance is also useful; see TrailLike\_.

### Paths

A `Path`{.hs}, also defined in `Diagrams.Path`{.mod}, is a (possibly
empty) collection of `Located Trail`{.hs}s. Paths of a single trail can
be constructed using the same functions described in the previous
section: `fromSegments`{.hs}, `fromOffsets`{.hs}, `fromVertices`{.hs},
`(~~)`{.hs}, and `cubicSpline`{.hs}.

`Path`{.hs}s also form a `Monoid`{.hs}, but the binary operation is
*superposition* (just like that of diagrams). Paths with multiple
components can be used, for example, to create shapes with holes:

``` {.diagram-code}
ring :: Path V2 Double
ring = circle 3 <> (circle 2 # reversePath)

example = stroke ring # fc purple
```

See the section on `Fill rules`{.hs}\_ for more information.

`stroke`{.hs} turns a path into a diagram, just as `strokeTrail`{.hs}
turns a trail into a diagram. (In fact, `strokeTrail`{.hs} really works
by first turning the trail into a path and then calling `stroke`{.hs} on
the result.)

`explodePath`{.hs}, similar to `explodeTrail`{.hs}, turns the segments
of a path into individual paths. Since a path is a collection of trails,
each of which is a sequence of segments, `explodePath`{.hs} actually
returns a list of lists of paths.

For information on other path manipulation functions such as
`pathFromTrail`{.hs}, `pathFromLocTrail`{.hs}, `pathVertices`{.hs},
`pathOffsets`{.hs}, `scalePath`{.hs}, and `reversePath`{.hs}, see the
Haddock documentation in `Diagrams.Path`{.mod}.

### Stroking trails and paths

The `strokeTrail`{.hs} and `stroke`{.hs} functions, which turn trails
and paths into diagrams respectively, have already been mentioned; they
are defined in `Diagrams.TwoD.Path`{.mod}. Both also have primed
variants, `strokeTrail'`{.hs} and `stroke'`{.hs}, which take a record of
`StrokeOpts`{.hs}. Currently, `StrokeOpts`{.hs} has two fields:

-   `vertexNames`{.hs} takes a list of lists of names, and zips each
    list with a component of the path, creating point subdiagrams (using
    `pointDiagram`{.hs}) associated with the names. This means that the
    names can be used to later refer to the locations of the path
    vertices (see `Named subdiagrams`{.hs}\_). In the case of
    `strokeTrail'`{.hs}, only the first list is used.

    By default, `vertexNames`{.hs} is an empty list.

-   `queryFillRule`{.hs} specifies the fill rule (see
    `Fill rules`{.hs}\_) used to determine which points are inside the
    diagram, for the purposes of its query (see `Using queries`{.hs}\_).
    Note that it does *not* affect how the diagram is actually drawn;
    for that, use the `fillRule`{.hs} function. (This is not exactly a
    feature, but for various technical reasons it is not at all obvious
    how to have this field actually affect both the query and the
    rendering of the diagram.)

    By default, `queryFillRule`{.hs} is set to `Winding`{.hs}.

### Offsets of segments, trails, and paths

Given a segment and an offset radius $r$ we can make an *offset segment*
that is the distance $r$ from the original segment. More specifically,
you can think of the offset as the curve traced by the end of a vector
of length $r$ perpendicular to the original curve. This vector goes on
the right of the curve for a positive radius and on the left for a
negative radius.

``` {.diagram-code}
import Diagrams.TwoD.Offset

example :: Diagram B
example = hcat' (with & sep .~ 1) $ map f
        [ straight p
        , bézier3 (r2 (0,0.5)) (r2 (1,0.5)) p
        ]
  where
    p = r2 (1,1)
    f :: Segment Closed V2 Double -> Diagram B
    f s =  fromSegments [s]
        <> offsetSegment 0.1 0.2 s # strokeLocTrail # lc blue
```

<div class="todo">

Animate tracing an offset?

</div>

For a straight segment this will clearly be a parallel straight line
with $r$ as the distance between the lines. For an counter-clockwise arc
of radius $R$ the offset will be an arc with the same center, start and
end angles, and radius $r+R$. Cubic segments present a problem, however.
The offset of a cubic Bézier curve could be a higher degree curve. To
accommodate this we approximate the offset with a sequence of segments.
We now have enough details to write the type for `offsetSegment`{.hs}.

``` {.haskell}
offsetSegment :: Double -> Double -> Segment Closed V2 Double -> Located (Trail V2 Double)
```

The first parameter to `offsetSegment`{.hs} is an epsilon factor
$\epsilon$. When the radius is multiplied by $\epsilon$ we get the
maximum allowed distance a point on the approximate offset can differ
from the true offset. The final parameters are the radius and the
segment. The result is a located trail. It is located because the
offset's start will be distance $r$ away from the segment start which is
the origin.

If we can offset a segment we naturally will want to extend this to
offset a trail. A first approach might be to simply map
`offsetSegment`{.hs} over the segments of a trail. But we quickly notice
that if the trail has any sharp corners, the offset will be
disconnected!

``` {.diagram-code}
import Diagrams.TwoD.Offset

locatedTrailSegments t = zipWith at (trailSegments (unLoc t)) (trailVertices t)

bindLoc f = join' . mapLoc f
  where
    join' x = let (p,a) = viewLoc x in translate (p .-. origin) a

offsetTrailNaive :: Double -> Double -> Trail V2 Double -> Path V2 Double
offsetTrailNaive e r = mconcat . map (pathFromLocTrail . bindLoc (offsetSegment e r))
                     . locatedTrailSegments . (`at` origin)

example :: Diagram B
example = (p # strokeTrail <> offsetTrailNaive 0.1 0.3 p # stroke # lc blue)
        # lw thick
  where p = fromVertices . map p2 $ [(0,0), (1,0.3), (2,0), (2.2,0.3)]
```

First let's consider the outside corner where the adjacent offset
segments do not cross. If we consider sweeping a perpendicular vector
along the original trail we have a problem when we get to a corner. It
is not clear what *perpendicular* means for that point. One solution is
to take all points distance $r$ from the corner point. This puts a
circle around the corner of radius $r$. Better is to just take the
portion of that circle that transitions from what is perpendicular at
the end of the first segment to what is perpendicular at the start of
the next. We could also choose to join together offset segments in other
sensible ways. For the choice of join we have the `_offsetJoin`{.hs}
field in the `OffsetOpts`{.hs} record.

``` {.diagram-code}
import Diagrams.TwoD.Offset

example :: Diagram B
example = (p # strokeTrail <> o # strokeLocTrail # lc blue)
        # lw thick
  where
    p = fromVertices . map p2 $ [(0,0), (1,0.3), (2,0), (2.2,0.3)]
    o = offsetTrail' (with & offsetJoin .~ LineJoinRound) 0.3 p
```

Inside corners are handled in a way that is consistent with outside
corners, but this yields a result that is most likely undesirable.
Future versions of Diagrams will include the ability to clip inside
corners with several options for how to do the clipping.

<div class="todo">

Update after implementing clipping.

</div>

There are other interesting ways we can join segments. We implement the
standard line join styles and will also in the future provide the
ability to specify a custom join.

``` {.diagram-code}
import Diagrams.TwoD.Offset

example :: Diagram B
example = hcat' (with & sep .~ 0.5) $ map f [LineJoinMiter, LineJoinRound, LineJoinBevel]
  where
    f s = p # strokeTrail <> (offsetTrail' (with & offsetJoin .~ s) 0.3 p # strokeLocTrail # lc blue)
    p = fromVertices . map p2 $ [(0,0), (1,0), (0.5,0.7)]
```

The `LineJoinMiter`{.hs} style in particular can use more information to
dictate how long a miter join can extend. A sharp corner can have a
miter join that is an unbounded distance from the original corner.
Usually, however, this long join is not desired. Diagrams follows the
practice of most graphics software and provides a
`_offsetMiterLimit`{.hs} field in the `OffsetOpts`{.hs} record. When the
join would be beyond the miter limit, the join is instead done with a
straight line as in the `LineJoinBevel`{.hs} style. The
`OffsetOpts`{.hs} record then has three parameters:

``` {.haskell}
data OffsetOpts = OffsetOpts
    { _offsetJoin       :: LineJoin
    , _offsetMiterLimit :: Double
    , _offsetEpsilon    :: Double
    }
```

And the type for `offsetTrail'`{.hs} is (`offsetTrail`{.hs} simply uses
the `Default`{.hs} instance for `OffsetOpts`{.hs}):

``` {.haskell}
offsetTrail  ::               Double -> Located (Trail V2 Double) -> Located (Trail V2 Double)
offsetTrail' :: OffsetOpts -> Double -> Located (Trail V2 Double) -> Located (Trail V2 Double)

offsetPath  ::               Double -> Path V2 Double -> Path V2 Double
offsetPath' :: OffsetOpts -> Double -> Path V2 Double -> Path V2 Double
```

Notice this takes a `Trail V2 Double`{.hs} which means it works for both
`Trail' Line V2 Double`{.hs} and `Trail' Loop V2 Double`{.hs}. The
second parameter is the radius for the offset. A negative radius gives a
`Line`{.hs} on the right of the curve, or a `Loop`{.hs} inside a
counter-clockwise `Loop`{.hs}. For `offsetPath`{.hs} we can simply map
`offsetTrail`{.hs} over the trails in the path in the most natural way.

### Expand segments, trails, and paths

Expanding is just like the offset, but instead of producing a curve that
follows one side we follow both sides and produce a `Loop`{.hs} that can
be filled representing all the area within a radius $r$ of the original
curve.

In addition to specifying how segments are joined, we now have to
specify the transition from the offset on one side of a curve to the
other side of a curve. This is given by the `LineCap`{.hs}.

``` {.haskell}
data ExpandOpts = ExpandOpts
    { _expandJoin       :: LineJoin
    , _expandMiterLimit :: Double
    , _expandCap        :: LineCap
    , _expandEpsilon    :: Double
    }

expandTrail  ::               Double -> Located (Trail V2 Double) -> Path V2 Double
expandTrail' :: ExpandOpts -> Double -> Located (Trail V2 Double) -> Path V2 Double

expandPath  ::               Double -> Path V2 Double -> Path V2 Double
expandPath' :: ExpandOpts -> Double -> Path V2 Double -> Path V2 Double
```

The functionality follows closely to the offset functions, but notice
that the result of `expandTrail`{.hs} is a `Path V2 Double`{.hs} where
`offsetTrail`{.hs} resulted in a `Located (Trail V2 Double)`{.hs}. This
is because an expanded `Loop`{.hs} will be a pair of loops, one inside
and one outside. To express this we need a `Path`{.hs}.

``` {.diagram-code}
import Diagrams.TwoD.Offset

example :: Diagram B
example = (p # strokeTrail # lw veryThick # lc white <> e # stroke # lw none # fc blue)
  where
    p = fromVertices . map p2 $ [(0,0), (1,0.3), (2,0), (2.2,0.3)]
    e = expandTrail' opts 0.3 p
    opts = with & expandJoin .~ LineJoinRound
                & expandCap  .~ LineCapRound
```

As long as the expanded path is filled with the winding fill rule we do
not need to worry about having clipping for inside corners. It works out
that the extra loop in the rounded line join will match with the outside
corner. We currently implement all the `LineCap`{.hs} styles, and plan
to support custom styles in future releases.

``` {.diagram-code}
import Diagrams.TwoD.Offset

example :: Diagram B
example = hcat' (with & sep .~ 0.5) $ map f [LineCapButt, LineCapRound, LineCapSquare]
  where
    f s =  p # strokeTrail # lw veryThick # lc white
        <> expandTrail' (opts s) 0.3 p # stroke # lw none # fc blue
    p = fromVertices . map p2 $ [(0,0), (1,0), (0.5,0.7)]
    opts s = with & expandJoin .~ LineJoinRound
                  & expandCap  .~ s
```

### The `TrailLike` class

As you may have noticed by now, a large class of functions in the
standard library---such as `square`{.hs}, `polygon`{.hs},
`fromVertices`{.hs}, and so on---generate not just diagrams, but *any*
type which is an instance of the `TrailLike`{.hs} type class.

The `TrailLike`{.hs} type class, defined in `Diagrams.TrailLike`{.mod},
has only a single method, \`trailLike\`:

``` {.haskell}
trailLike :: Located (Trail (V t)) -> t
```

That is, a trail-like thing is anything which can be constructed from a
`Located Trail`{.hs}.

There are quite a few instances of \`TrailLike\`:

-   \`Trail\`: this instance simply throws away the location.
-   \`Trail' Line\`: throw away the location, and perform `cutLoop`{.hs}
    if necessary. For example, `circle 3 :: Trail' Line V2 Double`{.hs}
    is an open $360^\circ$ circular arc.
-   \`Trail' Loop\`: throw away the location, and perform
    `glueLine`{.hs} if necessary.
-   \`Path\`: construct a path with a single component.
-   \`Diagram b\`: as long as the backend `b`{.hs} knows how to render
    paths, `trailLike`{.hs} can construct a diagram by stroking the
    generated single-component path.
-   \`[Point v]\`: this instance generates the vertices of the trail.
-   `Located (Trail v)`{.hs}, of course, has an instance which amounts
    to the identity function. More generally, however, `Located a`{.hs}
    is an instance of `TrailLike`{.hs} for *any* type `a`{.hs} which is
    also an instance. In particular, the resulting `Located a`{.hs} has
    the location of the input `Located Trail`{.hs}, and a value of type
    `a`{.hs} generated by another call to `trailLike`{.hs}. This is most
    useful for generating values of type `Located (Trail' Line v)`{.hs}
    and `Located (Trail' Loop
    v)`{.hs}. For example,
    `circle 3 # translateX 2 :: Located (Trail' Line
    V2 Double)`{.hs} is an open $360^\circ$ circular arc centered at
    $(2,0)$.

It is quite convenient to be able to use, say, `square 2`{.hs} as a
diagram, path, trail, list of vertices, *etc.*, whichever suits one's
needs. Otherwise, either a long list of functions would be needed for
each primitive (like `square`, `squarePath`, `squareTrail`,
`squareVertices`, `squareLine`, `squareLocatedLine`, ... ugh!), or else
explicit conversion functions would have to be inserted when you wanted
something other than what the `square`{.hs} function gave you by
default.

As an (admittedly contrived) example, the following diagram defines
`s`{.hs} as an alias for `square 2`{.hs} and then uses it at four
different instances of \`TrailLike\`:

``` {.diagram-code}
s = square 2  -- a squarish thingy.

blueSquares = atPoints  (concat . pathVertices $ s) {- 1 -}
                (replicate 4 (s {- 2 -} # scale 0.5) # fc blue)
paths       = lc purple . stroke $ star (StarSkip 2) s {- 3 -}
aster       = center . lc green . strokeLine
            . mconcat . take 5 . iterate (rotateBy (1/5))
            . onLineSegments init
            $ s {- 4 -}
example = (blueSquares <> aster <> paths)
```

Exercise: figure out which occurrence of `s`{.hs} has which type.
(Answers below.)

At its best, this type-directed behavior results in a "it just works/do
what I mean" experience. However, it can occasionally be confusing, and
care is needed. The biggest gotcha occurs when combining a number of
shapes using `(<>)`{.hs} or \`mconcat\`: diagrams, paths, trails, and
lists of vertices all have `Monoid`{.hs} instances, but they are all
different, so the combination of shapes has different semantics
depending on which type is inferred.

``` {.diagram-code}
ts = mconcat . iterateN 3 (rotateBy (1/9)) $ triangle 1
example = (ts ||| strokePath ts ||| strokeLine ts ||| fromVertices ts) # fc red
```

The above example defines `ts`{.hs} by generating three equilateral
triangles offset by 1/9 rotations, then combining them with
`mconcat`{.hs}. The sneaky thing about this is that `ts`{.hs} can have
the type of any `TrailLike`{.hs} instance, and it has completely
different meanings depending on which type is chosen. The example uses
`ts`{.hs} at each of four different monoidal `TrailLike`{.hs} types:

-   Since `example`{.hs} is a diagram, the first `ts`{.hs}, used by
    itself, is also a diagram; hence it is interpreted as three
    equilateral triangle diagrams superimposed on one another with
    `atop`{.hs}.
-   `stroke`{.hs} turns `Path`{.hs}s into diagrams, so the second
    `ts`{.hs} has type `Path V2 Double`{.hs}. Hence it is interpreted as
    three closed triangular paths superimposed into one three-component
    path, which is then stroked.
-   `strokeLine`{.hs} turns `Trail' Line`{.hs}s into diagrams, so the
    third occurrence of `ts`{.hs} has type `Trail' Line V2 Double`{.hs}.
    It is thus interpreted as three open triangular trails sequenced
    end-to-end into one long open trail. As a line (*i.e.* an open
    trail), it is not filled (in order to make it filled we could
    replace `strokeLine
    ts`{.hs} with `strokeLoop (glueLine ts)`{.hs}).
-   The last occurrence of `ts`{.hs} is a list of points, namely, the
    concatenation of the vertices of the three triangles. Turning this
    into a diagram with `fromVertices`{.hs} generates a
    single-component, open trail that visits each of the points in turn.

Of course, one way to avoid all this would be to give `ts`{.hs} a
specific type signature, if you know which type you would like it to be.
Then using it at a different type will result in a type error, rather
than confusing semantics.

Answers to the `square 2`{.hs} type inference challenge:

1.  `Path V2 Double`{.hs}
2.  `Diagram b V2 Double`{.hs}
3.  `[Point V2 n]`{.hs}
4.  `Trail' Line V2 Double`{.hs}

### Segments and trails as parametric objects

Both segments and trails, semantically, can be seen as *parametric
functions*: that is, for each value of a parameter within some given
range (usually $[0,1]$, there is a corresponding vector value (or point,
for `Located`{.hs} segments and trails). The entire collection of such
vectors or points makes up the segment or trail.

The `Diagrams.Parametric`{.mod} module provides tools for working with
segments and trails as parametric functions.

#### Parametric

As explained above, parametric objects can be viewed semantically as
functions. In particular, parametric objects of type `p`{.hs} can be
seen as functions of type `Scalar (V p) -> Codomain p`{.hs}, where the
type family `Codomain`{.hs} is defined in such a way as to make this
true. For example, `Codomain (Trail V2 Double) ~ V2 Double`{.hs},
because a trail can be thought of as a function
`Double -> V2 Double`{.hs}.

The `Parametric`{.hs} class defines the single method `atParam`{.hs}
which yields this parametric view of an object:

``` {.haskell}
atParam :: Parametric p => p -> Scalar (V p) -> Codomain p
```

(Note that it is not possible to convert in the other direction---every
function of type `Scalar (V p) -> Codomain p`{.hs} need not correspond
to something of type `p`{.hs}. For example, to convert from a function
to a trail one would need at the very least a guarantee of continuity;
segments are even more restricted.)

``` {.diagram-code}
spline :: Located (Trail V2 Double)
spline = cubicSpline False [origin, 0 ^& 1, 1 ^& 1, 1 ^& 0] # scale 3
pts = map (spline `atParam`) [0, 0.1 .. 1]
spot = circle 0.2 # fc blue

example = mconcat (map (place spot) pts) <> strokeLocTrail spline
```

Instances of `Parametric`{.hs} include:

-   \`Segment Closed\`: The codomain is the type of vectors. Note there
    is no instance for `Segment Open`{.hs}, since additional context is
    needed to determine the endpoint, and hence the parametrization, of
    an open segment.
-   \`FixedSegment\`: The codomain is the type of points.
-   \`Trail'`: The codomain is the vector space. Note that there is no
    difference between `{.hs}Line\` and `Loop`{.hs}.
-   \`Trail\`: same as the instance for `Trail'`{.hs}.
-   \`Located a\`: as long as `a`{.hs} is also `Parametric`{.hs} and the
    codomain of `a`{.hs} is a vector space, `Located a`{.hs} is
    parametric with points as the codomain. For example, calling
    `atParam`{.hs} on a `Located (Trail V2 Double)`{.hs} returns a
    `P2`{.hs}.

`Path`{.hs}s are *not* `Parametric`{.hs}, since they may have multiple
trail components and there is no canonical way to assign them a
parametrization.

#### DomainBounds

The `domainLower`{.hs} and `domainUpper`{.hs} functions simply return
the lower and upper bounds for the parameter. By default, these will be
$0$ and $1$, respectively. However, it is possible to have objects
parameterized over some interval other than $[0,1]$.

#### EndValues

The `EndValues`{.hs} class provides the functions `atStart`{.hs} and
`atEnd`{.hs}, which return the value at the start and end of the
parameter interval, respectively. In other words, semantically we have
`atStart x = x
\`{.hs}atParam\` domainLower x\`, but certain types may have more
efficient or accurate ways of computing their start and end values (for
example, Bézier segments explicitly store their endpoints, so there is
no need to evaluate the generic parametric form).

#### Sectionable

The `Sectionable`{.hs} class abstracts over parametric things which can
be split into multiple sections (for example, a trail can be split into
two trails laid end-to-end). It provides three methods:

-   `splitAtParam :: p -> Scalar (V p) -> (p, p)`{.hs} splits something
    of type `p`{.hs} at the given parameter into two things of type
    `p`{.hs}. The resulting values will be linearly reparameterized to
    cover the same parameter space as the parent value. For example, a
    segment with parameter values in $[0,1]$ will be split into two
    shorter segments which are also parameterized over $[0,1]$.
-   `section :: p -> Scalar (V p) -> Scalar (V p) -> p`{.hs} extracts
    the subpart of the original lying between the given parameters,
    linearly reparameterized to the same domain as the original.
-   `reverseDomain :: p -> p`{.hs} reverses the parameterization. It
    probably should not be in this class and is likely to move elsewhere
    in future versions.

#### HasArcLength

`HasArcLength`{.hs} abstracts over parametric things with a notion of
arc length. It provides five methods:

-   `arcLengthBounded`{.hs} approximates the arc length of an object to
    within a given tolerance, returning an interval which is guaranteed
    to contain the true arc length.
-   `arcLength`{.hs} is similar to `arcLengthBounded`{.hs}, but returns
    a single length value instead of an interval.
-   `stdArcLength`{.hs} approximates the arc length up to a standard
    accuracy of $\pm 10^{-6}$.
-   `arcLengthToParam`{.hs} converts an arc length to a parameter, up to
    a given tolernace
-   `stdArcLengthToParam`{.hs} is like `arcLengthToParam`{.hs}, but
    using a standard accuracy of $\pm 10^{-6}$.

#### Adjusting length

Anything which is an instance of `DomainBounds`{.hs},
`Sectionable`{.hs}, and `HasArcLength`{.hs} can be "adjusted" using the
`adjust`{.hs} function, which provides a number of options for changing
the length and extent.

#### Computing tangents and normals

The `Diagrams.Tangent`{.mod} module contains functions for computing
tangent vectors and normal vectors to segments and trails, at an
arbitrary parametmer (`tangentAtParam`{.hs}, `normalAtParam`{.hs}) or at
the start or end (`tangentAtStart`{.hs}, `tangentAtEnd`{.hs},
`normalAtStart`{.hs}, `normalAtEnd`{.hs}). (The start/end functions are
provided because such tangent and normal vectors may often be computed
more quickly and precisely than using the general formula with a
parameter of 0 or 1.)

### Splines

Constructing Bézier segments by hand is tedious. The
`Diagrams.CubicSpline`{.mod} module provides the `cubicSpline`{.hs}
function, which, given a list of points, constructs a smooth curved path
passing through each point in turn. The first argument to
`cubicSpline`{.hs} is a boolean value indicating whether the path should
be closed.

``` {.diagram-code}
pts = map p2 [(0,0), (2,3), (5,-2), (-4,1), (0,3)]
spot = circle 0.2 # fc blue # lw none
mkPath closed = position (zip pts (repeat spot))
             <> cubicSpline closed pts
example = mkPath False ||| strutX 2 ||| mkPath True
```

For more precise control over the generation of curved paths, see the
`Diagrams.TwoD.Path.Metafont`{.mod} module from
`diagrams-contrib`{.pkg}, which also has [its own
tutorial](http://hackage.haskell.org/package/SVGFonts).

### Fill rules

There are two main algorithms or "rules" used when determining which
areas to fill with color when filling the interior of a path: the
*winding rule* and the *even-odd rule*. The rule used to draw a
path-based diagram can be set with `fillRule`{.hs}, defined in
`Diagrams.TwoD.Path`{.mod}. For simple, non-self-intersecting paths,
determining which points are inside is quite simple, and the two
algorithms give the same results. However, for self-intersecting paths,
they usually result in different regions being filled.

``` {.diagram-code}
loopyStar = fc red
          . mconcat . map (cubicSpline True)
          . pathVertices
          . star (StarSkip 3)
          $ regPoly 7 1
example = loopyStar # fillRule EvenOdd
      ||| strutX 1
      ||| loopyStar # fillRule Winding
```

-   The *even-odd rule* specifies that a point is inside the path if a
    straight line extended from the point off to infinity (in one
    direction only) crosses the path an odd number of times. Points with
    an even number of crossings are outside the path. This rule is
    simple to implement and works perfectly well for
    non-self-intersecting paths. For self-intersecting paths, however,
    it results in a pattern of alternating filled and unfilled regions,
    as seen in the above example. Sometimes this pattern is desirable
    for its own sake.
-   The *winding rule* specifies that a point is inside the path if its
    *winding number* is nonzero. The winding number measures how many
    times the path "winds" around the point, and can be intuitively
    computed as follows: imagine yourself standing at the given point,
    facing some point on the path. You hold one end of an (infinitely
    stretchy) rope; the other end of the rope is attached to a train
    sitting at the point on the path at which you are looking. Now the
    train begins traveling around the path. As it goes, you keep hold of
    your end of the rope while standing fixed in place, not turning at
    all. After the train has completed one circuit around the path, look
    at the rope: if it is wrapped around you some number of times, you
    are inside the path; if it is not wrapped around you, you are
    outside the path. More generally, we say that the number of times
    the rope is wrapped around you (positive for one direction and
    negative for the other) is the point's winding number.

    <div class="todo">

    Draw a picture of you and the train

    </div>

    For example, if you stand outside a circle looking at a train
    traveling around it, the rope will move from side to side as the
    train goes around the circle, but ultimately will return to exactly
    the state in which it started. If you are standing inside the
    circle, however, the rope will end up wrapped around you once.

    For paths with multiple components, the winding number is simply the
    sum of the winding numbers for the individual components. This
    means, for example, that "holes" can be created in shapes using a
    path component traveling in the *opposite direction* from the outer
    path.

    This rule does a much better job with self-intersecting paths, and
    it turns out to be (with some clever optimizations) not much more
    difficult to implement or inefficient than the even-odd rule.

You should be aware that queries (see `Using queries`{.hs}\_) use the
winding rule by default, and are not affected by the path fill rule
attribute. Thus, if you apply the even-odd rule to a diagram, the query
may not match in the way you expect. For this reason, if you want to
make a shape with holes in it, it is usually better to form the holes
from paths winding in the opposite direction (using `reversePath`{.hs}
and the winding rule) than from the even-odd rule. For example, in the
diagram below, the annulus on the left is formed using the even-odd fill
rule, and the one on the right with the default winding rule and a
reversed inner circle. The dark blue points indicate places where the
associated query evaluates to true.

``` {.diagram-code}
points = [x ^& 0 | x <- [-2.3, -2.1 .. 2.3]]
dia1, dia2 :: Diagram B
dia1 = (circle 2 <> circle 1) # strokePath # fillRule EvenOdd
dia2 = (circle 2 <> reversePath (circle 1)) # strokePath

illustrate d = ((d # fc grey) `beneath`) . mconcat . map drawPt $ points
  where
    drawPt p | getAny (sample d p) = circle 0.1 # fc blue # moveTo p
             | otherwise           = circle 0.07 # fc lightblue # moveTo p

example = illustrate dia1 ||| strutX 1 ||| illustrate dia2
```

If you do want to make a diagram whose query uses the even-odd rule, you
can use the `stroke'`{.hs} function.

### Clipping

With backends that support clipping, paths can be used to *clip* other
diagrams. Only the portion of a clipped diagram falling inside the
clipping path will be drawn.

``` {.diagram-code}
example = square 3
        # fc green
        # lw veryThick
        # clipBy (square 3.2 # rotateBy (1/10))
```

Several functions are available, depending on what envelope and trace
you want the resulting diagram to have. `clipBy`{.hs} uses the envelope
and trace of the original diagram. `clipped`{.hs} uses the envelope and
trace of the clipping path. `clipTo`{.hs} uses the intersection of the
two envelopes, and a trace which matches the displayed outline of the
diagram. Note that in general the intersection of envelopes is larger
than the envelope of an intersection. Diagrams does not have a function
which returns the tight envelope of the intersection.

Altering a diagram's envelope can also be accomplished using
`withEnvelope`{.hs} (see `Envelope-related functions`{.hs}\_). The
`view`{.hs} function is also provided for the special case of setting a
diagram's envelope to some rectangle, often used for the purpose of
selecting only a part of a diagram to be "viewed" in the final output.
It takes a point---the lower-left corner of the viewing rectangle---and
the vector from the lower-left to upper-right corner.

``` {.diagram-code}
circles = (c ||| c) === (c ||| c) where c = circle 1 # fc fuchsia
example = circles # center # view (p2 (-1,-1)) (r2 (1.3, 0.7))
```

Note in the above example how the actual portion of the diagram that
ends up being visible is larger than the specification given to
`view`{.hs}---this is because the aspect ratio of the requested output
image does not match the aspect ratio of the rectangle given to
`view`{.hs} (and also because of the use of `pad`{.hs} by the framework
which renders the user manual examples). If the aspect ratios matched
the viewed portion would be exactly that specified in the call to
`view`{.hs}.

### Trail and path implementation details

Trails are implemented using [finger
trees](http://apfelmus.nfshost.com/articles/monoid-fingertree.html): in
particular, lines are finger trees of closed segments, while loops
consist of a finger tree of closed segments plus a single final open
segment.

The benefit of using a finger tree (instead of just, say, a list, or
even a `Seq`{.hs} structure from `Data.Sequence`{.mod}) is that it
allows caching monoidal "measures" of the entire trail. In particular,
the finger trees underlying trails cache

-   the number of segments
-   the total arc length (up to a standard error tolerance)
-   the total offset (vector from start to end)
-   the envelope

For more details, see the `Diagrams.Segment`{.mod} and
`Diagrams.Trail`{.mod} modules.

Another interesting aspect of the implementation is that upon stroking a
path to form a diagram, instead of simply putting the entire path into a
primitive, we separate out the lines and loops into two path primitives.
This is helpful for backends because they often have to do some active
work to *avoid* filling lines, and if `diagrams-lib`{.pkg} did not do
this separation, they would essentially have to end up doing it
themselves.

Arrows
------

`Diagrams.TwoD.Arrow`{.mod} and `Diagrams.TwoD.Arrowheads`{.mod} provide
specialized functionality for drawing arrows. Note that arrows are drawn
with scale-invariant heads and tails (see `Scale-invariance`{.hs}\_).
Arrows can be used to connect various things including literal points,
named subdiagrams, or their traces. For more detailed information,
examples, and exercises, see the [Arrows tutorial](core.html).

To create arrows, one may use the functions:

-   `arrowBetween`{.hs} to connect points;
-   `connect`{.hs} to connect diagrams;
-   `connectOutside`{.hs} to connect points on the boundary (trace) of
    diagrams (for an example, see the [symmetry
    cube](https://github.com/diagrams/diagrams-haddock/blob/master/README.md)
    example in the gallery);
-   `connectPerim`{.hs} to connect points on the traces of diagrams at
    particular external angles;
-   `arrowAt`{.hs} to place an arrow at a point.
-   `arrowV`{.hs} to create an arrow with the magnitude and direction of
    a given vector.

``` {.diagram-code}
sPt = 0.50 ^& 0.50
ePt = 5.2 ^& 0.50

-- Connect two points.
ex1 = arrowBetween sPt ePt

d = octagon 1 # lc blue # lw ultraThick # showOrigin
ds = d # named "1" ||| strut 3 ||| d # named "2"

-- Connect two diagrams and two points on their trails.
ex23 = ds # connect "1" "2"
          # connectPerim "1" "2" (15/16 @@ turn) (9/16 @@ turn)

-- Place an arrow at (0,0) the size and direction of (0,1).
ex4 = arrowAt (0 ^& 0) unit_Y

example = (ex1
          ===
          strutY 0.5
          ===
          (ex23 <> ex4)) # center
```

Notice that the arrows in the above diagram all have the same dart
shaped head, no tail, and a straight shaft. All of these aspects, and
many others, can be customized using companion functions to the ones
above, whose names end with an apostrophe. For example, the companion to
`connect`{.hs} is `connect'`{.hs}. These companion functions take an
extra `ArrowOpts`{.hs} record, whose fields are:

-   `arrowHead`{.hs} and `arrowTail`{.hs}, to specify the shape of the
    head and tail. The `Diagrams.TwoD.Arrowheads`{.mod} module exports
    the arrowheads `tri`{.hs}, `dart`{.hs}, `spike`{.hs}, `thorn`{.hs},
    `lineHead`{.hs}, and `noHead`{.hs}; the default is `dart`{.hs}. For
    tails we have `quill`{.hs}, `block`{.hs}, `lineTail`{.hs}, and
    `noTail`{.hs}; `noTail`{.hs} is the default. Addtionally, any head
    can be used as a tail by appending a `'`{.hs} (e.g. `dart'`{.hs}).
    There are also functions that can be used to create custom heads and
    tails (see `Diagrams.TwoD.Arrow`{.mod}).
-   `arrowShaft`{.hs} is any `Trail V2 Double`{.hs}; it will be sized
    automatically to fit between the endpoints of the arrow.
-   `headLength`{.hs} and `tailLength`{.hs} specify the size of the head
    and tail, defined as the length of the head or tail plus the joint
    connecting it to the shaft. Their value is of type
    `Measure V2 Double`{.hs} (see `Measurement units`{.hs}\_). The
    default value is `normal`{.hs} which is a synonym for
    `normalized 0.035`{.hs}. A traversal called `lengths`{.hs} sets both
    the `headLength`{.hs} and `tailLength`{.hs} at the same time.
-   `headGap`{.hs} and `tailGap`{.hs} both default to `none`{.hs} and
    are used to indicate the amount of space between the end of the
    arrow and the location it is pointing at. They are also of type
    `Measure V2 Double`{.hs}. A traversal called `gaps`{.hs} is provided
    to set both the `headGap`{.hs} and `tailGap`{.hs} simultaneously.
-   `headStyle`{.hs}, `tailStyle`{.hs} and `shaftStyle`{.hs} are used to
    pass in style functions like `fc blue . opacity 0.75`{.hs} to
    customize parts of the arrow. (By default, the entire arrow,
    including head and tail, is drawn using the current line texture.)

The Lenses `headTexture`{.hs}, `tailTexture`{.hs}, and
`shaftTexture`{.hs} are provided for conveniently setting the texture of
a head or tail. Addtionally, the function `solid`{.hs} converts a color
to a texture. For example, `(with & headTexture .~ solid blue)`{.hs}
will set the head color to blue.

The following example demonstrates the use of various `ArrowOpts`{.hs}.
See `Named subdiagrams`{.hs}\_ for the use of names and the `named`{.hs}
function.

``` {.diagram-code}
c = circle 2 # fc lightgray # lw none # showOrigin

x |-| y = x ||| strutX 3 ||| y

row1 = (c # named "1") |-| (c # named "3")
   |-| (c # named "5") |-| (c # named "7")
row2 = (c # named "2") |-| (c # named "4")
   |-| (c # named "6") |-| (c # named "8")

d = row1 === strutY 5 === row2

shaft1 = trailFromVertices (map p2 [(0, 0), (1, 0), (1, 0.2), (2, 0.2)])
shaft2 = cubicSpline False (map p2 [(0, 0), (1, 0), (1, 0.2), (2, 0.2)])
shaft3 = arc xDir (1/6 @@ turn)

example = d
   # connect' (with & arrowTail .~ quill & lengths .~ large
                    & tailTexture .~ solid orange & headTexture .~ solid orange
                    & arrowHead .~ spike
                    & shaftStyle %~ lw veryThick ) "1" "2"
   # connect' (with & arrowTail .~ thorn' & lengths .~ large
                    & arrowHead .~ thorn
                    & arrowShaft .~ shaft1 & shaftStyle %~ lw veryThick ) "3" "4"
   # connect' (with & arrowTail .~ block & gaps .~ small
                    & arrowHead .~ dart & headLength .~ large
                    & arrowShaft .~ shaft2
                    & headStyle %~ fc blue & tailStyle %~ fc blue
                    & shaftStyle %~ lw veryThick . lc blue ) "5" "6"
   # connect' (with & arrowShaft .~ shaft3
                    & arrowHead .~ tri & headLength .~ large
                    & headStyle %~ fc red . opacity 0.5
                    & shaftStyle %~ lw veryThick . lc black . opacity 0.5 ) "7" "8"
```

Text
----

<div class="warning">

Note: The various backends differ substantially in their text-handling
capabilities. For this and other reasons, there are two ways to add text
to diagrams, each with advantages. The method in this section is heavily
dependant on Backend support. The Cairo Backend has the most complete
support; in particular, this is the best approach for complex
(non-Roman) scripts. You may also want to look at `SVGFonts`{.pkg}
package, described in the section `Native font support`{.hs}\_ below,
which converts text directly as `Path`{.hs}s.

</div>

Text objects, defined in `Diagrams.TwoD.Text`{.mod}, can be created most
simply with the `text`{.hs} function, which turns a `String`{.hs} into a
diagram with (centered) text:

``` {.diagram-code}
example = text "Hello world!" <> rect 8 1
```

Text with different alignments can be created using `topLeftText`{.hs}
or `baselineText`{.hs} (or, more generally, `alignedText`{.hs}, though
it is not supported by all backends---the SVG backend in particular only
supports an approximation to `alignedText`{.hs}):

``` {.diagram-code}
pt = circle 0.1 # fc red

t1 = pt <> topLeftText         "top left"   <> rect 8 1
t2 = pt <> baselineText        "baseline"   <> rect 8 1
t3 = pt <> alignedText 0.7 0.5 "(0.7, 0.5)" <> rect 8 1

d1 =/= d2 = d1 === strutY 2 === d2
example = t1 =/= t2 =/= t3
```

The most important thing to keep in mind when working with text objects
is that they *take up no space*: they have a *point envelope* at the
origin, *i.e.* for the purposes of things like `beside`{.hs}, they have
a width and height of zero. (Note, however, this is not the same as
having an *empty* envelope. In particular, they still behave in an
intuitive manner when included as arguments to things like `hcat`{.hs}.)
If we omitted the rectangle from the above example, there would be no
output.

<div class="warning">

Text objects take up no space!

</div>

The main reason for this is that computing the size of some text in a
given font is rather complicated, and `diagrams` cannot (yet) do it
natively. The cairo backend can do it (see below) but we don't want to
tie diagrams to a particular backend.

Note, however, that the cairo backend includes a module
`Diagrams.Backend.Cairo.Text`{.mod} with functions for querying font and
text extents, and creating text diagrams that take up an appropriate
amount of space. So it *is* possible to have automatically-sized text
objects, at the cost of being tied to the cairo backend and bringing
`IO`{.hs} into the picture (or being at peace with some
probably-justified uses of `unsafePerformIO`{.hs}).

Various attributes of text can be set using `font`{.hs}, `bold`{.hs}
(or, more generally, `fontWeight`{.hs}), `italic`{.hs}, and
`oblique`{.hs} (or, more generally, `fontSlant`{.hs}). Text is colored
with the current fill color (see `Color`{.hs}\_).

``` {.diagram-code}
text' s t = text t # fontSize (local s) <> strutY (s * 1.3)
example = center $
      text' 10 "Hello" # italic
  === text' 5 "there"  # bold # font "freeserif"
  === text' 3 "world"  # fc green
```

### Font size

Font size is set using the `fontSize`{.hs} function, and is specified by
a value of type `Measure V2 Double`{.hs} (see
`Measurement units`{.hs}\_).

-   Text with a `local`{.hs} font size is measured relative to its local
    vector space. Such text is transformed normally by any
    transformations applied to it. For example, in the diagram below,
    `fontSize (local 1)`{.hs} is specified (this is actually the
    default, so it could be omitted without changing the diagram). Note
    how the F's are the same size as a unit box, and scale, stretch, and
    rotate along with it.

    ``` {.diagram-code}
    eff = text "F" <> square 1

    example = hcat
      [eff, eff # scale 2, eff # scaleX 2, eff # scaleY 2, eff # rotateBy (1/12)]
            # fontSize (local 1)
    ```

-   Text whose font size is specified in any measurement other than
    `local`{.hs} (that is, `normalized`{.hs}, `global`{.hs}, or
    `output`{.hs}) behaves differently.

    ``` {.diagram-code}
    eff = text "F" <> square 1

    example = hcat
      [eff, eff # scale 2, eff # scaleX 2, eff # scaleY 2, eff # rotateBy (1/12)]
            # fontSize (normalized 0.1)
    ```

    There are several things to notice about the above example diagram,
    which is identical to the previous one except for the fact that
    `normalized 0.1`{.hs} is used instead of \`local 1\`:

    -   The F's are 1/10th the size of the overall diagram. If we added
        more copies of `eff`{.hs} to the right, but kept the physical
        size of the rendered image the same, the F's would remain the
        same physical size on the screen, but would get bigger relative
        to the boxes (since the boxes would be smaller in absolute
        terms).
    -   The F's are all about the same size---in particular, the uniform
        scaling applied to the second F has no effect, and the fourth F
        is not twice as tall as the others. Note, however, that the
        final F rotates with the square as expected. Note also that the
        third and fourth F's are squished, as one would expect from a
        non-uniform scaling. The hand-wavy slogan is that
        non-`local`{.hs}-sized text is "affected by transformations, but
        without changing size".

        The technical specification is that applying a transformation
        $T$ to non-`local`{.hs}-sized text actually results in applying
        the transformation $T/|T|$, where $|T|$ denotes the *average
        scaling factor* of the transformation $T$, computed as the
        square root of the positive determinant of $T$. This behaves
        nicely: for example, the average scaling factor of `scale
        k`{.hs} is `k`{.hs}, so applying a uniform scaling to
        non-`local`{.hs}-sized text has no effect; it is also
        compositional, so applying `t`{.hs} and then `s`{.hs} to some
        text has exactly the same effect as applying `s <> t`{.hs}. For
        more information, see the `avgScale`{.hs} function and the
        comments associated with its source code.

### Native font support

The [SVGFonts package](http://hackage.haskell.org/package/SVGFonts)
implements native text support for diagrams, using fonts in the SVG
format (note that it can be used with *any* backend, not just the SVG
backend). Among other things, it provides its own `textSVG`{.hs}
function which can be used to convert text into a *path* tracing the
outline of the text. Here is a simple example:

``` {.diagram-code}
import Graphics.SVGFonts

text' d s =
  stroke (textSVG' (TextOpts lin2 INSIDE_H KERN False d d) s)
    # lw none

example = text' 5 "Hello" # fc blue ||| text' 3 "world" # fc green
```

For more details and examples, see the [Haddock
documentation](core.html).

Images
------

The `Diagrams.TwoD.Image`{.mod} module provides basic support for
including both external and embedded images in diagrams. Support for
images varies by backend. Only the cairo backend supports external
images. The rasterific backend supports embedded images of many formats
and the SVG backend supports embedded png images.

To create a diagram from an image file call `loadImageEmb`{.hs} to read
the image from a file path using `JuicyPixels`{.pkg} and return a
`DImage Embedded`{.hs}. Then use `image`{.hs} to convert the
`DImage Embedded`{.hs} to a diagram. You can also create an a diagram
with an embedded image by supplying a function that maps pixel
coordinates to `alphaColour`{.hs} s plus a width and a height to
the\`rasterDia\` function.

The function `loadImageExt`{.hs} checks to make sure the file exists,
uses `JuicyPixels`{.pkg} to determine its size and returns a reference
to the image. On the other hand `uncheckedImageRef`{.hs} simply packages
the reference with a width and height to make a `DImage External`{.hs}.

``` {.diagram-code}
no = (circle 1 <> hrule 2 # rotateBy (1/8))
   # lwO 20 # lc red # frame 0.2
example = do
  res <- loadImageEmb "doc/static/phone.png"
  return $ case res of
    Left err    -> mempty
    Right phone -> no <> image phone # sized (dims2D 1.5 1.5)
```

When using `loadImageEmb`{.hs} and `loadImageExt`{.hs} you do not need
to provide the width and height of the image, as they will be calculated
by `JuicyPixels`{.pkg}. Otherwise you must specify both a width and a
height for each image. In this case you might hope to be able to specify
just a width or just a height, and have the other dimension computed so
as to preserve the image's aspect ratio. However, there is no way for
`diagrams` to query an image's aspect ratio until rendering time, but
(until such time as a constraint solver is added) it needs to know the
size of the image when composing it with other subdiagrams. Hence, both
dimensions must be specified, and for the purposes of positioning
relative to other diagrams, the image will be assumed to occupy a
rectangle of the given dimensions.

However, note that the image's aspect ratio will be preserved: if you
specify dimensions that do not match the actual aspect ratio of the
image, blank space will be left in one of the two dimensions to
compensate. If you wish to alter an image's aspect ratio, you can do so
by scaling nonuniformly with `scaleX`{.hs}, `scaleY`{.hs}, or something
similar.

Currently, the cairo backend can only include images in `png` format,
but hopefully this will be expanded in the future. The rasterific
backend will render many different image formats including, `png`,
`jpg`, `tif`, `bmp` and `gif`.

Advanced tools for diagram creation
===================================

This section covers some of the more advanced tools provided by the core
and standard libraries for constructing diagrams. Most of the content in
this section is applicable to diagrams in any vector space, although 2D
diagrams are used as illustrations.

Envelopes
---------

The `Envelope`{.hs} type, defined in `Diagrams.Core.Envelope`{.mod},
encapsulates *envelopes* (see
`envelopes and local vector spaces`{.hs}\_). Things which have an
associated envelope---including diagrams, segments, trails, and
paths---are instances of the `Enveloped`{.hs} type class.

Envelopes are used implicitly when placing diagrams next to each other
(see `Juxtaposing diagrams`{.hs}\_) or when aligning diagrams (see
`Alignment`{.hs}\_).

### Envelope-related functions

-   `strut`{.hs} creates a diagram which produces no output but takes up
    the same space as a line segment. There are also versions
    specialized to two dimensions, `strutX`{.hs} and `strutY`{.hs}.
    These functions are useful for putting space in between diagrams.

    ``` {.diagram-code}
    example = circle 1 ||| strutX 2 ||| square 2
    ```

-   `pad`{.hs} increases the envelope of a diagram by a certain factor
    in all directions.

    ``` {.diagram-code}
    surround d = c === (c ||| d ||| c) # center === c
      where c = circle 0.5

    example = surround (square 1) ||| strutX 1
          ||| surround (square 1 # pad 1.2)
    ```

    However, the behavior of `pad`{.hs} often trips up first-time users
    of `diagrams`:

    <div class="warning">

    `pad`{.hs} expands the envelope *relative to the local origin*. So
    if you want the padding to be equal on all sides, use `center`{.hs}
    first or use `frame`{.hs} as described next.

    </div>

-   `frame`{.hs} increases the envelope in all directions by a given
    amount measued in local coordinates.

    For example,

    ``` {.diagram-code}
    surround d = c === d === c
      where c = circle 0.5

    s = square 1 # alignB

    p = s # pad 1.2 # showOrigin # center
    q = s # frame 0.2 # showOrigin # center
    r = s # center # showOrigin # pad 1.2

    example = surround p ||| strutX 0.5
          ||| surround q ||| strutX 0.5
          ||| surround r
    ```

-   Envelopes can be "extruded"---like `pad`{.hs}, but only in a certain
    direction---using `extrudeEnvelope`{.hs}. Likewise,
    `intrudeEnvelope`{.hs} does the same but pushes the envelope
    inwards.

    ``` {.diagram-code}
    {-# LANGUAGE ViewPatterns #-}
    import Diagrams.TwoD.Vector
    import Data.Maybe (fromJust)

    sampleEnvelope2D n d = foldr (flip atop) (d # lc red) bs
      where b  = fromJust $ appEnvelope (getEnvelope d)
            bs = [strokePath $ mkLine (origin .+^ (s *^ v))
                                  (5 *^ signorm (perp v))
                 | v <- vs, let s = b v
                 ]
            vs = map r2 [ (2 * cos t, 2 * sin t)
                        | i <- [0..n]
                        , let t = ((fromIntegral i) * 2.0 * pi)
                                / (fromIntegral n)
                        ]
            mkLine a v = moveTo a $ fromOffsets [v] # center

    example
      = square 2
      # extrudeEnvelope (2 ^& 1)
      # sampleEnvelope2D 100
      # center # pad 1.1
    ```

-   Manually setting the envelope of a diagram can be accomplished using
    `withEnvelope`{.hs}. Additionally, `phantom`{.hs} can be used to
    create a diagram which produces no output but takes up a certain
    amount of space, for use in positioning other diagrams.

    ``` {.diagram-code}
    example = hcat [ square 2
                   , circle 1 # withEnvelope (square 3 :: D V2 Double)
                   , square 2
                   , text "hi" <> phantom (circle 2 :: D V2 Double)
                   ]
    ```

    In the above example, `withEnvelope`{.hs} is used to put more space
    surrounding the circle, and `phantom`{.hs} is used to put space
    around `text "hi"`{.hs} (which would otherwise take up no space).
    Note that we could equally well have written
    `text "hi" # withEnvelope (circle 2 :: D V2 Double)`{.hs}. Notice
    that the `D V2 Double`{.hs} annotations are necessary, since
    otherwise GHC will not know what types to pick for `square 3`{.hs}
    and `circle 2`{.hs}. See `No instances
    for Backend b0 V2 Double ...`{.hs}\_ for more information.

-   `Diagrams.TwoD.Size`{.mod} provides functions for extracting
    information from the envelopes of two-dimensional diagrams, such as
    `width`{.hs}, `height`{.hs}, `extentX`{.hs}, `extentY`{.hs}, and
    `center2D`{.hs}.

    It also provides functions `sized`{.hs} and `sizedAs`{.hs}, which
    can be used for changing the size of an object. For example:

    ``` {.diagram-code}
    shapes = circle 1
         ||| square 2
         ||| circle 1 # scaleY 0.3 # sizedAs (square 2 :: D V2 Double)

    example = hrule 1 # sizedAs (shapes # scale 0.5 :: D V2 Double)
           <> shapes # centerX
    ```

### The `Enveloped` class

All objects with an associated envelope are instances of the
`Enveloped`{.hs} type class. This includes diagrams, segments, trails,
and paths. `Enveloped`{.hs} provides a single method,

``` {.haskell}
getEnvelope :: Enveloped b => b -> Envelope (V b)
```

which returns the envelope of an object.

In addition, the list type `[b]`{.hs} is an instance of `Enveloped`{.hs}
whenever `b`{.hs} is. The envelope for a list is simply the combination
of all the individual envelopes of the list's elements---that is, an
envelope that contains all of the list elements. In conjunction with the
`Transformable`{.hs} instance for lists (see
`The Transformable class`{.hs}\_), this can be used to do things such as
apply an alignment to a list of diagrams *considered as a group*. For
some examples and an explanation of why this might be useful, see
`Delayed composition`{.hs}\_.

Traces
------

Envelopes are useful for placing diagrams relative to one another, but
they are not particularly useful for finding actual points on the
boundary of a diagram. Finding points on the boundary of a diagram can
be useful for things like drawing lines or arrows between two shapes, or
deciding how to position another diagram relative to a given one.

Every diagram (and, more generally, anything which is an instance of the
`Traced`{.hs} type class) has a *trace*, a function which is like an
"embedded ray tracer" for finding points on the diagram boundary. In
particular, the trace function takes a *ray* as input (represented by a
pair `(p,v)` of a base point and a vector) and returns a sorted list of
parameters `t` such that `p .+^ (t *^ v)` is a point of intersection
between the ray and the boundary of the diagram.

Normally, a trace is accessed using one of the four functions
`rayTraceV`{.hs}, `rayTraceP`{.hs}, `maxRayTraceV`{.hs}, and
`maxRayTraceP`{.hs}.

-   `rayTraceV`{.hs} takes as inputs a base point `p`, a vector `v`, and
    any instance of `Traced`{.hs}. It looks for intersection points with
    the given object along the ray determined by `p` and `v`, and finds
    the smallest *positive* scalar `t` such that `p .+^ (t *^ v)` is a
    point of intersection between the ray and the boundary of the
    `Traced`{.hs} object. If such a `t` exists, it returns the vector
    from `p` to the intersection point, that is, `t *^ v`. If there is
    no such intersection, `rayTraceV`{.hs} returns `Nothing`.

    Intuitively, restricting to *positive* `t`-values means that only
    intersection points "in front of" the point `p` (that is, in the
    direction of `v`) are considered. This tends to be the most
    intuitive behavior, and parallels the way raytracers work---think of
    `p` as the location of the "camera" and `v` as the direction the
    camera is pointing. If you want to consider negative `t`-values, see
    the `traceV`{.hs} family of functions, described below, or use
    `getTrace`{.hs} to access the list of all intersection parameters
    directly.

    ``` {.diagram-code}
    import Data.Maybe (fromMaybe)

    drawV v = arrowAt origin v

    drawTraceV v d
      = lc green $
        fromMaybe mempty
          ((origin ~~) <$> rayTraceP origin v d)
    illustrateTraceV v d = (d <> drawV v <> drawTraceV v d) # showOrigin

    example = hcat' (with & sep .~ 1)
            . map (illustrateTraceV (0.5 *^ (r2 (1, 1))))
            $ [ circle 1 # translate (r2 (-1.5, -1.5))
              , circle 1
              , circle 1 # translate (r2 (1.5, 1.5))
              ]
    ```

-   `rayTraceP`{.hs} works similarly, except that it returns the point
    of intersection itself, which lies on the boundary of the object, or
    `Nothing` if there is no such point.

    That is, `rayTraceP p v x == Just p'` if and only if
    `rayTraceV p v x == Just (p' .-. p)`.

-   `maxRayTraceV`{.hs} and `maxRayTraceP`{.hs} are similar to
    `rayTraceV`{.hs} and `rayTraceP`{.hs}, respectively, except that
    they look for the *largest* positive `t`-value, that is, the
    *furthest* intersection point in the direction of `v`. Again,
    intersection points in the opposite direction from `v` are not
    considered.
-   The `traceV`{.hs}, `traceP`{.hs}, `maxTraceV`{.hs}, and
    `maxTraceP`{.hs} functions work similarly, but are a bit more
    low-level: they look for the intersection point with the *smallest*
    (respectively *largest*) parameter, even if it is negative.

For even more low-level access, the `Traced`{.hs} class provides the
`getTrace`{.hs} method, which can be used to directly access the trace
function for an object. Given inputs `p` and `v`, it returns a sorted
list of scalars `t` such that `p .+^ (t *^ v)` is a point of
intersection between the ray `(p,v)` and the boundary of the diagram.

The below diagram illustrates the use of the `rayTraceP`{.hs} function
to identify points on the boundaries of several diagrams.

<div class="dia-lhs">

:: \> {-\# LANGUAGE TypeFamilies \#-}

</div>

\> import Data.Maybe (mapMaybe) \> illustrateTrace :: (TrailLike a,
Traced a, Semigroup a, Monoid a, V a \~ V2 Double) =\> a -\> a \>
illustrateTrace d = d \<\> traceLines \> where \> traceLines =
mconcat \> . mapMaybe traceLine \> . iterateN 30 (rotateBy (1/60)) \> \$
unitX \> traceLine v = (basePt \~\~) \<\$\> traceP basePt v d \> basePt
= p2 (0, -2) \> \> example \> = hcat' (with & sep .\~ 1) \> . map
illustrateTrace \> \$ [ square 1 \> , circle 1 \> , triangle 1 \#
rotateBy (-1/4) ||| triangle 1 \# rotateBy (1/4) \> ]

Of course, diagrams are not the only instance of `Traced`{.hs}. Paths
are also `Traced`{.hs}, as are trails, segments, and points. Lists and
tuples are `Traced`{.hs} as long as all of their components are---the
trace for a list or tuple is the combination of all the element traces.

Named subdiagrams
-----------------

Although the simple combinatorial approach to composing diagrams can get
you a long way, for many tasks it becomes necessary (or, at least, much
simpler) to have a way to refer to previously placed subdiagrams. That
is, we want a way to give a name to a particular diagram, combine it
with some others, and then later be able to refer back to the the
subdiagram by name. Any diagram can be given a name with the
`named`{.hs} function.

### User-defined names

Anything can be used as a name, as long as its type is an instance of
the `IsName`{.hs} type class; to be an instance of the `IsName`{.hs}
class, it suffices for a type to be an instance of `Typeable`{.hs},
`Eq`{.hs}, `Ord`{.hs}, and `Show`{.hs}. Making a user-defined type an
instance of `IsName`{.hs} is as simple as:

``` {.haskell}
{-# LANGUAGE DeriveDataTypeable #-}
import Data.Typeable

data Foo = Baz | Bar | Wibble
  deriving (Typeable, Eq, Ord, Show)

instance IsName Foo
```

That's it! No method definitions are even needed for the `IsName`{.hs}
instance, since `toName`{.hs} (the sole method of `IsName`{.hs}) has a
default implementation which works just fine.

<div class="warning">

It is not recommended to use `GeneralizedNewtypeDeriving`{.hs} in
conjunction with `IsName`{.hs}, since in that case the underlying type
and the `newtype` will be considered equivalent when comparing names.
For example:

``` {.haskell}
newtype WordN = WordN Int deriving (Show, Ord, Eq, Typeable, IsName)
```

is unlikely to work as intended, since `(1 :: Int)`{.hs} and
`(WordN 1)`{.hs} will be considered equal as names. Instead, use

``` {.haskell}
newtype WordN = WordN Int deriving (Show, Ord, Eq, Typeable)
instance IsName WordN
```

</div>

### Listing names

Sometimes you may not be sure what names exist within a diagram---for
example, if you have obtained the diagram from some external module, or
are debugging your own code. The `names`{.hs} function extracts a list
of all the names recorded within a diagram and the locations of any
associated subdiagrams.

When using `names`{.hs} you will often need to add a type annotation
such as `Diagram B`{.hs} to its argument, as shown below---for an
explanation and more information, see
`No instances for Backend b0 V2 Double ...`{.hs}\_.

    ghci> names (circle 1 # named "joe" ||| circle 2 # named "bob" :: D V2 Double)
    [("bob",[P (2.9999999999999996 ^& 0.0)]),("joe",[P (0.0 ^& 0.0)])]

Of course, there is in fact an entire subdiagram (or subdiagrams)
associated with each name, not just a point; but subdiagrams do not have
a `Show`{.hs} instance.

### Accessing names

Once we have given names to one or more diagrams, what can we do with
them? The simplest tool for working with names is `lookupName`{.hs},
which has the type

``` {.haskell}
lookupName :: IsName n
           => n -> QDiagram b v m -> Maybe (Subdiagram b v m)
```

This function takes a name and a diagram, and returns the first
subdiagram associated to that name if any are found, and `Nothing`{.hs}
otherwise. (Note that `lookupName`{.hs} is implemented in terms of the
lower-level lookup functions `lookupSub`{.hs} and `subMap`{.hs};
occasionally it may be useful to directly access these lower-level
functions, but the hope is that you shouldn't need to.)

A more sophisticated tool is `withName`{.hs}, which has the (admittedly
scary-looking!) type

``` {.haskell}
withName :: (IsName nm, Metric v , Semigroup m, OrderedField n)
         => nm -> (Subdiagram b v n m -> QDiagram b v n m -> QDiagram b v n m)
         -> QDiagram b v n m -> QDiagram b v n m
```

Let's pick this apart a bit. First, we see that the type `nm`{.hs} must
be a name type. So far so good. The constraints on `v`{.hs} and `n`{.hs}
say that `v n`{.hs} must be a metric space (a vector space with a notion
of distance), and that `n`{.hs} must behave sufficiently like the real
numbers. So the first argument of `withName`{.hs} is a name---that makes
sense. The second argument is a function of type

``` {.haskell}
Subdiagram b v n m -> QDiagram b v n m -> QDiagram b v n m
```

We can see this function as a transformation on diagrams, except that it
also gets to use some extra information---namely, the `Subdiagram
b v n m`{.hs} associated with the name we pass as the first argument to
`withName`{.hs}.

Finally, the return type of `withName`{.hs} is itself a transformation
of diagrams.

So here's how `withName`{.hs} works. Suppose we call it with the
arguments `withName n f d`{.hs}. If some subdiagram of `d`{.hs} has the
name `n`{.hs}, then `f`{.hs} is called with that subdiagram as its first
argument, and `d`{.hs} itself as its second argument. So we get to
transform `d`{.hs} based on information about the given subdiagram, and
its context within the parent diagram `d`{.hs} (for example, its
location, attributes applied to it, and so on). And what if there is no
subdiagram named `n`{.hs} in `d`{.hs}? In that case `f`{.hs} is ignored,
and `d`{.hs} is returned unmodified.

Here's a simple example making use of names to draw a line connecting
the centers of two subdiagrams (though for this particular task it is
probably more convenient to use the provided `connect`{.hs} function):

``` {.diagram-code}
{-# LANGUAGE DeriveDataTypeable #-}
import Data.Typeable

data Foo = Baz | Bar | Wibble
  deriving (Typeable, Eq, Ord, Show)

instance IsName Foo

attach n1 n2
  = withName n1 $ \b1 ->
    withName n2 $ \b2 ->
      atop ((location b1 ~~ location b2) # lc red)

example = (square 3 # named Baz ||| circle 2.3 # named Bar)
        # attach Baz Bar
```

The `attach`{.hs} function takes two names and returns a *function* from
diagrams to diagrams, which adds a red line connecting the locations
denoted by the two names. Note how the two calls to `withName`{.hs} are
chained, and how we have written the second arguments to `withName`{.hs}
using lambda expressions (this is a common style). Finally, we draw a
line between the two points (using the `location`{.hs} function to
access the locations of the subdiagrams within the parent diagram), give
it a style, and specify that it should be layered on top of the diagram
given as the third argument to `attach`{.hs}.

We then draw a square and a circle, give them names, and use
`attach`{.hs} to draw a line between their centers. Of course, in this
example, it would not be too hard to manually compute the endpoints of
the line (this is left as an exercise for the reader); but in more
complex examples such manual calculation can be quite out of the
question.

`withName`{.hs} also has two other useful variants:

-   `withNameAll`{.hs} takes a single name and makes available a list of
    *all* subdiagrams associated with that name. (`withName`{.hs}, by
    contrast, returns only the most recent.) This is useful when you
    want to work with a collection of named subdiagrams all at once.
-   `withNames`{.hs} takes a list of names, and makes available a list
    of the most recent located envelopes associated with each. Instead
    of the two calls to `withName`{.hs} in the example above, we could
    have written

    ``` {.haskell}
    attach n1 n2
      = withNames [n1,n2] $ \[b1,b2] ->
          ...
    ```

There is also a function `place`{.hs}, which is simply a flipped version
of `moveTo`{.hs}, provided for convenience since it can be useful in
conjunction with `withName`{.hs}. For example, to draw a square at the
location of a given name, one can write something like

``` {.haskell}
withName n $ atop . place (square 1) . location
```

This computes the location of the name `n`{.hs}, positions a square at
that location, and then superimposes the positioned square atop the
diagram containing `n`{.hs}.

### Subdiagrams

So far, the examples we have seen have only made use of the local origin
associated with each subdiagram, accessed using the `location`{.hs}
function. However, subdiagrams are full-fledged diagrams, so there is
much more information to be taken advantage of. For example, the below
code draws a tree of circles, using subdiagram traces (see
`Traces`{.hs}\_) to connect the *bottom* edge of the parent circle to
the *top* edge of each child circle, instead of connecting their
centers.

``` {.diagram-code}
import Data.Maybe (fromMaybe)

root   = circle 1 # named "root"
leaves = center
       . hcat' (with & sep .~ 0.5)
       $ map (\c -> circle 1 # named c) "abcde"

parentToChild child
  = withName "root" $ \rb ->
    withName child  $ \cb ->
      atop (boundaryFrom rb unit_Y ~~ boundaryFrom cb unitY)

nodes  = root === strutY 2 === leaves

example = nodes # applyAll (map parentToChild "abcde")
```

Note the use of the `boundaryFrom`{.hs} function, which uses the traces
of the subdiagrams to compute suitable points on their boundary.

### Qualifying names

To avoid name clashes, sometimes it is useful to be able to *qualify*
existing names with one or more prefixes. Names actually consist of a
*sequence* of atomic names, much like Haskell module names consist of a
sequence of identifiers like `Diagrams.TwoD.Shapes`{.mod}.

To qualify an existing name, use the `(|>)`{.hs} operator, which can be
applied not only to individual names but also to an entire diagram
(resulting in all names in the diagram being qualified). To construct a
qualified name explicitly, separate the components with `(.>)`{.hs}.

``` {.diagram-code}
{-# LANGUAGE DeriveDataTypeable #-}
import Data.Typeable

data Corner = NW | NE | SW | SE
  deriving (Typeable, Eq, Ord, Show)
instance IsName Corner

attach n1 n2
  = withName n1 $ \b1 ->
    withName n2 $ \b2 ->
      atop ((location b1 ~~ location b2) # lc red # lw thick)

squares =  (s # named NW ||| s # named NE)
       === (s # named SW ||| s # named SE)
  where s = square 1

d = hcat' (with & sep .~ 0.5) (zipWith (|>) [0::Int ..] (replicate 5 squares))

pairs :: [(Name, Name)]
pairs = [ ((0::Int) .> NE, (2::Int) .> SW)
        , ((1::Int) .> SE, (4::Int) .> NE)
        , ((3::Int) .> NW, (3::Int) .> SE)
        , ((0::Int) .> SE, (1::Int) .> NW)
        ]

example = d # applyAll (map (uncurry attach) pairs)
```

We create a four-paned square with a name for each of its panes; we then
make five copies of it. At this point, each of the copies has the same
names, so there would be no way to refer to any of them individually.
The solution is to qualify each of the copies differently; here we have
used a numeric prefix.

(As an aside, note how we had to use a type annotation on the integers
that we used as names; numeric literals are polymorphic and `(|>)`{.hs}
needs to know what type of atomic name we are using. Without the type
annotations, we would get an [error about an "ambiguous type
variable"](`More%20ambiguity`_). It's a bit annoying to insert all these
annotations, of course; another option would be to use monomorphic
constants like `String`{.hs}s or `Char`{.hs}s instead, or to create our
own data type with a short constructor name that wraps an `Int`{.hs}.)

Note how we also made use of `applyAll`{.hs}, which takes a list of
functions as an argument and composes them into one; that is,
`applyAll [f, g, h] === f . g . h`{.hs}.

### localizing names

In some situations, giving globally unique names to everything (even
with the qualification mechanism) is a big pain. The `localize`{.hs}
function "localizes" the scope of names: any names within a call of
`localize`{.hs} are not visible outside the call.

<div class="todo">

Needs an example.

</div>

Using queries
-------------

Every diagram has an associated *query*, which assigns a value to every
point in the diagram. These values must be taken from some monoid (see
`Semigroups and monoids`{.hs}\_). Combining two diagrams results in
their queries being combined pointwise.

### The default query

The default query assigns a value of type `Any`{.hs} to each point in a
diagram. In fact, `Diagram b v`{.hs} is really a synonym for
`QDiagram b v Any`{.hs}. `Any`{.hs} represents the monoid on the
booleans with logical or as the binary operation (and hence `False`{.hs}
as the identity). The default query simply indicates which points are
"inside" the diagram and which are "outside".

<div class="warning">

The default `Any`{.hs} query and the envelope are quite different, and
may give unrelated results. The envelope is an approximation used to be
able to place diagrams next to one another; the `Any`{.hs} query is a
more accurate record of which points are enclosed by the diagram. (Using
the query in order to position diagrams next to each other more
accurately/snugly would be, generally speaking, computationally
infeasible.)

</div>

The following example queries an ellipse (using the `sample`{.hs}
function to sample it at a set of particular points), coloring points
inside the ellipse red and points outside it blue.

``` {.diagram-code}
import System.Random (randomRIO)
import Control.Monad (replicateM)

c :: Diagram B
c = circle 5 # scaleX 2 # rotateBy (1/14)

mkPoint p = (p, circle 0.3
          	  # lw none
          	  # fc (case sample c p of
          	          Any True  -> red
          	          Any False -> blue
          	       )
            )

rand10 :: IO Double
rand10 = randomRIO (-10,10)

example = do
  points <- replicateM 20 (mkP2 <$> rand10 <*> rand10)
  return $ c <> position (map mkPoint points)
```

### Using other monoids

You can use monoids besides `Any`{.hs} to record other information about
a diagram. For example, the diagram below uses the `Sum`{.hs} monoid to
draw dots whose size is determined by the number of overlapping shapes
at a given point. Note the use of the `value`{.hs} function to switch
from the default `Any`{.hs} to a different monoid: `value v`{.hs}
replaces `Any True`{.hs} with `v`{.hs} and `Any False`{.hs} with
`mempty`{.hs}.

``` {.diagram-code}
import System.Random (randomRIO)
import Control.Monad (replicateM)

withCount = (# value (Sum 1))

c :: QDiagram B V2 Double (Sum Int)
c = (   circle 5 # scaleX 2 # rotateBy (1/14) # withCount
     <> circle 2 # scaleX 5 # rotateBy (-4/14) # withCount
    )

mkPoint p = (p, circle (case sample c p of
                          Sum n  -> 2 * fromIntegral n / 5 + 1/5)
                # fc black
            )

rand10 :: IO Double
rand10 = randomRIO (-10,10)

example = do
  points <- replicateM 20 (mkP2 <$> rand10 <*> rand10)
  return $ c # clearValue <> position (map mkPoint points)
```

Notice also the use of `clearValue`{.hs} to get rid of the custom query;
the program that builds this documentation requires `example`{.hs} to
have the type `QDiagram B V2 Double Any`{.hs}.

As another interesting example, consider using a set monoid to keep
track of names or identifiers for the diagrams at a given point. This
could be used, say, to identify which element(s) of a diagram have been
selected by the user after receiving the coordinates of a mouse click.

### Queries and fill rules

By default, queries use the winding rule (see `Fill rules`{.hs}\_). You
can pass an extra option to the `stroke'`{.hs} function to specify the
even-odd fill rule if you wish. Be aware that queries are unaffected by
applications of the `fillRule`{.hs} attribute, which only affects the
way a diagram is drawn.

Bounding boxes
--------------

Envelopes (see `Envelopes`{.hs}\_) are more flexible and compositional
than bounding boxes for the purposes of combining diagrams. However,
occasionally it is useful for certain applications to be able to work
with bounding boxes, which support fast tests for inclusion as well as
union and intersection operations (envelopes support union but not
inclusion testing or intersection).

To this end, a generic implementation of arbitrary-dimensional bounding
boxes is provided in `Diagrams.BoundingBox`{.mod}. Bounding boxes can be
created from sets of points or from any `Enveloped`{.hs} object, used
for inclusion or exclusion testing, and combined via union or
intersection.

To obtain a rectangle corresponding to a diagram's bounding box, use
`boundingRect`{.hs}.

Scale-invariance
----------------

The `ScaleInv`{.hs} wrapper can be used to create "scale-invariant"
objects. (Note that `ScaleInv`{.hs} is not exported from
`Diagrams.Prelude`{.mod}; to use it, import
`Diagrams.Transform.ScaleInv`{.mod}.) In the diagram below, the
same transformation is applied to each pair of arrows.

<div class="warning">

Diagrams contains native support for drawing arrows (see
`Arrows`{.hs}\_); the arrows in the example below are constructed
manually in order to demonstrate scale-invariance.

</div>

The arrows on the right are wrapped in `ScaleInv`{.hs} but the ones on
the left are not.

``` {.diagram-code}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import Diagrams.Transform.ScaleInv
import Control.Lens ((^.))

class Drawable d where
  draw :: d -> Diagram B

instance Drawable (QDiagram B V2 Double Any) where
  draw = id

instance Drawable a => Drawable (ScaleInv a) where
  draw = draw . (^. scaleInvObj)

instance (Drawable a, Drawable b) => Drawable (a,b) where
  draw (x,y) = draw x <> draw y

arrowhead, shaft :: Diagram B
arrowhead = triangle 0.5 # fc black # rotateBy (-1/4)
shaft = origin ~~ p2 (3, 0)

arrow1 = (shaft,          arrowhead       # translateX 3)
arrow2 = (shaft, scaleInv arrowhead unitX # translateX 3)

showT tr = draw (arrow1 # transform tr)
       ||| strutX 1
       ||| draw (arrow2 # transform tr)

example = vcat' (with & sep .~ 0.5)
            (map (centerX . showT)
              [ scalingX (1/2)
              , scalingY 2
              , scalingX (1/2) <> rotation (-1/12 @@ turn)
              ])
```

In addition, the `scaleInvPrim`{.hs} function creates a scale-invariant
diagram from a primitive (such as a path). At the moment it is not
possible to create a scale-invariant diagram from another *diagram*.

Measurement expressions
-----------------------

<div class="todo">

Go through this section and update it if we merge physical units for
`output`{.hs}.

</div>

There is more to `Measure`{.hs}s (see `Measurement units`{.hs}\_) than
just the four reference frames. In fact, a small domain-specific
language for constructing measurements is provided, with the following
features:

-   `atLeast :: Measure n -> Measure n -> Measure n`{.hs} finds the
    maximum of two measurements. For example,
    `normalized 0.2 \`{.hs}atLeast\` local 1\` evaluates to whichever
    measurement ends up being larger, `normalized 0.2`{.hs} or
    `local 1`{.hs}.

    In fact, the standard line widths like `medium`{.hs}, `thick`{.hs},
    *etc.* are defined as `normalized w \`{.hs}atLeast\` output 0.5\`,
    each with a different value of `w`{.hs} (for example, for
    `medium`{.hs}, `w = 0.004`{.hs}).
-   Similarly, `atMost`{.hs} takes the minimum of two `Measure`{.hs}s.
-   `Measure v`{.hs} is an instance of `AdditiveGroup`{.hs}, which
    provides `zero
    :: Measure v`{.hs}, `negated :: Measure v -> Measure v`{.hs}, and
    `(^+^)`{.hs} for adding measurements. For example,
    `normalized 0.1 ^+^ output 1`{.hs} represents 10% of the width or
    height of the diagram plus one output unit.

The semantics of these expressions is what you would expect: everything
is first converted to compatible units, and then the operations are
interpreted in the obvious way.

Tips and tricks
===============

Using absolute coordinates
--------------------------

Diagrams tries to make it easy to construct many types of graphics while
thinking in only "relative" terms: put this to the right of that; lay
these out in a row; draw this wherever that other thing ended up; and so
on. Sometimes, however, this is not enough, and one really wants to just
think in absolute coordinates: draw this here, draw that there. If you
find yourself wanting this, here are some tips:

-   If you have a list of diagrams which are already correctly
    positioned, you can combine them with `mconcat`{.hs}.
-   The `position`{.hs} function takes a list of diagrams associated
    with positions and combines them while placing them at the indicated
    absolute positions.
-   `juxtapose`{.hs} can be used to position a diagram relative to
    something else without composing them; see `Juxtaposing without
    composing`{.hs}\_.
-   `moveTo`{.hs} can be used to position a single diagram absolutely.
-   `place`{.hs} is a flipped version of `moveTo`{.hs} which is
    sometimes convenient.

Delayed composition
-------------------

Suppose we have four diagrams that we want to lay out relative to one
another. For example:

``` {.diagram-code}
t = triangle   5   # fc orange
s = square     3   # fc red
o = ellipseXY  2 3 # fc blue
c = circle     2   # fc green

d = centerX (t ||| s ||| o ||| c)

example = d
```

(Instead of `(|||)`{.hs} we could equivalently have used `hcat`{.hs}.)
Now `d`{.hs} is the diagram consisting of these four shapes laid out in
a centered row.

But what if we want to do further processing on the individual shapes?
At this point, we are out of luck. There is (currently) no way to break
apart a diagram into subdiagrams once it has been composed together. We
could use `juxtapose`{.hs} (which positions one diagram relative to
another without actually doing any composition) but that would get ugly
and unintuitive.

Here is where the nifty trick comes in: simply enclose each shape in a
list, like so:

``` {.haskell}
ds = centerX ([t] ||| [s] ||| [o] ||| [c])
```

Now `ds`{.hs} is a *list* of four diagrams, which are the same as the
original `t`{.hs}, `s`{.hs}, `o`{.hs}, `c`{.hs} except that they have
been positioned as they would be in `d`{.hs}! We can now go on to do
other things with them individually. For example, we could alter their
positions slightly before composing them (*e.g.* this makes for an easy
way to apply some random "jitter" to a layout):

``` {.diagram-code}
t = triangle   5   # fc orange
s = square     3   # fc red
o = ellipseXY  2 3 # fc blue
c = circle     2   # fc green

ds = centerX ([t] ||| [s] ||| [o] ||| [c])
d' = mconcat $ zipWith translateY [0.5, -0.6, 0, 0.4] ds

example = d'
```

In other words, enclosing diagrams in a list allows them to be
positioned, aligned, *etc.* as they normally would, *except* that it
delays actually composing them!

This works because lists are instances of `Juxtaposable`{.hs},
`Alignable`{.hs}, `Enveloped`{.hs}, `HasOrigin`{.hs}, `HasStyle`{.hs},
`Transformable`{.hs}, and, of course, `Monoid`{.hs}. All these instances
work in the "obvious" way---for example, the envelope for a list is the
combination of the envelopes of the elements---so applying an operation
to a list of diagrams has the same effect as applying the operation to
the composition of those diagrams. In other words, operations such as
`centerX`{.hs}, `scale`{.hs}, `juxtapose`{.hs}, *etc.* all commute with
`mconcat`{.hs}.

Naming vertices
---------------

Most functions that create some sort of shape (*e.g.* `square`{.hs},
`pentagon`{.hs}, `polygon`{.hs}...) can in fact create any instance of
the `TrailLike`{.hs} class (see `TrailLike`{.hs}\_). You can often take
advantage of this to do some custom processing of shapes by creating a
*trail* instead of a diagram, doing some processing, and then turning
the trail into a diagram.

In particular, assigning names to the vertices of a shape can be
accomplished as follows. Instead of writing just (say) `pentagon`{.hs},
write

``` {.haskell}
stroke' ( with & vertexNames .~ [[0..]] ) pentagon
```

which assigns consecutive numbers to the vertices of the pentagon.

Deciphering error messages
--------------------------

Although making `diagrams` an *embedded* domain specific language has
many benefits, it also has (at least) one major downside:
difficult-to-understand error messages. Interpreting error messages
often requires understanding particular details about the internals of
the `diagrams` framework as well as the particular behavior of GHC. This
section attempts to make the situation a bit more palatable by
explaining a few common types of error message you might get while using
`diagrams`, along with some suggestions as to their likely causes and
solutions.

This section is certainly incomplete; please send examples of other
error messages to the [diagrams mailing
list](http://groups.google.com/group/diagrams-discuss?pli=1) for help
interpreting them and/or so they can be added to this section.

### Couldn't match type `V (P2 Double)`{.hs} with `V2 Double`{.hs}

This error is due to what appears to be a bug in 7.6.\* versions of GHC.
For some reason the definition of the `V`{.hs} type family for points is
not exported. To solve this you can add an explicit import of the form
`import Diagrams.Core.Points`{.hs} to the top of your file.

### No instances for Backend b0 V2 Double ...

There will probably come a time when you get an error message such as

    <interactive>:1:8:
        No instances for (Backend b0 V2 Double,
                          Renderable Diagrams.TwoD.Ellipse.Ellipse b0)
          arising from a use of `circle'

The problem really has nothing to do with missing instances, but with
the fact that a concrete backend type has not been filled in for
`b0`{.hs} (or whatever type variable shows up in the error message).
Such errors arise when you pass a diagram to a function which is
polymorphic in its input but monomorphic in its output, such as
`width`{.hs}, `height`{.hs}, `phantom`{.hs}, or `names`{.hs}. Such
functions compute some property of the diagram, or use it to accomplish
some other purpose, but do not result in the diagram being rendered. If
the diagram does not have a monomorphic type, GHC complains that it
cannot determine the diagram's type.

For example, here is the error we get if we try to compute the width of
a radius-1 circle:

    ghci> width (circle 1)

    <interactive>:1:8:
        No instances for (Backend b0 V2 Double,
                          Renderable (Path V2 Double) b0)
          arising from a use of `circle'
        Possible fix:
          add instance declarations for
          (Backend b0 V2 Double, Renderable (Path V2 Double) b0)
        In the first argument of `width', namely `(circle 1)'
        In the expression: width (circle 1)
        In an equation for `it': it = width (circle 1)

GHC complains that it cannot find an instance for "`Backend b0
V2 Double`{.hs}"; what is really going on is that it does not have
enough information to decide which backend to use for the circle (hence
the type variable `b0`{.hs}). This is annoying because *we* know that
the choice of backend cannot possibly affect the width of the circle;
but there is no way for GHC to know that.

The special type `D`{.hs} is provided for exactly this situation,
defined as

``` {.haskell}
type D v n = Diagram NullBackend v n
```

`NullBackend`{.hs} is a "backend" which simply does nothing: perfect for
use in cases where GHC insists on knowing what backend to use but the
backend really does not matter.

For example, the solution to the problem with `width`{.hs} is to
annotate `circle 1`{.hs} with the type `D V2 Double`{.hs}, like so:

    ghci> width (circle 1 :: D V2 Double)
    2.0

### More ambiguity

You may also see error messages that directly complain about ambiguity.
For example, the code below is taken from the example in the section on
`Qualifying names`{.hs}\_:

``` {.haskell}
hcat' ( with & sep .~ 0.5 ) (zipWith (|>) [0 .. ] (replicate 5 squares))
```

It is an attempt to qualify the names in five copies of `squares`{.hs}
with the numbers `0`{.hs}, `1`{.hs}, `2`{.hs}, ... However, it generates
the error shown below:

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
some polymorphic value. Here, the polymorphic values in question are the
numbers `0`{.hs}, `1`{.hs}, ... Numeric literals are polymorphic in
Haskell, so GHC does not know whether they should be `Int`{.hs}s or
`Integer`{.hs}s or `Double`{.hs}s or... The solution is to annotate the
`0`{.hs} with the desired type.

Creating 3D diagrams
====================

`diagrams`' support for three dimensions is growing: currently, modules
`Diagrams.ThreeD.Align`{.mod}, `Diagrams.ThreeD.Camera`{.mod},
`Diagrams.ThreeD.Light`{.mod}, `Diagrams.ThreeD.Shapes`{.mod},
`Diagrams.ThreeD.Transform`{.mod}, `Diagrams.ThreeD.Types`{.mod}, and
`Diagrams.ThreeD.Vector`{.mod} are all included in `diagrams-lib`{.pkg}.
This should still be considered a "feature preview"---in particular,
appropriate 3D backends are still under construction (see
`diagrams-povray`{.repo}). Look for fuller (and more fully documented)
support for 3D diagrams in an upcoming release! In the meantime, consult
the [3D tutorial](3D.html) for a more detailed feature preview.

Animation
=========

Diagrams has experimental support for the creation of *animations*.
Animations are created with the help of a generic `Active`{.hs}
abstraction, defined in the `active`{.pkg} package. Additionally,
animated GIFs can be created using the cairo or rasterific backend.

<div class="warning">

The `active`{.pkg} package is being completely rewritten based on a much
improved semantics. The rewritten version is slated for integration with
an upcoming version of diagrams, Real Soon Now (tm).

</div>

Active
------

The `active`{.pkg} package defines a simple abstraction for working with
*time-varying values*. A value of type `Active a`{.hs} is either a
constant value of type `a`{.hs}, or a time-varying value of type
`a`{.hs} (*i.e.* a function from time to `a`{.hs}) with specific start
and end times. Since active values have start and end times, they can be
aligned, sequenced, stretched, or reversed. In a sense, this is sort of
like a stripped-down version of functional reactive programming (FRP),
without the reactivity.

There are two basic ways to create an `Active`{.hs} value. The first is
to use `mkActive`{.hs} to create one directly, by specifying a start and
end time and a function of time. More indirectly, one can use the
`Applicative`{.hs} instance for `Active`{.hs} together with the "unit
interval" `ui`{.hs}, which takes on values from the unit interval from
time 0 to time 1, or `interval`{.hs}, which is like `ui`{.hs} but over
an arbitrary interval.

For example, to create a value of type `Active Double`{.hs} which
represents one period of a sine wave starting at time 0 and ending at
time 1, we could write

``` {.haskell}
mkActive 0 1 (\t -> sin (fromTime t * tau))
```

or

``` {.haskell}
(sin . (*tau)) <$> ui
```

`pure`{.hs} can also be used to create `Active`{.hs} values which are
constant and have no start or end time. For example,

``` {.haskell}
mod <$> (floor <$> interval 0 100) <*> pure 7
```

cycles repeatedly through the numbers 0-6.

To take a "snapshot" of an active value at a particular point in time,
the `runActive`{.hs} function can be used to turn one into a function of
time. For example,

    > runActive ((sin . (*tau)) <$> ui) $ 0.2
    0.9510565162951535

<div class="todo">

Write more about using the active library. For now, you can read the
[package
documentation](http://hackage.haskell.org/packages/archive/active/latest/doc/html/Data-Active.html)
for more information.

-   Transforming active values
-   Combining active values

</div>

Using Active with diagrams
--------------------------

An animation is defined, simply, as something of type
`Active (Diagram b v)`{.hs} for an appropriate backend type `b`{.hs} and
vector space `v`{.hs}. Hence it is possible to make an animation by
using the `mkActive`{.hs} function and specifying a function from time
to diagrams.

However, most often, animations are constructed using the
`Applicative`{.hs} interface. For example, to create a moving circle we
can write

``` {.haskell}
translateX <$> ui <*> circle 2
```

`diagrams-cairo`{.pkg} includes a very primitive animation rendering
function, `animMain`{.hs}, which takes an animation and spits out a
bunch of image files, one for each frame. You can then assemble the
generated frames into an animation using, *e.g.*, `ffmpeg`. (More
sophisticated animation rendering will be added in future releases.) If
you use `animMain`{.hs} to visualize the above animation, however, you
will find that all the generated frames look the same---the circle is
not moving!

Actually, it *is* moving, it's just that it gets centered in the output
at each instant. It's as if the viewport is panning along at the same
rate as the circle, with the result that it appears stationary. The way
to fix this is by placing the moving circle on top of something larger
and stationary in order to "fix" the viewpoint. Let's use an invisible
square:

``` {.haskell}
(translateX <$> ui <*> circle 2) <> (pure (square 6 # lw none))
```

Notice that we composed two animations using `(<>)`{.hs}, which does
exactly what you would think: superimposes them at every instant in
time.

Since this is such a common thing to want, the
`Diagrams.Animation`{.mod} module provides a function
`animEnvelope`{.hs} for expanding the envelope of an animation to the
union of all the envelopes over time (determined by sampling at a number
of points). That is, the animation will now use a constant envelope that
encloses the entirety of the animation at all points in time.

``` {.haskell}
animEnvelope (translateX <$> ui <*> circle 2)
```

Since `Active`{.hs} is generic, it is also easy (and useful) to create
active `Point`{.hs}s, `Path`{.hs}s, colors, or values of any other type.

<div class="todo">

-   Examples of animating things other than diagrams

</div>

Animated GIFs
-------------

Animated GIFs can be created directly using the cairo backend. This is
done by calling `mainWith`{.hs} with an argument of type `[(Diagram
Cairo V2 Double, GifDelay)]`{.hs} where `GifDelay`{.hs} is a synonym for
`Int`{.hs}. Each tuple is a diagram frame of the animation and a time in
hundredths of a second until the next frame. This creates an executable
which takes an output file with the extension gif. The other command
line options which can be used are `--dither` (to turn on dithering),
`--looping-off`, and `--loop-repeat` (to specify the number of times to
repeat the loop after the first time).

Rendering backends
==================

Diagrams has a system for "pluggable" rendering backends, so new
backends can be added by implementing instances of some type classes.
There are currently four "officially supported" backends, described
below, and several other unofficial or experimental backends. New
backends are welcome! To get started, take a look at the existing
backends for examples, read the section below on `Tools for
backends`{.hs}\_, and consult the [core library
reference](cmdline.html).

The SVG backend
---------------

The SVG backend, `diagrams-svg`{.pkg}, outputs SVG files. It is the
default "out-of-the-box" backend, i.e. what one gets by typing just
`cabal install diagrams`. It is implemented purely in Haskell, with no
dependencies on external libraries via the FFI. This means that it
should be easy to install on all platforms.

Note that at the moment the SVG backend does not yet support embedding
images, but this is planned for a future release. Otherwise, the SVG
backend is on a par with the cairo backend in terms of features
(excluding a few special features specific to the cairo backend,
described above). For information on making use of the SVG backend, see
`Diagrams.Backend.SVG`{.mod}.

Gradient support is complete in this backend however, most browsers to
not handle the SVG spec correctly when it comes to reflect and repeat.
Apparently only Chrome and IE follow the spec correctly at this point,
while Safari does not handle reflect and repeat at all and Firefox gets
it wrong.

The source code for the SVG backend can be found in the
`diagrams-svg`{.repo} repository. Note the functions `renderDia`{.hs}
and `renderSVG`{.hs} for rendering diagrams directly.

The postscript backend
----------------------

The postscript backend, `diagrams-postscript`{.pkg}, like the SVG
backend, is written purely in Haskell. It outputs encapsulated
PostScript (EPS) files. Note that by nature, EPS does not support
transparency. The postscript backend also does not support embedded
images. However, it is fairly complete in its support for other features
with the exception of gradients.

The source code for the postscript backend can be found in the
`diagrams-postscript`{.repo} repository.

The cairo backend
-----------------

The cairo backend, `diagrams-cairo`{.pkg}, is built on top of the
`cairo`{.pkg} package, which contains bindings to the [cairo 2D graphics
library](http://www.cairographics.org/). Although it is quite
full-featured, the cairo library itself can be unfortunately difficult
to install on some platforms, particularly OS X.

The cairo backend can produce PNG, SVG, PDF, postscript, and animated
GIF output. The cairo backend does support gradients however, do to a
bug in the cairo package it does not handle reflect and repeat correctly
for radial gradients,

For specific information on how to make use of it, see the documentation
for the `Diagrams.Backend.Cairo`{.mod} module.

`diagrams-cairo` was the first officially supported backend, and has
quite a few advanced features that other backends do not have:

-   `Diagrams.Backend.Cairo.Text`{.mod} has functions for working with
    text and creating diagrams from text with proper bounding boxes
    (though currently it seems a bit buggy).
-   `Diagrams.Backend.Cairo.List`{.mod} exports the `renderToList`{.hs}
    function, which can convert a 2D diagram to a matrix of pixel color
    values.
-   `Diagrams.Backend.Cairo.Ptr`{.mod} exports functions for rendering
    diagrams directly to buffers in memory.
-   Direct output of animated GIFs.

The source code for the cairo backend can be found in the
`diagrams-cairo`{.repo} repository. The functions `renderDia`{.hs} and
`renderCairo`{.hs} provide an alternative to the
`Diagrams.Backend.Cairo.CmdLine`{.hs} interface for more programmatic
control of the output.

The GTK backend
---------------

The GTK backend, `diagrams-gtk`{.pkg}, used to be part of the cairo
backend (and is still built on top of it), but has been split out into a
separate package in order to reduce the dependencies of the cairo
backend, hence making it easier to install for those who don't need GTK
support. You can install it at the same time as the rest of the diagrams
framework by passing the `-fgtk`{.hs} flag:
`cabal install -fgtk diagrams`, or it can be installed separately later
with `cabal install diagrams-gtk`.

The GTK backend allows rendering diagrams directly to GTK windows
instead of to a file (`defaultRender`{.hs} and `renderToGtk`{.hs}). Note
that it is possible to receive mouse clicks and then query the
corresponding location in a diagram to find out which part the user
clicked on (see `Using queries`{.hs}\_).

The source code for the GTK backend can be found in the
`diagrams-gtk`{.repo} repository.

The Rasterific backend
----------------------

The Rasterific backend is built on top of the `Rasterific`{.pkg}
package, which is a pure haskell rasterizer that uses
`JuicyPixels`{.pkg} and `FontyFruity`{.pkg}. This is a fully featured
backend that supports almost everything that the cairo backend does plus
a few other things. It can produce PNG, JPG, BMP, TIF and animated GIF
images. It also supports embedded images (see `DImage`{.hs}) and
although does not yet have the text handling capabilities of cairo, it
does use the exact text bounding box for alignment. Gradients are fully
supported including, repeat and reflect.

The Rasterific backend can be invoked via
`Diagrams.Backend.Rasterific.CmdLine`{.mod} module, or via the
`renderDia`{.hs}/`renderRasterific`{.hs} functions.

Other backends
--------------

For a list of other backends and their status, see [the diagrams
wiki](http://www.haskell.org/haskellwiki/Diagrams/Projects#Backends).

Tools for backends
------------------

-   `Diagrams.Segment`{.mod} exports a `FixedSegment`{.hs} type,
    representing segments which *do* have an inherent starting location.
    Trails and paths can be "compiled" into lists of
    `FixedSegment`{.hs}s with absolute locations using `fixTrail`{.hs}
    and `fixPath`{.hs}. This is of interest to authors of rendering
    backends that do not support relative drawing commands.
-   A test harness for comparing the outputs of different backends can
    be found in the `diagrams-backend-tests`{.repo} repo; the output of
    the test harness for all officially supported backends is [kept
    up-to-date
    here](http://projects.haskell.org/diagrams/backend-tests/all-index.html).

Other tools
===========

There are several "extra" packages which are officially maintained but
do not automatically come bundled with the `diagrams`{.hs} package.

diagrams-builder
----------------

The `diagrams-builder`{.pkg} package provides a service for *dynamic*
rendering of diagrams---that is, you hand it a `String`{.hs} at runtime
representing some diagrams code, and you get back the result of
rendering the code using whatever backend you like. This could be
useful, for example, as part of a preprocessor tool for interpreting
diagrams code embedded in some other document. Currently it is used by
the `BlogLiterately-diagrams`{.pkg} package (for rendering diagrams
embedded in blog posts) as well as `diagrams-haddock`{.pkg} (for
rendering diagrams embedded in Haddock comments).

diagrams-haddock
----------------

`diagrams-haddock`{.pkg} is a tool for embedding diagrams in Haddock
documentation. The idea is that you can add images (worth 1000+ words,
of course) to your documentation simply by embedding diagrams code in a
special format, and then running `diagrams-haddock`{.pkg}. See the
[README](cmdline.html) for instructions on using it.

SVGFonts
--------

The `SVGFonts`{.pkg} provides support for reading fonts in SVG format
and rendering text to diagrams paths. For more, see
`Native font support`{.hs}\_.

Type reference
==============

This section serves as a reference in understanding the types used in
the diagrams framework.

Understanding diagrams types
----------------------------

Let's look again at the type of `hcat`{.hs}, mentioned in
`Types and type classes`{.hs}\_:

``` {.haskell}
hcat ::
  (InSpace V2 n a, Floating n, Juxtaposable a, HasOrigin a,
   Monoid' a) =>
  [a] -> a
```

This is fairly typical of the types you will encounter when using
diagrams. They can be intimidating at first, but with a little practice
they are not hard to read. Let's look at the components of this
particular type from right to left:

-   `[a] -> a`{.hs}. This part is simple enough: it denotes a function
    from a list of `a`{.hs}'s to a single `a`{.hs}. Typically, the type
    to the right of `=>`{.hs} will be some simple polymorphic type.
-   `TypeableFloat n`{.hs}. This says that the numeric type `n`{.hs}
    must behave like a real number. `TypeableFloat`{.hs} is a type alias
    for the type families `Typeable`{.hs} and `RealFloat`{.hs}, which
    imply `Real`{.hs}, `Floating`{.hs}, `Fractional`{.hs}, `Num`{.hs},
    and `Ord`{.hs}.
-   `V a ~ V2 Double, N a ~ n`{.hs}. These are [type equality
    constraints](http://www.haskell.org/ghc/docs/latest/html/users_guide/equality-constraints.html),
    which say that the types `V a`{.hs} and `V2`{.hs} must be equal, and
    that we will refer to `N a`{.hs} with the type variable `n`{.hs}. In
    this case `V2`{.hs} is the [type of two-dimensional
    vectors](`Basic%202D%20types`_), and `V`{.hs} is a [type
    family](http://www.haskell.org/haskellwiki/GHC/Type_families) which
    tells us the vector space that corresponds to a particular type. So
    `V a ~ V2`{.hs} means "the vector space corresponding to `a`{.hs}
    must be two-dimensional", or more informally, "`a`{.hs} must be a
    type representing two-dimensional things".
-   `Juxtaposable a, ...`{.hs} These are type class constraints on
    `a`{.hs}, specifying what primitive operations `a`{.hs} must support
    in order to be meaningfully used with `hcat`{.hs}. For a complete
    reference on all the type classes used by diagrams, see the next
    section, `Type class
    reference`{.hs}\_.

Type class reference
--------------------

This section serves as a reference for all the type classes defined or
used by diagrams; there are quite a lot. (Some might even say too many!)
Most, if not all, of these are also covered elsewhere, but it is useful
to have them collected all in one place. The declaration of each type
class is shown along with a short explanation, a list of instances, and
links to further reading.

### Classes for transforming and combining

#### HasOrigin

`HasOrigin`{.hs} is defined in `Diagrams.Core.HasOrigin`{.mod}.

``` {.haskell}
class HasOrigin t where
 moveOriginTo :: Point (V t) (N t) -> t -> t
```

`HasOrigin`{.hs} classifies types with a notion of a fixed "location"
relative to some "local origin", and provides a means of moving the
local origin. This is provided as a separate class from
`Transformable`{.hs} since some things with a local origin do not
support other sorts of transformations; and contrariwise some things
that support transformations are translation-invariant (like trails and
vectors) and hence do not have a `HasOrigin`{.hs} instance.

The `moveOriginTo`{.hs} method moves the *local origin* to the given
point.

Instances:

-   The instances for `Point`{.hs}, `SubMap`{.hs}, `Subdiagram`{.hs},
    and `QDiagram`{.hs} all have the meaning you would expect.
-   The instances for `Trace`{.hs}, `Envelope`{.hs}, and `Query`{.hs}
    all obey the invariant that, *e.g.*,
    `getEnvelope . moveOriginTo p t == moveOriginTo p t . getEnvelope`.
    That is, if `e` is the envelope/trace/query for diagram `d`, moving
    the origin of `e` to `p` yields the envelope/trace/query for `d`
    with its origin moved to `p`.
-   Container types can be translated by translating each element
    (`(a,b)`, `[a]`, `Set`{.hs}, `Map`{.hs}).
-   Things wrapped in `TransInv`{.hs} are not supposed to be affected by
    translation, so the `TransInv`{.hs} instance has
    `moveOriginTo = const
    id`{.hs}.
-   The instance for `Transformation`{.hs} constructs a translation and
    composes it appropriately.

Further reading: `Alignment`{.hs}\_.

#### Transformable

`Transformable`{.hs} is defined in `Diagrams.Core.Transform`{.mod}.

``` {.haskell}
class Transformable t where
  transform :: Transformation (V t) (N t) -> t -> t
```

It represents types which support arbitrary affine transformations (or
linear transformations, in the case of translationally invariant
things).

Instances:

-   `Prim`{.hs}, `SubMap`{.hs}, `Subdiagram`{.hs}, \`QDiagram\`: these
    have the meaning you would expect.
-   Of course, `Transformation`{.hs} is itself transformable, by
    composition.
-   Container types can be transformed by transforming each element
    (`(t,t)`, `(t,t,t)`, `[t]`, `Set`{.hs}, `Map`{.hs}).
-   `Point v n` is transformable whenever `v n` is; translations
    actually affect points (whereas they might not have an effect on the
    underlying type `v n`).
-   Anything wrapped in `TransInv`{.hs} will not be affected by
    translation.
-   Anything wrapped in `ScaleInv`{.hs} will not be affected by scaling.
    See `Scale-invariance`{.hs}\_ for more information.
-   Applying a transformation to a `Style`{.hs} simply applies it to
    every attribute.
-   The meaning of transforming an `Attribute`{.hs} depends on the
    particular attribute.
-   The instances for `Trace`{.hs}, `Envelope`{.hs}, and `Query`{.hs}
    all obey the invariant that, *e.g.*,
    `getEnvelope . transform t == transform t . getEnvelope`. That is,
    if `e` is the envelope/trace/query for diagram `d`, transforming `e`
    with `t` yields the envelope/trace/query for `d` transformed by `t`.
-   The instance for `Deletable`{.hs} simply lifts transformations on
    the underlying type.
-   The instance for `NullPrim`{.hs} does nothing, since there is
    nothing to transform.
-   Uniform scales can be applied to `Double`{.hs} and `Rational`{.hs}
    values; translations can also be applied but have no effect.

Further reading: `Euclidean 2-space`{.hs}\_;
`2D Transformations`{.hs}\_.

#### Juxtaposable

`Juxtaposable`{.hs} is defined in `Diagrams.Core.Juxtapose`{.mod}.

``` {.haskell}
class Juxtaposable a where
  juxtapose :: V a -> a -> a -> a
```

`Juxtaposable`{.hs} represents types of things which can be positioned
"next to" one another. Note that this is more general than "having an
envelope" (though certainly any instance of `Enveloped`{.hs} can be made
an instance of `Juxtaposable`{.hs}, using `juxtaposeDefault`{.hs}). For
example, animations are an instance of `Juxtaposable`{.hs} (which
corresponds to juxtaposing them at every point in time), but not of
`Enveloped`{.hs}.

`juxtapose v a1 a2`{.hs} positions `a2`{.hs} next to `a1`{.hs} in the
direction of `v`{.hs}. In particular, it places `a2`{.hs} so that
`v`{.hs} points from the local origin of `a1`{.hs} towards the old local
origin of `a2`{.hs}; `a1`{.hs}'s local origin becomes `a2`{.hs}'s new
local origin. The result is just a translated version of `a2`{.hs}. (In
particular, `juxtapose`{.hs} does not *combine* `a1`{.hs} and `a2`{.hs}
in any way.)

Instances:

:   -   `QDiagram`{.hs} and `Envelope`{.hs} are of course instances.
    -   Many container types are also instances, since container types
        have `Enveloped`{.hs} instances that work by superimposing all
        the envelopes of the individual elements: `[a]`{.hs},
        `(a,b)`{.hs}, `Set`{.hs}, `Map`{.hs}

Further reading: `Juxtaposing diagrams`{.hs}\_;
`Juxtaposing without composing`{.hs}\_.

#### Enveloped

`Enveloped`{.hs} is defined in `Diagrams.Core.Envelope`{.mod}. It
classifies types which have an associated `Envelope`{.hs}.

``` {.haskell}
class (Metric (V a), OrderedField (N a)) => Enveloped a where
  getEnvelope :: a -> Envelope (V a) (N a)
```

The `getEnvelope`{.hs} method simply computes or projects out its
argument's associated `Envelope`{.hs}. `InnerSpace`{.hs}, defined in
`Data.VectorSpace`{.mod}, classifies vector spaces with an inner (dot)
product. Computing envelopes almost always involves projection of one
vector onto another, which requires an inner product. The
`OrderedField`{.hs} class is simply a synonym for a collection of
classes, requiring that the scalar type have multiplicative inverses and
be linearly ordered. See `OrderedField`{.hs}\_.

Instances:

:   -   The instance for `QDiagram`{.hs} does what you would expect.
    -   The instance for `Subdiagram`{.hs} yields an envelope positioned
        relative to the parent diagram.
    -   Every `Point`{.hs} has a "point envelope" consisting of the
        constantly zero envelope translated to the given point. Note
        this is not the same as the empty envelope.
    -   Many container types have instances which work by combining all
        the envelopes of the individual elements: `[a]`{.hs},
        `(a,b)`{.hs}, `Set`{.hs}, `Map`{.hs}.

Further reading: `Envelopes and local vector spaces`{.hs}\_;
`Envelopes`{.hs}\_.

#### Traced

`Traced`{.hs} is defined in `Diagrams.Core.Trace`{.mod}, and plays a
similar role as `Enveloped`{.hs}. `Traced`{.hs} types have an associated
`Trace`{.hs}, which is like an embedded ray tracer that can be used to
find points on the boundary of an object.

``` {.haskell}
class (Ord (N a), Additive (V a)) => Traced a where
  getTrace :: a -> Trace (V a) (N a)
```

Instances:

-   The instance for `QDiagram`{.hs} does what you would expect.
-   The instance for `Subdiagram`{.hs} yields a trace positioned
    relative to the parent diagram.
-   The trace of a `Point`{.hs} is the empty trace.
-   Many container types have instances which work by combining all the
    envelopes of the individual elements: `[a]`{.hs}, `(a,b)`{.hs},
    `Set`{.hs}, `Map`{.hs}.

Further reading: `Traces`{.hs}\_.

### Classes for attributes and styles

#### AttributeClass

`AttributeClass`{.hs}, defined in `Diagrams.Core.Style`{.mod}, is simply
a proxy for `Typeable`{.hs} and `Semigroup`{.hs}; it has no methods. Any
type used as an attribute must be made a member of this class.

``` {.haskell}
class (Typeable a, Semigroup a) => AttributeClass a
```

Instances: many; see `Diagrams.Attributes`{.mod} and
`Diagrams.TwoD.Path`{.mod}.

Further reading: `Attributes and styles`{.hs}\_; `Text`{.hs}\_.

#### HasStyle

`HasStyle`{.hs}, also defined in `Diagrams.Core.Style`{.mod}, classifies
things to which a `Style`{.hs} can be applied.

``` {.haskell}
class HasStyle a where
  applyStyle :: Style (V a) (N a) -> a -> a
```

`applyStyle`{.hs} applies the given `Style`{.hs} to an object, combining
it on the left with the existing `Style`{.hs} (according to the
`Monoid`{.hs} instance of `Style`{.hs}).

Instances:

-   `Style`{.hs} itself is an instance.
-   Many container types are instances as long as their elements are;
    applying a style to a container simply applies the style uniformly
    to every element: `(a,b)`{.hs}, `Map k a`{.hs}, `Set`{.hs},
    `[a]`{.hs}.
-   Functions `(b -> a)`{.hs} are an instance as long as `a`{.hs} is.
    (This can also be thought of as a "container type".)
-   Of course, `QDiagram b v m`{.hs} is an instance, given a few
    restrictions on `v`{.hs} and `m`{.hs}.

Further reading: `Attributes and styles`{.hs}\_; `Text`{.hs}\_.

### Classes for names

#### IsName

`IsName`{.hs} is defined in `Diagrams.Core.Names`{.mod}. It simply
provides the `toName`{.hs} method for converting to `Name`{.hs}, with a
default implementation that wraps up a value as an atomic name. It
allows values of arbitrary types to be used as names for subdiagrams.

``` {.haskell}
class (Typeable a, Ord a, Show a) => IsName a where
  toName :: a -> Name
  toName = Name . (:[]) . AName
```

Instances:

-   Many primitive types such as `()`{.hs}, `Bool`{.hs}, `Char`{.hs},
    `Int`{.hs}, `Float`{.hs}, `Double`{.hs}, `Integer`{.hs},
    `String`{.hs}, `[a]`{.hs}, `(a,b)`{.hs}, `(a,b,c)`{.hs} have a
    default `IsName`{.hs} instance.
-   `AName`{.hs} is an instance; converting an atomic name to
    `Name`{.hs} works by creating a singleton list.
-   `Name`{.hs} is an instance, with `toName`{.hs} as the identity
    function.

Further reading: `Stroking trails and paths`{.hs}\_;
`Named subdiagrams`{.hs}\_; `User-defined names`{.hs}\_.

#### Qualifiable

`Qualifiable`{.hs} is also defined in `Diagrams.Core.Names`{.mod}.
Instances of `Qualifiable`{.hs} are things which can be "qualified" by
prefixing them with a name.

``` {.haskell}
class Qualifiable q where
  -- | Qualify with the given name.
  (|>) :: IsName a => a -> q -> q
```

Instances:

-   \`Name\`: qualifying one name with another is just concatenation.
-   `SubMap`{.hs} and \`QDiagram\`: qualifying prefixes a name on all
    the existing names.

Further reading: `Named subdiagrams`{.hs}\_; `Subdiagrams`{.hs}\_;
`Qualifying
names`{.hs}\_.

### Classes for trails and paths

#### TrailLike

The `TrailLike`{.hs} class, defined in `Diagrams.TrailLike`{.mod},
abstracts over things that are "trail-like", so that functions such as
`square`{.hs} can be used to construct a diagram, a path, a trail,
*etc.*.

``` {.haskell}
class (Metric (V t), OrderedField (N t)) => TrailLike t where

  trailLike
    :: Located (Trail (V t) (N t))  -- ^ The concretely located trail.  Note
                                    --   that some trail-like things
                                    --   (e.g. 'Trail's) may ignore the
                                    --   location.
    -> t
```

The `trailLike`{.hs} method provides a generic way to build a
"trail-like" thing by specifying the low-level trail data. Note that
there should usually not be any need for end users to call
`trailLike`{.hs} directly (though there certainly may be some use
cases).

Instances:

-   \`Trail\`: this instance simply throws away the location.
-   \`Trail' Line\`: throw away the location, and perform `cutLoop`{.hs}
    if necessary. For example, `circle 3 :: Trail' Line V2 Double`{.hs}
    is an open $360^\circ$ circular arc.
-   \`Trail' Loop\`: throw away the location, and perform
    `glueLine`{.hs} if necessary.
-   \`Path\`: construct a path with a single component.
-   \`Diagram b V2 Double\`: as long as the backend `b`{.hs} knows how
    to render 2D paths, `trailLike`{.hs} can construct a diagram by
    stroking the generated single-component path.
-   \`[Point v]\`: this instance generates the vertices of the trail.
-   `Located (Trail v)`{.hs}, of course, has an instance which amounts
    to the identity function. More generally, however, `Located a`{.hs}
    is an instance of `TrailLike`{.hs} for *any* type `a`{.hs} which is
    also an instance. In particular, the resulting `Located a`{.hs} has
    the location of the input `Located Trail`{.hs}, and a value of type
    `a`{.hs} generated by another call to `trailLike`{.hs}. This is most
    useful for generating values of type
    `Located (Trail' Line v Doubl)`{.hs} and `Located (Trail' Loop
    v)`{.hs}. For example,
    `circle 3 # translateX 2 :: Located (Trail' Line
    V2 Double)`{.hs} is an open $360^\circ$ circular arc centered at
    $(2,0)$.
-   `Active t`{.hs} (for any `TrailLike p`{.hs}): creates a constant
    `Active`{.hs} value.

Further reading: `Trails and paths`{.hs}\_; `Trails`{.hs}\_;
`Paths`{.hs}\_; `TrailLike`{.hs}\_.

### Classes for parametric objects

`Diagrams.Parametric`{.mod} provides a set of classes for working with
objects which can be viewed as parametric functions, such as segments
and trails: `Parametric`{.hs}, `DomainBounds`{.hs}, `EndValues`{.hs},
`Sectionable`{.hs}, and `HasArcLength`{.hs}. These classes are fairly
specialized and do not really show up anywhere else; see the section on
`Segments and trails
as parametric objects`{.hs}\_ for more information.

### Classes for backends

#### Backend

The `Backend`{.hs} class, defined in `Diagrams.Core.Types`{.mod},
defines the primary interface for any diagrams rendering backend. Unlike
many of the other type classes in diagrams, it is quite large. For a
full discussion, see the [core library reference](cmdline.html).

#### MultiBackend

`MultiBackend`{.hs}, also defined in `Diagrams.Core.Types`{.mod}, is for
backends which support rendering multiple diagrams, for example to a
multi-page pdf or something similar. It simply provides the
`renderDias`{.hs} function for rendering multiple diagrams at once; the
meaning of this function depends on the backend.

``` {.haskell}
class Backend b v n => MultiBackend b v n where
  renderDias :: (Metric v, OrderedField n, Monoid' m)
             => b -> Options b v n -> [QDiagram b v n m] -> Result b v n
```

So far, the only backend which supports multi-diagram rendering is the
[postscript
backend](http://hackage.haskell.org/package/diagrams-postscript/).

Further reading: `Rendering backends`{.hs}\_.

#### Renderable

The `Renderable`{.hs} type class (from `Diagrams.Core.Types`{.mod}) is a
two-parameter type class connecting backends to primitives which they
know how to render. Backend `B`{.hs} declares that it knows how to draw
primitive `P`{.hs} by giving a `Renderable P B`{.hs} instance, which
requires implementing the `render`{.hs} function which takes a primitive
and renders it.

``` {.haskell}
class Transformable t => Renderable t b where
  render :: b -> t -> Render b (V t) (N t)
```

Instances: There are many instances defined by each backend.

Further reading: `Rendering backends`{.hs}\_.

#### ToResult

The `ToResult`{.hs} class (from `Diagrams.Backend.CmdLine`{.mod})
essentially defines a very generic form of uncurrying. It is used to
implement the general interface for building command-line-driven diagram
generation programs, and in particular to enable building executables
out of (curried) functions, which requires collecting up all their
arguments at once. See the [command-line tutorial](cmdline.html) for
more.

``` {.haskell}
class ToResult d where
    type Args d :: *
    type ResultOf d :: *

    toResult :: d -> Args d -> ResultOf d
```

The most interesting instance is the one for functions:

``` {.haskell}
instance ToResult d => ToResult (a -> d) where
    type Args (a -> d) = (a, Args d)
    type ResultOf (a -> d) = ResultOf d

    toResult f (a,args) = toResult (f a) args
```

#### Parseable

The `Parseable`{.hs} class (`Diagrams.Backend.CmdLine`{.mod}) contains
just one method, `parser :: Parser a`{.hs}, which defines a command-line
parser for a given type. Things with `Parseable`{.hs} instances can be
used in conjunction with the command-line creation framework. See the
[command-line
tutorial](http://www.haskell.org/haskellwiki/GHC/Type_families) for
more.

#### Mainable

The `Mainable`{.hs} class (`Diagrams.Backend.CmdLine`{.mod}) governs
types which can be used to build a command-line-driven
diagram-generation program. For example, a diagram; but also animations,
lists of diagrams, association lists of strings and diagrams, and
functions from parseable things to any of the above. See the
[command-line tutorial](core.html) for more.

``` {.haskell}
class Mainable d where
    type MainOpts d :: *

    mainArgs   :: Parseable (MainOpts d) => d -> IO (MainOpts d)
    mainRender :: MainOpts d -> d -> IO ()
    mainWith   :: Parseable (MainOpts d) => d -> IO ()
```

### Poor man's type synonyms

There are several cases where a certain set of type class constraints
are used together so often that it is convenient to define a synonym to
stand in for the entire set of constraints. In more recent versions of
GHC that support the `ConstraintKinds` extension, this could be
accomplished with a simple type synonym. However, since diagrams still
supports older versions of GHC, these are declared as a new type class
with no methods and a single universal instance. For example,

``` {.haskell}
class (Class1 a, Class2 a, Class3 a) => Synonym a
instance (Class1 a, Class2 a, Class3 a) => Synonym a
```

Ideally, at some point in the future diagrams will drop support for
versions of GHC without `ConstraintKinds` and switch to the more
sensible way of defining constraint synonyms.

#### Monoid'

`Monoid' m`{.hs} is a synonym for

> `(Semigroup m, Monoid m)`{.hs},

defined in `Diagrams.Core`{.mod}. This is something of an unfortunate
hack: although every monoid is a semigroup mathematically speaking,
`Semigroup`{.hs} is not actually a superclass of `Monoid`{.hs}, so if we
want to use both we have to actually declare both.

#### HasBasis

`HasBasis v`{.hs} is a synonym for

> `(Representable v, Rep v ~ E v)`{.hs},

which is used to get access to the basis elements of a vector.

#### HasLinearMap

`HasLinearMap v`{.hs} is a synonym for

> `(Additive v, Applicative v, Traversable v)`{.hs},

which is used for many of the functions related to transforms.

#### OrderedField

`OrderedField s`{.hs}, defined in `Diagrams.Core.Envelope`{.mod}, is a
synonym for

> `(Fractional s, Floating s, Ord s, AdditiveGroup s)`{.hs},

*i.e.* a floating-point type which is totally ordered. When dealing with
`Envelopes`{.hs} it's often necessary to have scalars which support all
four arithmetic operations as well as square root, and can be compared
for ordering.

#### TypeableFloat

`TypeableFloat n`{.hs}, defined in `Diagrams.Core.Types`{.mod}, is a
synonym for

> `(Typeable n, RealFloat n)`{.hs}

which implies `(Real n, Floating n, Fractional n, Num n, Ord n)`{.hs}.
These constraints are needed on many functions that produce diagrams,
due to constraints on transformations and attributes.

#### DataFloat

`DataFloat n`{.hs} is the same as `TypeableFloat n`{.hs}, but
strengthens the `Typeable`{.hs} constraint to `Data`{.hs}.

Type family reference
---------------------

*Type families* are a GHC extension to Haskell enabling "type-level
functions". You can [read about them in detail here](core.html), but
understanding them enough to use them as they arise in diagrams is not
anywhere near as complicated as that page might suggest. Simply put,
type families are functions which can take types as input and produce
other types as output. Of course, in one sense any polymorphic type
constructor already does this: for example, `Maybe`{.hs} takes types as
input (say, `Int`{.hs}) and produces types as output (say,
`Maybe Int`{.hs}). The difference is that `Maybe`{.hs} works *uniformly*
for all input types (it does not, indeed cannot, do anything "different"
for `Char`{.hs} than it does for `Int`{.hs}). Type families, on the
other hand, can have a specific definition for each input type (much as
type class methods can have a different implementation for each instance
type). For example, the following (admittedly contrived) example
declares a type family named `Foo`{.hs}, with two definition clauses.

``` {.haskell}
type family Foo a :: *
type instance Foo Int  = Int
type instance Foo Char = [String]
```

Diagrams only makes use of a few type families, though two of them
(`V`{.hs} and `N`{.hs}) are used quite extensively. The following
sections list each of the type families employed by diagrams

### V

The `V`{.hs} type family is defined in `Diagrams.Core.V`{.hs}. The idea
is that many types have an "associated" vector space, *i.e.* the vector
space in which they "live". The vector space is described by it's
dimension and its numeric type. `V`{.hs} simply maps from types to a
type representing the vector space dimension. For example, `V (Path V2
Double) = V2`{.hs} (ordinary two-dimensional paths live in
`V2 Double`{.hs}), and `V [a] = V a`{.hs} (lists of `a`{.hs}'s live in
whatever vector space `a`{.hs}'s themselves live in).

Often, `V`{.hs} shows up in a constraint on the left hand side of
`=>`{.hs}, as in

``` {.haskell}
alignT :: (Alignable a, HasOrigin a, V a ~ V2, N a ~ n, Floating n) => a -> a
```

This type says that `alignT`{.hs} can be applied to values of any type
`a`{.hs}, *as long as* `a`{.hs} is an instance of `Alignable`{.hs}, and
`a`{.hs} lives in the vector space `V2`{.hs}, that is, `V a ~ V2`{.hs}
(the tilde expresses a *type equality constraint*).

Other times, `V`{.hs} can show up on the right-hand side of `=>`{.hs},
as in

``` {.haskell}
deform :: Deformation (V a) (N a) -> a -> a
```

This says that `deform`{.hs} takes two arguments: a `Deformation`{.hs}
and a value of some type `a`{.hs}. However, `Deformations`{.hs}s are
parameterized by a vector space; `Deformation (V a) (N a)`{.hs} means
that the vector space of the deformation is the vector space associated
to `a`{.hs}. Many types in diagrams are parameterized this way, by
`v`{.hs} and `n`{.hs} parameters which together define a vector space.

### N

The `N`{.hs} type family is defined in `Diagrams.Core.V`{.mod}. Whereas
`V`{.hs} describes the *dimension* of a vector space, `N`{.hs} describes
the scalar value used to represent coördinates or distances in the
space. A "scalar" can be thought of as a distance, or scaling factor.
For example, you can scale a vector by a scalar (using `(*^)`{.hs}), and
the `norm`{.hs} function takes a vector and returns a scalar.

### Render

`Render`{.hs} is an associated data family of the `Backend`{.hs} class.
It determines the type of rendering operations for a given backend. For
more information, see the [core library reference](core.html).

### Result

`Result`{.hs} is an associated type family of the `Backend`{.hs} class.
It determines the type of the final result obtained from the backend
after rendering a complete diagram. For more information, see the
`core library reference`{.hs}\_\_.

### Options

`Options`{.hs} is an associated data family of the `Backend`{.hs} class.
It determines the type of options which can be passed to the backend
when initiating a rendering operation. For more information, see the
`core
library reference`{.hs}\_\_.

### Codomain

`Codomain`{.hs} is a type family defined in `Diagrams.Parametric`{.mod}.
Parametric objects of type `a`{.hs} can be viewed as functions of type
`N a -> Codomain a`{.hs}. For more information, see `Segments and
trails as parametric objects`{.hs}\_.
