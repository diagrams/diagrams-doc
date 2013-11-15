.. role:: pkg(literal)
.. role:: hs(literal)
.. role:: mod(literal)
.. role:: repo(literal)

.. default-role:: hs

================
Trails and paths
================

.. contents::

Introduction
============

The system diagrams uses for constructing paths can seem overly
complex at first---there are many different types involved, and a
plethora of ways to convert between them.  However, it is the result
of deep thought and careful design---that's not to say it is perfect,
but for the most part, things are the way they are for a reason. The
concept of a "path" has quite a few inherent subtleties!

The goal of this tutorial, therefore, is to give you proficiency in
working with trails and paths via practice on numerous
exercises. Along the way, we will also motivate some of the design
decisions. This tutorial is not intended to be a comprehensive
reference; for that, consult the `user manual`_.  As a prerequisite,
you should already be familiar with the material in the `tutorial on
vectors and points`__.

.. _`user manual`: manual.html#trails-and-paths
__ vector.html

Segments
========

The basic building block of diagrams paths is the *segment*.  For the
purposes of this tutorial, however, it is enough to know that a
segment can be either a straight line segment or a (cubic Bézier)
curve, and that the lines, loops, trails, and paths we will meet in
the following sections are all built out of segments.  Typically, in
creating diagrams, one does not need to work directly with
segments. Consult the `user manual`__ if you wish to know more about
them.

__ manual.html#segments

Lines
=====

The first foundational concept to understand is that of a *line*,
which is a *translation-invariant path through space*, consisting of a
sequence of segments end-to-end. Note that a line can be arbitrarily kinked and
curved. Think not of a "straight line", but rather of a "train line"
or a "subway line".

A line is a kind of *trail* (we will meet the other kind in the
following section), and has type `Trail' Line v` for some vector type
`v` (typically `R2`).

Constructing lines
------------------

Lines are an instance of the `TrailLike` class, so to construct a
line, you can use any function with a return type like `TrailLike t =>
... -> t`. Examples of such functions include (click a function name
to see its type, its documentation, and other nearby functions):
`fromOffsets`, `fromVertices`, `fromSegments`, `(~~)`, `triangle`,
`square`, `pentagon`, `rect`, `roundedRect`, `polygon`, and
`cubicSpline`.

A line can be turned into a diagram with `strokeLine`. Since lines are
*translation-invariant*, they have no definite location in space, and
`strokeLine` must arbitrarily pick a location; by default, the origin
is chosen as the starting location of the line. (Of course, in many
situations this does not matter.)

.. container:: warning

  Lines are never filled, so setting a fill color on a line has no
  effect.  (For things that do get filled, see the next section on
  `Loops`_.)

Below are a series of diagrams that you should attempt to reproduce.
Each exercise lists functions that you may find useful in completing
it.  If you are really stuck or want to check your answers, you can
find the source code for this tutorial on github.

.. container:: exercises

  #. `fromOffsets`

     .. class:: dia

     ::

     > example
     >   = fromOffsets [unitX, 2 *^ unitY, 2 *^ unitX]
     >   # strokeLine
     >   # centerXY # pad 1.1

  #. `lineOffset`, `direction`

     .. class:: dia

     ::

     > ln    = fromOffsets [unitX, 2 *^ unitY, 2 *^ unitX]
     > theta = direction (lineOffset ln)
     > example
     >   = ln # rotateBy (negate theta)
     >   # strokeLine
     >   # centerXY # pad 1.1

  #. `fromVertices`

     .. class:: dia

     ::

     > pts = [ p2 (x,y) | x <- [0 .. 4], y <- [0, 1] ]
     > example = pts # fromVertices # strokeLine
     >         # centerXY # pad 1.1

  #. `pentagon`

     .. class:: dia

     ::

     > example = pentagon 1 # strokeLine # centerXY # pad 1.1

  #. `pentagon`, `onLineSegments`

     .. class:: dia

     ::

     > example = pentagon 1 # onLineSegments tail # strokeLine
     >         # centerXY # pad 1.1

Composing lines
---------------

A very important feature of lines is that they are an instance of
`Monoid`, with the empty line (containing no segments) as `mempty`,
and concatenation of lines as `mappend` (aka `<>`).

.. container:: exercises

  #. `mappend`

     .. class:: dia

     ::

     > p = pentagon 1 # onLineSegments init
     > example = (p <> p) # strokeLine
     >         # centerXY # pad 1.1

  #. `iterateN`, `rotateBy`, `mconcat`

     .. class:: dia

     ::

     > p = pentagon 1 # onLineSegments init
     > example = iterateN 5 (rotateBy (1/5)) p
     >    # mconcat # strokeLine # centerXY # pad 1.1 # sized (Width 2)

  #. `reverseLine`

     .. class:: dia

     ::

     > hanoi 0 = mempty
     > hanoi n = mconcat
     >   [ h' # rotateBy (-1/3) # reverseLine
     >   , fromOffsets [unitX] # rotateBy (1/6)
     >   , h'
     >   , fromOffsets [unitX] # rotateBy (-1/6)
     >   , h' # rotateBy (1/3) # reverseLine
     >   ]
     >   where h' = hanoi (n-1)
     >
     > example = hanoi 4 # strokeLine
     >   # centerXY # pad 1.1 # sized (Width 2)

Loops
=====

A *loop* is another kind of trail, with type `Trail' Loop R2`.  Loops
are like lines, except for the fact that they are "closed": they end
in the same place where they start, and have an "inside" and an
"outside".

Constructing loops
------------------

Loops are also an instance of `TrailLike`, so many of the same
functions mentioned in the previous section for constructing lines can
also be used to construct loops.

Loops can be turned into diagrams with `strokeLoop`.

.. container:: exercises

  #. `strokeLoop`

     .. class:: dia

     ::

     > example = pentagon 1 # strokeLine # fc blue # centerXY # pad 1.1

  #. Change `strokeLoop` to `strokeLine` in your solution to the
     previous exercise.  Explain the difference in the output.

  #. XXX more exercises here

Converting between lines and loops
----------------------------------

* `glueLine`
* `closeLine`
* `cutLoop`

Note that unlike lines, loops *do not* have a `Monoid` instance.

.. container:: exercises

  #. `glueLine`

     .. class:: dia

     ::

     > p = pentagon 1 # onLineSegments init
     > example = iterateN 5 (rotateBy (1/5)) p
     >    # mconcat # glueLine # strokeLoop
     >    # fc green # centerXY # pad 1.1 # sized (Width 2)

Trails
======

Located
=======

Paths
=====

Hints
=====

