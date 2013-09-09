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

.. container:: todo

  Coming soon!  A tutorial on segments, trails and paths, complete with
  exercises and links to relevant documentation...

As a prerequisite, this tutorial assumes that you are already familiar
with the material in the `tutorial on vectors and points`__.

__ vector.html

Lines
=====

A "line" is a translation-invariant path through space. (Note a line
can be arbitrarily kinked and curved; think of a "train line" or a
"subway line", not a "straight line".)

Lines have type `Trail' Line v` for some vector type `v` (typically
`R2`).  XXX construct lines --- anything which returns
`TrailLike`. `fromOffsets`, `fromVertices`, `fromSegments`.  Also
anything like `pentagon`, `cubicSpline`, etc.

Lines can be turned into diagrams with `strokeLine`.  Lines
are never filled, so setting a fill color has no effect.  (For
filling, see the next section on `Loops`_.)

.. container:: exercises

  1. `fromOffsets`

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

Loops
=====

Trails
======

Paths
=====
