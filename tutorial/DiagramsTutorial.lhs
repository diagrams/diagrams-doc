Introduction
============

This tutorial will walk you through the basics of using the diagrams
DSL to create graphics in a powerful, modular, declarative way.

This is not a Haskell tutorial (although a
Haskell-tutorial-via-diagrams is a fun idea and may happen in the
future).  For now, we recommend [Learn You a
Haskell](http://learnyouahaskell.com/) for a nice introduction to
Haskell; Chapters 1-6 should give you pretty much all you need for
working with diagrams.

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

Before being able to generate beautiful diagrams, you'll need a few things:

GHC/The Haskell Platform
--------------------

You'll need a recent version (at least 7.0.2) of the [Glasgow Haskell
Compiler](http://haskell.org/ghc), as well as some standard libraries
and tools.  There are several methods for obtaining these:

* [The Haskell Platform](http://hackage.haskell.org/platform/) has
everything you need in one convenient package. If you are unsure, you
should use this.

* If you already have GHC and/or know what you are doing and want to
install things yourself, just make sure you have version 7.0.2 or
later of [GHC](http://haskell.org/ghc) and a recent version of the [cabal-install tool](http://hackage.haskell.org/trac/hackage/wiki/CabalInstall).

Cairo
-----

Unlike its previous incarnation, diagrams is designed with modularity
in mind, and makes it extremely easy to plug in new rendering
backends.  At the moment, however, the only seriously supported
rendering backend is [Cairo](http://cairographics.org/).  If you don't
already have it installed, you will need to [install the Cairo
development libraries](http://cairographics.org/download/).

Unfortunately, the Haskell Cairo bindings currently do not work on
Windows.  

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

* `diagrams-cairo` is a backend which renders diagrams using Cairo.

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
Cairo backend for rendering diagrams.  Among other things, it provides
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

A few things to note: in addition to PDF, the Cairo backend can
generate `.png`, `.ps`, and `.svg` formats; the format to use is
determined automatically by the extension of the output file name.
There are several more options besides `-o`; you can see what they are
by typing `./DiagramsTutorial --help`.

