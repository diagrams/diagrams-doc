Introduction
============

This tutorial will walk you through the basics of using the diagrams
DSL to create XXX

This is not a Haskell tutorial (although a
Haskell-tutorial-via-diagrams is a fun idea and may happen in the
future).  For now, we recommend [Learn You a
Haskell](http://learnyouahaskell.com/) for a nice introduction to
Haskell; Chapters 1-6 should give you pretty much all you need for
working with diagrams.

Obtaining help
==============

If you need help

Getting started
===============

Before being able to generate beautiful diagrams, you'll need a few things:

1. GHC/the Haskell Platform
1. cairo
1. The diagrams library itself

The Haskell Platform
--------------------

You'll need a recent version (at least 7.0.2) of the Glasgow Haskell
Compiler, as well as some standard libraries and tools.  There are several
methods for obtaining these:

* [The Haskell Platform](http://hackage.haskell.org/platform/) has
everything you need in one convenient package. If you are unsure, you
should use this.

* If you already have GHC and/or know what you are doing and want to
install things yourself, just make sure you have version 7.0.2 or
later of [GHC](http://haskell.org/ghc) and a recent version of the [cabal-install tool](http://hackage.haskell.org/trac/hackage/wiki/CabalInstall).


> {-# LANGUAGE NoMonomorphismRestriction #-}
>
> import Diagrams.Prelude
> import Diagrams.Backend.Cairo.CmdLine


> main = defaultMain mempty