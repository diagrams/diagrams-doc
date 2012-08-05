---
title: Installation
---

Installing
----------

If you already have the [Haskell
Platform](http://hackage.haskell.org/platform/) as well as [cairo](http://www.cairographics.org/), getting diagrams should be as simple as

    cabal install gtk2hs-buildtools diagrams

(You only need to include `gtk2hs-buildtools` if you have never
installed `gtk2hs` or any of its components before -- if you are
unsure, it can't hurt to include it.)

Otherwise, see the [tutorial](/tutorial/DiagramsTutorial.html) and
[the wiki](http://www.haskell.org/haskellwiki/Diagrams/Install) for
more detailed instructions.

You may also want to install `diagrams-contrib`, a package of
user-contributed tools and extensions.

Getting the sources
-------------------

`diagrams` is an open-source project, and contributions are
encouraged!  All diagrams-related repositories are in the
[diagrams organization](http://github.com/diagrams) on github.  The
[Contributing page](http://www.haskell.org/haskellwiki/Diagrams/Contributing)
on the diagrams wiki explains how to get the repositories and make
contributions.
