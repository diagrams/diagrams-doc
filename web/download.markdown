---
title: Download
---

Installing
----------

If you already have the [Haskell
Platform](http://hackage.haskell.org/platform/) as well as [cairo](http://www.cairographics.org/), getting diagrams should be as simple as

    cabal install gtk2hs-buildtools diagrams

(You only need to include `gtk2hs-buildtools` if you have never
installed `gtk2hs` or any of its components before -- if you are
unsure, it can't hurt to include it.)

Otherwise, see the [tutorial](/tutorial/DiagramsTutorial.html) for more detailed
instructions.

Getting the sources
-------------------

`diagrams` is an open-source project, and contributions are
encouraged!  You can get the sources using either [darcs](http://darcs.net) or [git](http://git-scm.com):

    darcs get http://patch-tag.com/r/byorgey/diagrams-FOO
    git clone git://github.com/byorgey/diagrams-FOO.git

where `FOO` is one of

  * `core`: the core diagrams framework
  * `lib`: standard library of combinators and utilities
  * `cairo`: rendering backend using cairo
  * `doc`: documentation, including website, manual, tutorials, etc.

Use whichever you feel more comfortable with. Thanks to
[Owen Stephens's](http://www.owenstephens.co.uk/) great work on
[darcs-bridge](http://wiki.darcs.net/DarcsBridgeUsage), patches/pull
requests are accepted via both!  See the
[bug tracker](http://code.google.com/p/diagrams/issues/list) for a
list of bugs and feature requests.



