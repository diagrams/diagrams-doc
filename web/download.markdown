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

Otherwise, see the [tutorial](/tutorial/DiagramsTutorial.html) and
[the wiki](http://www.haskell.org/haskellwiki/Diagrams/Install) for
more detailed instructions.

You may also want to install `diagrams-contrib`, a package of
user-contributed tools and extensions.

Getting the sources
-------------------

`diagrams` is an open-source project, and contributions are
encouraged!  You can get the sources using [darcs](http://darcs.net):

    darcs get http://patch-tag.com/r/byorgey/diagrams-FOO

where `FOO` is one of

  * `core`: the core diagrams framework
  * `lib`: standard library of combinators and utilities
  * `cairo`: rendering backend using cairo
  * `contrib`: collection of user contributions
  * `doc`: documentation, including website, manual, tutorials, etc.
  
In the past, experimental git mirrors were also offered; we hope to be able to
offer them again in the future.

<center>
![](/images/Diagrams.png)
</center>

The packages listed above are only the beginning of the diagrams
world: there are many other related projects
[listed on the wiki](http://www.haskell.org/haskellwiki/Diagrams/Projects).

<!--
 or [git](http://git-scm.com):
 
     git clone git://github.com/byorgey/diagrams-FOO.git

Use whichever you feel more comfortable with. Thanks to
[Owen Stephens's](http://www.owenstephens.co.uk/) great work on
[darcs-bridge](http://wiki.darcs.net/DarcsBridgeUsage), patches/pull
requests are accepted via both!  See the
[bug tracker](http://code.google.com/p/diagrams/issues/list) for a
list of bugs and feature requests.

-->

If you're looking for ways to help, see the
[bug tracker](http://code.google.com/p/diagrams/issues/list) or the [wiki](http://haskell.org/haskellwiki/Diagrams).
