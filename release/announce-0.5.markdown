I am pleased to announce the release of version 0.5 of
[diagrams](http://projects.haskell.org/diagrams), a full-featured
framework and embedded domain-specific language for declarative
drawing.  Check out the
[gallery](http://projects.haskell.org/diagrams/gallery.html) for
examples of what it can do!

Highlights of this release include:

  * A new
    [diagrams-contrib](http://hackage.haskell.org/package/diagrams%2Dcontrib)
    package of user-contributed modules, which so far contains code
    for tree drawing, Apollonian gaskets, planar tilings, "wrapped"
    layout, and turtle graphics.

  * Experimental support for animation, built on top of the new
    [active](http://hackage.haskell.org/package/active) library.

  * Numerous small additions and improvements, including more general
    rounded rectangle shapes and better text support.

  * Much better performance in some common situations, such as laying
    out a very long list of diagrams using 'cat' and related
    combinators.

  * Tested with GHC 6.12, 7.0. 7.2, and 7.4.

See the [release notes](http://projects.haskell.org/diagrams/releases.html) for
complete details, and the
[diagrams wiki](http://www.haskell.org/haskellwiki/Diagrams/Migrate0.5)
for help migrating from 0.4 to 0.5 (changes should be minimal).

Try it out
----------

For the truly impatient:

    cabal install gtk2hs-buildtools
    cabal install diagrams

Diagrams is fully supported under GHC 6.12, 7.0, 7.2, and 7.4.
However, getting cairo to build can be tricky on some platforms; see
the [diagrams wiki](http://www.haskell.org/haskellwiki/Diagrams) for
more information and workarounds regarding specific platforms.  (A new
native SVG backend is in the works, targeted for the 0.6 release.)

To get started with diagrams, read the
[quick tutorial](http://projects.haskell.org/diagrams/tutorial/DiagramsTutorial.html),
which will introduce you to the fundamentals of the framework.

For those who are even less impatient but want to really dig in and
use the power features, read the
[user manual](http://projects.haskell.org/manual/diagrams-manual.html).

Get involved
------------

Subscribe to the
[project mailing list](http://groups.google.com/group/diagrams-discuss),
and/or come hang out in the `#diagrams` IRC channel on freenode.org
for help and discussion.  Make some diagrams.
[Fix some bugs](http://code.google.com/p/diagrams/issues/list). Submit
your cool examples for inclusion in the
[gallery](http://projects.haskell.org/diagrams/gallery.html) or your
cool code for inclusion in the
[diagrams-contrib](http://hackage.haskell.org/package/diagrams%2Dcontrib)
package!

Happy diagramming!

Brought to you by the diagrams team:

* Peter Hall
* Ian Ross
* Michael Sloan
* Ryan Yates
* Brent Yorgey

with contributions from:

* Sam Griffin
* Claude Heiland-Allen
* John Lato
* Vilhelm Sjöberg
* Luite Stegeman
* Kanchalai Suveepattananont
* Scott Walck
