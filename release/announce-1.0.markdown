I am pleased to announce the 1.0 release of
[diagrams](http://projects.haskell.org/diagrams), a framework and
embedded domain-specific language for declarative drawing in Haskell.
Check out the
[gallery](http://projects.haskell.org/diagrams/gallery.html) for some
examples of what it can do!  Brent gave a talk at the New York Haskell
Users' Group presenting the new release: you can find the
[video here](XXX) and the [slides here](http://www.cis.upenn.edu/~byorgey/pub/13-11-25-nyhaskell-diagrams.pdf).

XXX include some example images?

Highlights of this release include:

* Support for drawing arrows between given points or between diagrams,
  with many options for customization
  ([tutorial](http://projects.haskell.org/diagrams/doc/arrow.html),
  [documentation](http://projects.haskell.org/diagrams/doc/manual.html#arrows),
  [API](http://projects.haskell.org/diagrams/haddock/Diagrams-TwoD-Arrow.html)).

* New framework for creating custom command-line-driven executables
  for diagram generation
  ([tutorial](http://projects.haskell.org/diagrams/doc/cmdline.html), [API](http://projects.haskell.org/diagrams/haddock/Diagrams-Backend-CmdLine.html)).

* Offsets of trails and paths, *i.e.* compute the trail or path lying
  a constant distance from the given one
  ([documentation](http://projects.haskell.org/diagrams/doc/manual.html#offsets-of-segments-trails-and-paths),
  [API](http://projects.haskell.org/diagrams/haddock/Diagrams-TwoD-Offset.html)).

* Tangent and normal vectors of segments and trails ([API](http://projects.haskell.org/diagrams/haddock/Diagrams-Tangent.html)).

* Alignment can now be done by trace in addition to envelope ([API](http://projects.haskell.org/diagrams/haddock/Diagrams-TwoD-Align.html)).

* `lens`es are now used consistently for record fields throughout the
  library ([documentation](http://projects.haskell.org/diagrams/doc/manual.html#faking-optional-named-arguments)).

* Across-the-board improvements in performance and size of generated files.

Try it out
----------

For the truly impatient:

    cabal install diagrams

Diagrams is supported under GHC 7.4 and 7.6.

To get started with diagrams, read the
[quick tutorial](http://projects.haskell.org/diagrams/doc/quickstart.html),
which will introduce you to the fundamentals of the framework and
provide links for further reading.

For those who are less impatient and want to really dig in and
use the power features, read the
[user manual](http://projects.haskell.org/diagrams/doc/manual.html).

Get involved
------------

Subscribe to the
[project mailing list](http://groups.google.com/group/diagrams-discuss),
and/or come hang out in the `#diagrams` IRC channel on freenode.org
for help and discussion.  Development continues stronger than ever,
and there are a wide range of projects available for new contributors
of all levels of Haskell skill.  Make some diagrams.
[Fix some bugs](http://github.com/diagrams/). Submit your cool
examples for inclusion in the
[gallery](http://projects.haskell.org/diagrams/gallery.html) or your
cool code for inclusion in the
[diagrams-contrib](http://hackage.haskell.org/package/diagrams%2Dcontrib)
package!

Happy diagramming!

Brought to you by the diagrams team:

* Daniel Bergey
* Jeff Rosenbluth
* Ryan Yates
* Brent Yorgey

with contributions from:

* Jan Bracker
* Conal Elliott
* Daniil Frumin
* Sam Griffin
* Niklas Haas
* Peter Hall
* Claude Heiland-Allen
* Deepak Jois
* John Lato
* Felipe Lessa
* Chris Mears
* Ian Ross
* Carlos Scheidegger
* Vilhelm Sjöberg
* Michael Sloan
* Jim Snavely
* Luite Stegeman
* Kanchalai Suveepattananont
* Michael Thompson
* Scott Walck
