% Diagrams 1.0

    [BLOpts]
	profile = wp
	postid = 1161
	categories = diagrams, haskell, projects
	tags = 1.0, release, announcement

The diagrams team is very pleased to announce the 1.0 release of
[diagrams](http://projects.haskell.org/diagrams), a framework and
embedded domain-specific language for declarative drawing in
Haskell. Check out the
[gallery](http://projects.haskell.org/diagrams/gallery.html) for some
examples of what it can do. Diagrams can be used for a wide range of
purposes, from
[data visualization](http://idontgetoutmuch.wordpress.com/2013/10/23/parking-in-westminster-an-analysis-in-haskell/)
to
[illustration](https://www.fpcomplete.com/user/edwardk/cellular-automata/part-1)
to
[art](http://mathlesstraveled.com/2013/04/06/stars-of-the-minds-sky-with-diagrams/),
and diagrams code can be seamlessly embedded in
[blog posts](http://byorgey.wordpress.com/2012/08/28/creating-documents-with-embedded-diagrams/),
[LaTeX documents](http://projects.haskell.org/diagrams/doc/latex.html),
and
[Haddock documentation](http://byorgey.wordpress.com/2013/03/23/introducing-diagrams-haddock/),
making it easy to incorporate diagrams into your documents with
minimal extra work.

 <div style="text-align: center;">
[![](http://projects.haskell.org/diagrams/gallery/images/Chart.thumb.png)](http://projects.haskell.org/diagrams/gallery/Chart.html)
 &nbsp;&nbsp;&nbsp;
[![](http://projects.haskell.org/diagrams/gallery/images/KnightTour.thumb.png)](http://projects.haskell.org/diagrams/gallery/KnightTour.html)
 &nbsp;&nbsp;&nbsp;
[![](http://projects.haskell.org/diagrams/gallery/images/SquareLimit.thumb.png)](http://projects.haskell.org/diagrams/gallery/SquareLimit.html)
 </div>

What's new
----------

Brent recently gave a talk at the
[New York Haskell Users' Group](http://www.meetup.com/NY-Haskell/)
presenting the new release.  You can find videos of the talk on vimeo:
[part 1 presents a basic introduction to the library](http://vimeo.com/84104226),
and
[part 2 talks about mathematical abstraction and DSL design](http://vimeo.com/84249042).
The
[slides are also available](http://www.cis.upenn.edu/~byorgey/pub/13-11-25-nyhaskell-diagrams.pdf).

This release includes a number of significant new features and
improvements.  Highlights include:

* Support for drawing arrows between given points or between diagrams,
  with many options for customization
  ([tutorial](http://projects.haskell.org/diagrams/doc/arrow.html),
  [documentation](http://projects.haskell.org/diagrams/doc/manual.html#arrows),
  [API](http://projects.haskell.org/diagrams/haddock/Diagrams-TwoD-Arrow.html)).

* A new framework for creating custom command-line-driven executables
  for diagram generation
  ([tutorial](http://projects.haskell.org/diagrams/doc/cmdline.html), [API](http://projects.haskell.org/diagrams/haddock/Diagrams-Backend-CmdLine.html)).

* Offsets of trails and paths, *i.e.* compute the trail or path lying
  a constant distance from the given one
  ([documentation](http://projects.haskell.org/diagrams/doc/manual.html#offsets-of-segments-trails-and-paths),
  [API](http://projects.haskell.org/diagrams/haddock/Diagrams-TwoD-Offset.html)).

* A new API, based on Metafont, for constructing cubic splines with
  control over things like tangents and "tension"
  ([tutorial](http://projects.haskell.org/diagrams/doc/metafont.html),
  [API](http://projects.haskell.org/diagrams/haddock/Diagrams-TwoD-Path-Metafont.html)).

* Tangent and normal vectors of segments and trails ([API](http://projects.haskell.org/diagrams/haddock/Diagrams-Tangent.html)).

* Alignment can now be done by trace in addition to envelope ([API](http://projects.haskell.org/diagrams/haddock/Diagrams-TwoD-Align.html)).

* The [`lens`](http://hackage.haskell.org/package/lens) package is now used consistently for record fields throughout the
  library ([documentation](http://projects.haskell.org/diagrams/doc/manual.html#faking-optional-named-arguments)).

* Across-the-board improvements in performance and size of generated
  files.

See the
[release notes](http://projects.haskell.org/diagrams/releases.html)
for full details, and the
[migration guide](http://www.haskell.org/haskellwiki/Diagrams/Dev/Migrate1.0)
for help porting your diagrams 0.7 code to work with diagrams 1.0.

Try it out
----------

For the truly impatient:

    cabal install diagrams

Diagrams is supported under GHC 7.4 and 7.6.

To get started, read the
[quick start tutorial](http://projects.haskell.org/diagrams/doc/quickstart.html),
which will introduce you to the fundamentals of the framework and
provide links for further reading.

For those who are less impatient and want to really dig in and
use the power features, read the extensive
[user manual](http://projects.haskell.org/diagrams/doc/manual.html).
There is also a growing
[collection of tutorials](http://projects.haskell.org/diagrams/documentation.html)
on specific topics.

Get involved
------------

XXX excellent foo blah bar

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
package.

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
