I am pleased to announce the release of version 0.6 of
[diagrams](http://projects.haskell.org/diagrams), a full-featured
framework and embedded domain-specific language for declarative
drawing.  Check out the
[gallery](http://projects.haskell.org/diagrams/gallery.html) for
examples of what it can do!

Highlights of this release include:

- Diagrams now comes with a native-Haskell
  [SVG backend](http://hackage.haskell.org/package/diagrams%2Dsvg) by
  default.  If you were holding off on trying diagrams because you
  couldn't install cairo, you no longer have an excuse!

- Proper support for subdiagrams: previous versions of diagrams-core
  had a mechanism for associating names with a pair of a location and
  an envelope.  Now, names are associated with actual subdiagrams
  (including their location and envelope, along with all the other
  information stored by a diagram).  This enables cool techniques like
  constructing a diagram in order to position its subelements and then
  taking it apart again, or constructing animations via keyframing.

- Traces: in addition to an envelope, each diagram now stores a
  "trace", which is like an embedded raytracer: given any ray
  (represented by a base point and a vector), the trace computes
  the closest point of intersection with the diagram along the
  ray.  This is useful for determining points on the boundary of a
  diagram, *e.g.* when drawing arrows between diagrams.

- The core data structure underlying diagrams has been completely
  refactored and split out into its own separate package,
  [dual-tree](http://hackage.haskell.org/package/dual%2Dtree).

- Support for GHC 7.6.

- Many more new features, bug fixes, and improvements!

See the [release notes](http://projects.haskell.org/diagrams/releases.html) for
complete details, and the
[diagrams wiki](http://www.haskell.org/haskellwiki/Diagrams/Migrate0.6)
for help migrating from 0.5 to 0.6.

Try it out
----------

For the truly impatient:

    cabal install diagrams

Diagrams is supported under GHC 7.0 through 7.6.

To get started with diagrams, read the
[quick tutorial](http://projects.haskell.org/diagrams/tutorial/DiagramsTutorial.html),
which will introduce you to the fundamentals of the framework.

For those who are less impatient and want to really dig in and
use the power features, read the
[user manual](http://projects.haskell.org/manual/diagrams-manual.html).

Get involved
------------

Subscribe to the
[project mailing list](http://groups.google.com/group/diagrams-discuss),
and/or come hang out in the `#diagrams` IRC channel on freenode.org
for help and discussion.  Make some diagrams.
[Fix some bugs](http://github.com/diagrams/). Submit
your cool examples for inclusion in the
[gallery](http://projects.haskell.org/diagrams/gallery.html) or your
cool code for inclusion in the
[diagrams-contrib](http://hackage.haskell.org/package/diagrams%2Dcontrib)
package!

Happy diagramming!

Brought to you by the diagrams team:

* Michael Sloan
* Ryan Yates
* Brent Yorgey

with contributions from:

* Sam Griffin
* Niklas Haas
* Peter Hall
* Claude Heiland-Allen
* Deepak Jois
* John Lato
* Felipe Lessa
* Chris Mears
* Ian Ross
* Vilhelm Sjöberg
* Jim Snavely
* Luite Stegeman
* Kanchalai Suveepattananont
* Michael Thompson
* Scott Walck
