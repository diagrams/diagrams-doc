I am pleased to announce the release of version 0.5 of
[diagrams](http://projects.haskell.org/diagrams), a full-featured
framework and embedded domain-specific language for declarative
drawing.

Major improvements since the 0.4 release include

* animation

* diagrams-contrib

See the
[diagrams wiki](http://www.haskell.org/haskellwiki/Diagrams/Migrate0.5)
for help migrating from 0.4 to 0.5.

Try it out
----------

For the truly impatient:

    cabal install gtk2hs-buildtools
    cabal install diagrams

Diagrams is fully supported under GHC 6.12, 7.0, 7.2, and 7.4.
However, getting cairo to build can be tricky on some platforms; see
the [diagrams wiki](http://www.haskell.org/haskellwiki/Diagrams) for
more information and workarounds regarding specific platforms.  

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
for help, discussion of new features, etc.

XXXXX  what to put here?  don't put as much?

* There are lots of easy bug fixes, improvements, and feature requests
  just waiting for people wanting to get involved: see the
  [bug tracker](http://code.google.com/p/diagrams/issues/list) for a
  list of open tickets.

  The source repositories are mirrored using both darcs (on
  patch-tag.com) and git (on github.com), and patches are accepted in
  either place, thanks to Owen Stephen's great work on
  [darcs-bridge](http://wiki.darcs.net/DarcsBridgeUsage).

* Create a higher-level module built on top of the diagrams framework
  (e.g. tree or graph layout, generating Turing machine configuration
  diagrams, Penrose tilings ... your imagination is the only limit!)
  and submit it for inclusion in a special diagrams-contrib package
  which will be created for such higher-level user-contributed modules.

* Use diagrams to create some cool graphics and submit them for
  inclusion in the [gallery](http://projects.haskell.org/diagrams/gallery.html).

* Start your own project built on top of diagrams and let us know how
  it goes!

* Last but certainly not least, just try it out for your pet graphics
  generation needs and contribute your bug reports and feature
  requests.


Happy diagramming!

XXXX


Brought to you by the diagrams team:

* Brent Yorgey
* Ryan Yates

with contributions from:

* Sam Griffin
* Claude Heiland-Allen
* John Lato
* Vilhelm Sjöberg
* Luite Stegeman
* Kanchalai Suveepattananont
* Scott Walck
