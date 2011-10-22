I am pleased to announce the release of version 0.4 of
[diagrams](http://projects.haskell.org/diagrams), a full-featured
framework and embedded domain-specific language for declarative
drawing.

The last announcement was of the 0.1 release; there have been quite a
few changes and improvements since then, including:

  * A new [website](http://projects.haskell.org/diagrams) including a
    [gallery](http://projects.haskell.org/diagrams/gallery.html) of examples

  * A new comprehensive
    [user manual](http://projects.haskell.org/manual/diagrams-manual.html)
    with lots of illustrative examples

  * New primitive shapes: rounded rectangles, wedges, and a new
    flexible API for generating polygons

  * Cubic splines

  * Basic text support

  * Support for external image primitives

  * Lots more convenient combinators, bug fixes, and improvements


Cool, how can I try it out?
---------------------------

For the truly impatient:

    cabal install gtk2hs-buildtools
    cabal install diagrams

For the slightly less impatient, read the [quick
tutorial](http://projects.haskell.org/diagrams/tutorial/DiagramsTutorial.html),
which has detailed information about how to install the necessary
packages and will introduce you to the fundamentals of the framework.  

For those who are even less impatient but want to really dig in and
use the power features, read the
[user manual](http://projects.haskell.org/manual/diagrams-manual.html).


Cool, how can I contribute?
---------------------------

There are lots of ways you can contribute!  First, you may want to
subscribe to the [project mailing
list](http://groups.google.com/group/diagrams-discuss), and/or come
hang out in the `#diagrams` IRC channel on freenode.org.

* There are lots of easy bug fixes, improvements, and feature requests
  just waiting for people wanting to get involved: see the
  [bug tracker](http://code.google.com/p/diagrams/issues/list) for a
  list of open tickets.

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
