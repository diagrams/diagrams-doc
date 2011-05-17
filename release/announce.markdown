I am extremely pleased to announce a "developer preview" release of
the [diagrams framework](http://code.google.com/p/diagrams/) for
declarative drawing.  This is a well-thought-out, well-documented,
working release with all core functionality in place, but with many
planned features still missing (for example, support for rendering
text and higher-level tools for constructing curves).  If you are
interested in

* trying out a new way of producing vector graphics,

* providing feedback to help drive ongoing development, or

* getting involved and contributing some code yourself,

please give it a try!  On the other hand, if you are looking for a
complete, full-featured package that will let you jump right into
producing the graphics you need, you may want to wait for the 1.0
release.

If you are familiar with the [diagrams
package](http://hackage.haskell.org/package/diagrams) already on
Hackage, this is a complete rewrite which has been in the works for
over a year and a half.

What is it?
-----------

Diagrams is an embedded domain-specific library (EDSL) for creating
diagrams, illustrations, and other sorts of vector graphics.  The
overall vision is for diagrams to become a viable alternative to
systems like MetaPost, Asymptote, and PGF/TikZ.

Diagrams is:

* *Declarative*: you specify *what* a diagram is, not *how* to draw it.

* *Compositional*: diagrams can be combined in many ways to produce
   more complex diagrams.  Diagrams are scale- and
   translation-invariant, so you never have to worry about a "global"
   coordinate system, only "local" ones.

* *Embedded*: the full power of Haskell, including every library on
   Hackage, is available to help construct and manipulate diagrams.

* *Extensible*: extending diagrams with additional or higher-level
   functionality is as simple as writing a Haskell module.

* *Flexible*: diagrams is designed from the ground up to be as generic
   and flexible as possible.  Features include:

    * Pluggable rendering backends -- creating a new rendering backend
      is as simple as writing a type class instance.

    * Arbitrary vector spaces -- the core diagrams library data types
      and primitives work for any vector space, so given a suitable
      rendering backend you can produce diagrams of any dimension, or
      even more exotic things...

Cool, how can I try it out?
---------------------------

Start by reading the [quick
tutorial](http://projects.haskell.org/diagrams/tutorial/DiagramsTutorial.html),
which has detailed information about how to install the necessary
packages and will introduce you to the fundamentals of the framework.

Or, for the truly impatient:

    cabal install diagrams-core diagrams-lib diagrams-cairo

How can I contribute?
---------------------

There are lots of ways you can contribute!  First, you may want to
subscribe to the [project mailing
list](http://groups.google.com/group/diagrams-discuss), and/or come
hang out in the <code>#diagrams</code> IRC channel on freenode.org.

* Cairo is the only well-supported backend at the moment, but you
  might create another backend or [contribute to an existing
  project](http://code.google.com/p/diagrams/wiki/BackendProjects).

* The standard library is in need of additional features.  Visit the
  [Google Code site](http://code.google.com/p/diagrams/) for a list of
  open tickets.

* Create a higher-level module built on top of the diagrams framework
  (e.g. tree or graph layout, generating Turing machine configuration
  diagrams, Penrose tilings ... your imagination is the only limit!)
  and submit it for inclusion in a special diagrams-contrib package
  which will be created for such higher-level user-contributed modules.

* Use diagrams to create some cool graphics and submit them for
  inclusion in a gallery of examples (to be created soon).

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
* Vilhelm Sjöberg
* Luite Stegeman
* Kanchalai Suveepattananont
* Scott Walck
