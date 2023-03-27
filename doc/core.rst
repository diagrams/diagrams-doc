.. role:: pkg(literal)
.. role:: hs(literal)
.. role:: mod(literal)
.. role:: repo(literal)

.. default-role:: hs

.. contents::

Introduction
============

This document explains the low-level inner workings of
`diagrams-core`:pkg:.  Casual users of ``diagrams`` should not need to
read this (although a quick skim may well turn up something
interesting).  It is intended more for developers and power users who
want to learn how ``diagrams`` actually works under the hood; there is
quite a lot that goes on behind the scenes to enable the powerful
tools provided in `diagrams-lib`:pkg:.

Chris Mears has written `an article giving a quick walkthrough of some
of the internals
<http://www.cmears.id.au/articles/diagrams-internals.html>`_ which is
useful for getting started.

The remainder of this document is organized around the modules in
`diagrams-core`:pkg:.  At some level, there is no substitute for just
diving in and reading the source code (see the `diagrams-core`:repo:
repository), which is generally well-commented, but the hope is that
this document can serve to orient you and supply useful commentary.

Diagrams.Core
=============

This module simply re-exports many things from the other modules for
convenience.

Diagrams.Core.V
===============

`Diagrams.Core.V`:mod: contains the definition of the `V` type family,
which maps from types to their associated vector space.
(See the `relevant section from the user manual`__) along with some
basic instances.

__ manual.html#v

Diagrams.Core.Points
====================

The `linear`:pkg: package defines a `Point` type in `Linear.Affine`:mod:
along with some functions for working with points.

The `Diagrams.Core.Points`:mod: module simply re-exports a few things
from `linear`:pkg:, defines an instance of `V` and `N` for `Point`,
and adds a few utility functions for points.

Diagrams.Core.Names
===================

`Diagrams.Core.Names`:mod: defines the infrastructure for names which
can be used to identify subdiagrams.

`AName`, representing *atomic names*, is an existential wrapper,
allowing (almost) any type to be used for names, as the user finds
convenient.  Strings may be used of course, but also numbers,
characters, even user-defined types.  The only restriction is that the
wrapped type must be an instance of the following three classes:

* `Typeable` (so values can be pulled back out of the wrappers in a
  type-safe way),
* `Show` (so names can be displayed, for debugging purposes), and
* `Ord` (in order to be able to create a `Map` from names to
  subdiagrams).

Equality on atomic names works as expected: two names are equal if their
types match and their values are equal.

The `Ord` instance for atomic names works by first ordering names
according to (a `String` representation of) their type, and then by
value for equal types (using the required `Ord` instance).

A *qualified name* (`Name`) is a list of atomic names.  The `IsName`
class covers things which can be used as a name, including many
standard base types as well as `ANames` and `Names`.  Most user-facing
functions which take a name as an argument actually take any type with
an `IsName` constraint, so the user can just pass in a `String` or an
`Int` or whatever they want.

The motivation for having names consist of *lists* of atomic names is
that it is not always convenient or even feasible to have globally
unique names (especially when multiple modules by different authors
are involved).  In such a situation it is possible to *qualify* all
the names in a particular diagram by some prefix.  This operation
governed by the `Qualifiable` class, containing the function ``(|>) ::
IsName a => a -> q -> q`` for performing qualification.

Diagrams.Core.HasOrigin
=======================

This module defines the `HasOrigin` class (containing the
`moveOriginTo` method) as well as related functions like
`moveOriginBy`, `moveTo`, and `place`.  It also defines instances of
`HasOrigin` for a number of types, including `Point`\s, tuples, lists,
sets, and maps.

See the `section of the type class reference on HasOrigin`__ for more
information.

__ manual.html#hasorigin

Diagrams.Core.Transform
=======================

This module defines a type of generic affine transformations
parameterized over any vector space, along with a large number of
methods for working with transformations.

First, the `(:-:)` type consists of a pair of functions, which are
assumed to be linear and inverse to each other.

A `Transformation` type is then defined to contain three components:

* a linear map and its inverse (stored using `(:-:)`)
* the transpose of the linear map, with *its* inverse (again stored using `(:-:)`)
* a vector, representing a translation

The point is that we need transposes and inverses when transforming
things like `Envelope`\s and `Trace`\s.  While it would be possible in
theory to simply store a transformation as a matrix and compute its
transpose or inverse whenever required, this would be computationally
wasteful (especially computing inverses).  Instead, we simply package
up a transformation along with its inverse, transpose, and inverse
transpose (which we can think of as a little 2x2 matrix of functions).
Such a representation is closed under composition, and we can compute
its inverse or transpose by just flipping the matrix along the
appropriate axis.

Along with the definition of the `Transformation` type itself, this
module exports many functions for generically creating, transforming,
querying, and applying `Transformation` values.  For example, in
addition to straightforward things like composing and applying
transformations, this is where you can find code to convert a
`Transformation` to a matrix representation or to compute its
determinant.  (On the other hand, converting a matrix to a
`Transformation` is only implemented specifically for 2 or 3
dimensions, and can be found in the `diagrams-lib`:pkg: package, in
`Diagrams.Transform.Matrix`:mod:.)

This module also defines the important `Transformable` class of things
to which `Transformation`\s can be applied, along with many generic
instances.

Finally, the module defines a few specific transformations which are
polymorphic over the vector space, namely, translation and scaling.
Other specific transformations (*e.g.* `scaleX` and so on) are defined
in `diagrams-lib`:pkg:.

Diagrams.Core.Envelope
======================

This module defines the `Envelope` type; see the `user manual section
on envelopes`__ for a general overview of what envelopes are and how
to use them.

__ manual.html#envelopes

For an explanation of the specific way that `Envelope` is defined, see
`Brent Yorgey's paper on diagrams and monoids`__.

__ http://ozark.hendrix.edu/~yorgey/pub/monoid-pearl.pdf

The real meat of this module consists of the definitions of
`HasOrigin` and `Transformable` instances for the `Envelope` type.
The fact that packaging transformations together with their transpose
and inverse makes it possible to correctly compute the affine
transformation of an envelope is one of the key insights making the
diagrams framework possible.  The source code has `extensive comments
explaining the instances`__; consult those if you want to understand
how they actually work.

__ https://github.com/diagrams/diagrams-core/blob/master/src/Diagrams/Core/Envelope.hs#L181

Finally, this module defines the `Enveloped` class for things with
`Envelope`\s, a number of functions like `envelopeV`, `envelopePMay`,
and so on for querying envelopes, and size-related functions like
`diameter`, `extent`, and `size` that are defined in terms of
envelopes.

Diagrams.Core.Juxtapose
=======================

This module defines the `Juxtaposable` class, the default
implementation `juxtaposeDefault` for instances of `Enveloped` and
`HasOrigin`, and generic instances for `Envelope`, pairs, lists, maps,
sets, and functions.

See the `type class reference section on Juxtaposable`__ for more
information.

__ manual.html#juxtaposable

Diagrams.Core.Measure
=====================

This module defines the `Measured` type along with a number of utility
functions and instances for working with it.  See the `user manual
section on measurement units`__.

`Measured` values are implemented as functions from a triple of
scaling factors to a final value: the local scaling factor, global
scaling factor, and normalized scaling factor.  XXX write about how
these are computed

__ manual.html#measurement-units

Diagrams.Core.Trace
===================

This module implements the `trace`__ which is associated with every
diagram.  A trace is essentially an "embedded raytracer" which can
compute an intersection with a diagram in any direction from any given
base point.  Note that a trace needs to be able to answer a trace
query from *any* given base point, not just from some chosen
particular base point (*e.g.* the origin), since we need to be able to
apply affine transformations, including translations.

__ manual.html#traces

Often when one thinks about raytracing the basic idea is that you
follow a ray and return the *first* intersection that occurs.
However, to allow for also computing the *last* intersection and other
generalizations, the base framework in this module actually computes a
*sorted list* of *all* the intersection points.  Hence this module
defines a small abstraction for sorted lists, as well as the `Trace`
abstraction itself.  A number of functions for querying `Trace` values
are defined here, as well as the `Traced` class for things which have
a `Trace`.

Diagrams.Core.Query
===================

A `Query` is a function that associates a value of some (monoidal)
type to each point in a diagram; see `the user manual section on
queries`__.  There is not much in this module besides a great many
type class instances for the `Query` type.

__ manual.html#using-queries

Diagrams.Core.Style
===================

This module implements *styles*, which are collections of *attributes*
(such as line color, fill color, opacity, ...) that can be applied to
diagrams.  Diagrams takes a *dynamically typed* approach to attributes
and styles.  This is in contrast to the approach with backends and
primitives, where the type of a diagram tells you what backend it is
to be rendered with---or, if it is polymorphic in the backend, there
are type class constraints that say what primitives the backend must
be able to render.  But the type of a diagram never says anything
about what attributes a backend must support; indeed, by looking only
at the type of a diagram it is impossible to tell what types of
attributes it contains.  In general, backends pick out the attributes
they can handle and simply ignore any others.

Attributes
----------

Attributes are the primitive values out of which styles are built.
Almost any type can be used as an attribute, with only a few
restrictions: attributes must be `Typeable`, to support the use of
dynamic typing, and a `Semigroup`, so there is some sensible notion of
combining multiple attributes of the same type (which is used to
combine attributes applied within the same scope; as we will see, for
many standard attributes the semigroup is simply the one which keeps
one attribute and discards the other).  `AttributeClass` is defined as
a synonym for the combination of `Typeable` and `Semigroup`.

The `Attribute` type is then defined as an existential wrapper around
`AttributeClass` types.  In a simpler world `Attribute` would be
defined like this:

.. class:: lhs

::

  data Attribute where
    Attribute :: AttributeClass a => a -> Attribute

Historically, it did indeed start life defined this way.  However, as
you can see if you look at the source, by now the actual definition is
more complicated:

.. class:: lhs

::

  data Attribute (v :: * -> *) n :: * where
    Attribute  :: AttributeClass a => a -> Attribute v n
    MAttribute :: AttributeClass a => Measured n a -> Attribute v n
    TAttribute :: (AttributeClass a, Transformable a, V a ~ v, N a ~ n) => a -> Attribute v n

This looks like the simpler definition if you ignore the type
parameters and consider only the `Attribute` constructor.  So let's
consider each of the other constructors.

* `MAttribute` is for attributes that are `Measured`, *i.e.* whose
  values depend on the size of the final diagram and/or the requested
  output size; the primary examples are *line width* and *font size*.
  Recall that a `Measured n a` is actually a function that can produce
  a value of type `a` once it is provided some measurement factors of
  type `n`.  The `unmeasureAttribute` function is provided to turn
  `MAttribute` constructors into `Attribute` constructors; this is
  typically used when preparing a diagram for rendering.

* `TAttribute` is for attributes that are `Transformable`, *i.e.*
  which are affected by transformations applied to the objects to
  which they are attached.  The primary examples are *line* and *fill
  texture* (*e.g.* gradients), and *clipping paths*.  (Note that
  `MAttribute`\s can actually be affected by transformations too, in
  the case of `Local` units.)

The `Attribute` type has instances of `Semigroup` (combine attributes
of the same type, otherwise take the rightmost) and `Transformable`
(ignore `Attribute` constructors and do the appropriate thing for the
other constructors).  There are also various lenses/prisms for
accessing them.

Note that one does not typically construct an `Attribute` value directly
using the constructors; instead, the functions `applyAttr`,
`applyMAttr`, and `applyTAttr` are provided for applying an attribute
directly to any instance of `HasStyle`.

Styles
------

A `Style` is just a dynamically-typed, heterogeneous collection of
attributes.  The attributes are actually stored as values in a hash
table, keyed by their type (specifically, a `TypeRep`), so at most one
attribute of any given type can be stored in a `Style`.

`Style` is an instance of `Semigroup`, with union as a combining
operation.  If the two styles both have an attribute of a given type,
those attributes are combined according to the `Semigroup` instance
for that attribute type.

There are a number of other functions for creating `Style` values and
extracting attributes from them, which are mostly self-explanatory.
One function worth mentioning is `unmeasureAttrs`, which maps over a
`Style` and changes all `MAttribute`\s into `Attributes` (based on the
provided scaling factors).  This is typically done as a final step
before rendering.

Finally, the `HasStyle` class governs types which "have a `Style`",
specifically, types to which a `Style` can be applied.  `Style` itself
has an instance of `HasStyle`, corresponding to the semigroup
operation on `Style`.  This module also defines a number of other
instances for applying styles to entire data structures such as lists,
tuples, functions, maps, and sets.

Diagrams.Core.Types
===================

This is an unfortunately large module which contains definitions and
utility functions for many of the core data structures of diagrams.
In principle, it would be nice to break it up into smaller pieces, but
in fact a lot of things in this module end up cyclically depending on
one another, so easier said than done.

QDiagram
--------

The central type in diagrams is the definition of `QDiagram`:

.. class:: lhs

::

  newtype QDiagram b v n m
    = QD (D.DUALTree (DownAnnots v n) (UpAnnots b v n m) Annotation (QDiaLeaf b v n m))

`DUALTree` is defined in the `dual-tree`:pkg: package.  A value of
type `DUALTree d u a l` consists of an n-ary (rose) tree, with:

* Values of type `l` at the leaves.
* Monoidal annotations of type `u` which "travel up" the tree.  Each
  leaf of type `l` has a corresponding value of type `u`, and the `u`
  values are combined as one travels up the tree, so that the root
  would contain the `mconcat` of the `u` values in all the leaves.  We
  think of each tree (and subtree) as intrinsically possessing a `u`
  value, which is often some kind of "summary" or "measurement".
* There are also values of another monoid, of type `d`, which can be
  *applied* to a tree at the root and accumulate as one travels down
  any path from the root to a leaf.  These `d` values may *act on* the `u`
  values, that is, there is a function `d -> u -> u` satisfying
  certain coherence properties with respect to the monoid structures
  of `d` and `u` (though for a particular `d` and `u` the action may
  be trivial, *i.e.* `const id`).
* Finally, there are values of type `a` which can be stored at
  internal nodes. They are simply "along for the ride": they do not
  have associated `u` values nor are they affected by `d` values.  `a`
  values will never be moved (on the other hand, it is permissible to
  push `d` values up or down the tree in a way that preserves all
  monoid compositions).

The `QDiagram` type specifically instantiates these types as follows:

* `l` values at leaves are `QDiaLeaf` values.  One might think these
  consist simply of primitives, but actually they are a bit more
  complicated, because they also handle the case of "delayed" diagrams
  that need to know something about their context before they can be
  generated; this is explained in more detail below.

* The inert internal `a` values are of type `Annotation`.  Currently
  these consist solely of information about hrefs (for backends that
  support hyperlinks) and opacity grouping.

* The upwards-traveling `u` values are of type `UpAnnots`, explained
  in more detail below, which record various summary information about
  a diagram (envelope, trace, named subdiagrams, *etc.*).

* The downwards-traveling `d` values are of type `DownAnnots`,
  explained in more detail below, which record information imposed on
  a diagram (affine transformations, attributes, *etc.*).

QDiaLeaf
~~~~~~~~

The `QDiaLeaf` type has two cases:

.. class:: lhs

::

  data QDiaLeaf b v n m
    = PrimLeaf (Prim b v n)
    | DelayedLeaf (DownAnnots v n -> n -> n -> QDiagram b v n m)

The first case, `PrimLeaf`, is simple enough: it contains a `Prim`
(also defined in this module), which is simply an existential wrapper
around a `Transformable`, `Typeable`, `Renderable` thing.

The second case is trickier.  It represents a diagram which needs to
know its *global context*, represented by the accumulated `DownAnnots`
values along the path from the root to this delayed leaf, together
with the normal-to-output scaling factor and the global-to-output
scaling factor.  Note that we cannot possibly know this information
until just before the diagram is to be rendered, at which point it
becomes fixed and we know we are not going to insert this diagram into
a yet bigger diagram, and we know the requested output size and hence
can compute the scaling factors.  So just before rendering, delayed
leaves are recursively expanded into `QDiagrams`.  For an example of
`DelayedLeaf` in action, see `Diagrams.TwoD.Arrow`:mod: in
`diagrams-lib`:pkg:, where it is needed since arrowhead sizes can
depend on this global context.

Annotation
~~~~~~~~~~

The `Annotation` type has two constructors, one for hyperlinks
(`Href`) and one for opacity groups.  In both of these cases, it's
important that these annotations stay exactly where the user places
them in the tree, and they are unaffected by transformations and the
like.

UpAnnots
~~~~~~~~

`UpAnnots` is a heterogeneous list of monoidal values (with an
elementwise monoidal operation; see `Data.Monoid.MList`:mod: in the
`monoid-extras`:pkg: package) which serves as the "upwards-traveling"
monoid in a `QDiagram`.  That is, every primitive diagram at a leaf
has an intrinsic associated `UpAnnots` value, and these get combined
as one moves up the tree.  An `UpAnnots` value consists of the
following components:

* An `Envelope` (in a `Deletable` wrapper so we can implement
  `setEnvelope` monoidally; see `Data.Monoid.Deletable`:mod: in
  `monoid-extras`:pkg:),

* a `Trace` (similarly `Deletable`),

* a `SubMap` (also `Deletable`) which maps names to subdiagrams (more
  on these later), and

* a `Query`, which associates a monoidal value of some type `m` to
  every point in the diagram.

DownAnnots
~~~~~~~~~~

`DownAnnots` is 

Subdiagrams
-----------

Backends
--------

Diagrams.Core.Compile
=====================
