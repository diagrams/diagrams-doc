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

Diagrams.Core.Trace
===================

Diagrams.Core.Query
===================

Diagrams.Core.Style
===================

Diagrams.Core.Types
===================

QDiagram
--------

Backends
--------

Diagrams.Core.Compile
=====================
