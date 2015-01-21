Introduction
============

This document explains the low-level inner workings of
`diagrams-core`{.pkg}. Casual users of `diagrams` should not need to
read this (although a quick skim may well turn up something
interesting). It is intended more for developers and power users who
want to learn how `diagrams` actually works under the hood; there is
quite a lot that goes on behind the scenes to enable the powerful tools
provided in `diagrams-lib`{.pkg}.

Chris Mears has written [an article giving a quick walkthrough of some
of the
internals](http://www.cmears.id.au/articles/diagrams-internals.html)
which is useful for getting started.

The remainder of this document is organized around the modules in
`diagrams-core`{.pkg}. At some level, there is no substitute for just
diving in and reading the source code (see the `diagrams-core`{.repo}
repository), which is generally well-commented, but the hope is that
this document can serve to orient you and supply useful commentary.

Diagrams.Core.V
===============

`Diagrams.Core.V`{.mod} contains the definition of the `V`{.hs} type
family, which maps from types to their associated vector space. (See the
[relevant section from the user manual](manual.html#v)) along with some
basic instances.

Diagrams.Core.Points
====================

The `linear`{.pkg} package defines a `Point`{.hs} type in
`Linear.Affine`{.mod} along with some functions for working with points.

The `Diagrams.Core.Points`{.mod} module simply re-exports a few things
from `linear`{.pkg}, defines an instance of `V`{.hs} and `N`{.hs} for
`Point`{.hs}, and adds a few utility functions for points.

Diagrams.Core.Names
===================

`Diagrams.Core.Names`{.mod} defines the infrastructure for names which
can be used to identify subdiagrams.

`AName`{.hs}, representing *atomic names*, is an existential wrapper,
allowing (almost) any type to be used for names, as the user finds
convenient. Strings may be used of course, but also numbers, characters,
even user-defined types. The only restriction is that the wrapped type
must be an instance of the following three classes:

-   `Typeable`{.hs} (so values can be pulled back out of the wrappers in
    a type-safe way),
-   `Show`{.hs} (so names can be displayed, for debugging purposes), and
-   `Ord`{.hs} (in order to be able to create a `Map`{.hs} from names to
    subdiagrams).

Equality on atomic names works as expected: two names are equal if their
types match and their values are equal.

The `Ord`{.hs} instance for atomic names works by first ordering names
according to (a `String`{.hs} representation of) their type, and then by
value for equal types (using the required `Ord`{.hs} instance).

A *qualified name* (`Name`{.hs}) is a list of atomic names. The
`IsName`{.hs} class covers things which can be used as a name, including
many standard base types as well as `ANames`{.hs} and `Names`{.hs}. Most
user-facing functions which take a name as an argument actually take any
type with an `IsName`{.hs} constraint, so the user can just pass in a
`String`{.hs} or an `Int`{.hs} or whatever they want.

The motivation for having names consist of *lists* of atomic names is
that it is not always convenient or even feasible to have globally
unique names (especially when multiple modules by different authors are
involved). In such a situation it is possible to *qualify* all the names
in a particular diagram by some prefix. This operation governed by the
`Qualifiable`{.hs} class, containing the function
`(|>) :: IsName a => a -> q -> q` for performing qualification.

Diagrams.Core.HasOrigin
=======================

Diagrams.Core.Juxtapose
=======================

Diagrams.Core.Transform
=======================

Diagrams.Core.Envelope
======================

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