---
title: Home
---

Diagrams is a powerful, flexible, declarative domain-specific language
for creating vector graphics.  Diagrams is:

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