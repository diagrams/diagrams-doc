---
title: Releases
---

diagrams 1.2: 2 June 2014
=========================

[diagrams-core-1.2](http://hackage.haskell.org/package/diagrams-core-1.2)

* **New features**

    - New function `matrixHomRep` to convert a transformation to a
      homogeneous matrix representation.

    - New function `dropTransl` to drop the translation component
      from a transformation.

    - A mini-DSL for Measures.

    - New `extent` function, used in `diameter`.

    - New `dimension` function to return the dimension of a vector space.

    - New `_relative` iso between points and vectors.

    - `avgScale` function (for computing the average scaling factor of
      a transformation) has been moved from `diagrams-lib` to
      `diagrams-core` and generalized to work over any vector space.

* **Dependency/version changes**

    - Allow `semigroups-0.14`
    - Allow `lens-4.1`

* **API changes**

    - Major refactoring which removes `freeze` (and hence `Split` transforms,
      etc.) and adds units of `Measure`.

    - Refactoring and simplification of the `Backend` class.

    - Remove `Multibackend`.

    - Remove `nullPrim`, `IsPrim` and simplify `RPrim` so that it does not
      carry a transformation.

    - Update `adjustDia` to return a transformation, not just a scale factor.
      Add `renderDiaT` which returns a transformation (for use by end
      users, e.g. to convert output coordinates back into local coordinates).
      
[diagrams-lib-1.2](http://hackage.haskell.org/package/diagrams-lib-1.2)

* **New features**

    - Several attributes (such as line width, dashing size, arrowhead
      size, and font size) that formerly had a value of type `Double`
      now have the more general type `Measure R2`. This allows the
      attributes to be specified relative to one of four measurement
      frames: `Local`, `Global`, `Normalized`, and `Output`.

    - New DSL for specifying measurements.

    - New synonyms for specifying measurements, *e.g.* `thin`,
      `thick`, and `medium`, `large`.

    - Support for radial and linear gradients for fills and strokes.

    - New `DImage` type that supports both embedded and external images in
      Backends that support them.

    - New `lengths` Traversal for setting `headLength` and `tailLength`
      simultaneously.

    - `Frustrum` and `Box` shapes added to `Diagrams.ThreeD.Shapes`.

    - New function `quartForm` to find roots of quartic polynomials.

    - New Lenses for polar coordinates.

    - New trig functions, `sinA`, `atanA`, etc. which take `Angle` as
      input or output.

* **New instances**

    - `Transformable` instances for `LineWidth`, `Dashing`,
      `LineTexture`, and  `FillTexture`.

* **API changes**

    - `FillColor` and `LineColor` attributes have been replaced with
      the more general `FillTexture` and `LineTexture`. Use the `solid`
      function to convert a color to a texture.

    - The size of arrow heads and tails is now specified in terms of
      length instead of the radius of their circumcircle.

    - Gaps at the ends of arrows are now specified using `Measure R2`.

    - The `gap` traversal has been replaced by `gaps` for consistency
      in naming, though `gap` is still provided for backwards compatibility.

    - `fontSize` now takes a value of type `Measure R2`.

    - Get rid of (bitrotted) `Show` backend.

    - Functions in `TwoD.Adjust` now return the adjustment
      transformation itself in addition to the resized `Diagram` and
      `Options` record; this can be used, *e.g.* to convert screen
      coordinates back into diagram coordinates.

    - Export `pathLocSegments`.

    - The `avgScale` function has been moved to `Diagrams.Core`.

    - The `Angle` definition and related functions (*e.g.*
      `angleBetween`) have moved to a separate module, `Diagrams.Angle`.

    - A separate `Diagrams.TwoD.Attributes` module now contains most
      of the attributes that require 2D transformation instances.

    - The `splitColorFills` function has been replaced by `splitTextureFills`.

* **Dependency/version changes**

    - Allow `semigroups-0.15`
    - Allow `optparse-applicative-0.9.0`
  
[diagrams-cairo-1.2](http://hackage.haskell.org/package/diagrams-cairo-1.2)

* **New features**

    - Much improved text support using the `pango` library instead of
      cairo's "toy" text API.

    - Support for linear and radial gradients on strokes and fills.

* **Dependency/version changes**

    - Allow `opt-parse-applicative-0.9`
    - Allow `lens-4.2`
    - Allow `mtl-2.2`
    - Allow `transformers-0.4`
  
[diagrams-postscript-1.1](http://hackage.haskell.org/package/diagrams-postscript-1.1)

  - Changes to reflect `Measure` refactoring.
  - Allow `diagrams-core-1.2` and `diagrams-lib-1.2`
  - Allow `semigroups-0.15`
    
[diagrams-svg-1.1](http://hackage.haskell.org/package/diagrams-svg-1.1)

* **New features**

    - Support for radial and linear gradients.

    - Support for embedded images in `.png` format.

* **New instances**

    - `Renderable` instances for `DImage Embedded`.

* **API changes**

    - Updates to work with `Measure` units.

    - Substantial refactoring of `Backend` instance to support changes in
      `Diagrams.Core`.
      
[diagrams-rasterific-0.1](http://hackage.haskell.org/package/diagrams-rasterific_0.1)

  *Initial release*
      
[diagrams-builder 0.5.0.10](http://hackage.haskell.org/package/diagrams-builder-0.5,0,10)

  - Module parse error messages now include the error location
    (thanks to Yiding Jia)

diagrams 1.1: 8 March 2014
==========================

[diagrams-core-1.1](http://hackage.haskell.org/package/diagrams-core-1.1)
--------------------------

* **New features**

    - New `basis` function
	- New `determinant` function for computing the determinant of a
      `Transformation`
    - Add `Typeable` constraint on `Prim`s, making it possible to
      extract things back out of a `Prim` wrapper using `cast`
	- Raw `Trace`s now return a *sorted list* of intersections,
      instead of only the smallest.  This is used to implement a new
      family of functions `rayTraceV`, `rayTraceP`, `maxRayTraceV`,
      `maxRayTraceP`, which work similarly to the parallel versions
      without `Ray`, but return the first intersection in the
      *positive* direction from the given point, rather than the
      smallest in absolute terms.
    - New `Annotation` type and corresponding `applyAnnotation`
      function, for attaching uninterpreted annotations at specific
      points in a diagram tree.  Currently this is used for
      hyperlinks; more annotation types will be added in the future.

* **Dependency/version changes**

    - Require `lens-4.0`
    - Allow `vector-space-points-0.2`

* **Bug fixes**

    - Looking up a subdiagram by name now results in a diagram which
      still has that name (#43)

[diagrams-lib-1.1](http://hackage.haskell.org/package/diagrams-lib-1.1)
--------------------------

* **New features**

    - Support for `Deformation`s, arbitrary (non-affine)
      transformations on objects such as points, paths, and located
      trails (though not on diagrams).

    - New functions `clipTo`, which clips a diagram's envelope and
      trace along with its visual representation, and `clipped`, which
      clips the diagram's visual representation but replaces its
      envelope and trace with those of the clipping path.

    - New `arrowV` function, for creating an arrow with the direction
      and magnitude of a given vector.

    - `gap` traversal, for setting the head and tail gaps of an arrow
      simultaneously.

    - Generalized types for `centerXY` and `snugXY`, based on new
      `basis` function from `diagrams-core

    - New 3D `Transform`s, alignment, and 3D-specific `Prelude`.

    - New `frame` function similar to `pad`, but increases the envelope
      of a diagram by an amount specified in local units in every direction
      irrespective of the local origin.

    - New `splitFills` function for pushing fill attributes down to
      subtrees containing only loops (mostly of relevance only to
      backend implementors).

* **New instances**

    - `Typeable` instances for all data types that are used as diagram
      primitives.
    - `Sectionable` instance for `FixedSegment`.

* **API changes**

    - `Angle` is now a type, rather than a class.  It uses a single
      internal representation for angles, and lenses `turn`, `rad,`
      and `deg` are supplied for constructing (using `@@`) and viewing
      (using `^.`) `Angle`s in various units.  In addition, the `Num`
      instance for `Angle` has been removed, eliminating a class of
      errors where a bare number is interpreted in units other than
      what you expect.

    - Removed `Num` instance for angles.

* **Dependency/version changes**

    - Require `lens >= 4.0`.
	- Allow `array-0.5`.
	- Allow `hashable-1.1`.
	- Remove `NumInstances` dependency.

* **Bug fixes**

    - Exclude joins in offsets on close segments (#160).
    - Exclude extra segment when joining loops in offset (#155).

* **Performance improvements**

    - `colorToSRGBA` function now avoids expensive matrix operations,
      offering dramatic speedups in rendering diagrams with many color
      attributes.

[diagrams-cairo-1.1](http://hackage.haskell.org/package/diagrams-cairo-1.1)
--------------------------

* **New features**

    - It is now possible to directly output animated GIFs, using the
      `gifMain` function.

* **Dependency/version changes**

    - allow `diagrams-core-1.1` and `diagrams-lib-1.1`
    - allow `unix-2.7`
    - allow `vector-0.10`

* **Bug fixes**

    - Don't explicitly draw final segment of a loop if it is straight
      ([#38](https://github.com/diagrams/diagrams-cairo/issues/38))

[diagrams-gtk-1.0.1](http://hackage.haskell.org/package/diagrams-gtk-1.0.1)
--------------------------

* Use double buffering.

[diagrams-postscript-1.0.2](http://hackage.haskell.org/package/diagrams-postscript-1.0.2)
--------------------------

* **New features**

  - Experimental support for raw CMYK colors.

* **Dependency/version changes**

  - Allow `diagrams-core-1.1` and `diagrams-lib-1.1`.

[diagrams-svg-1.0.2](http://hackage.haskell.org/package/diagrams-svg-1.0.2)
--------------------------

* **New features**

    - Support for including hyperlinks.

* **Dependency/version changes**

    - Allow `diagrams-core-1.1` and `diagrams-lib-1.1`
    - Allow `lens-4.0`

* **Bug fixes**

    - Use `splitFills` to properly render certain diagrams with mixed
      lines and filled loops.  Previously, in certain situations loops that should
      have been filled were not.  ([#43](https://github.com/diagrams/diagrams-svg/issues/43))

    - Don't emit last segment of a loop if it is linear.

      See [diagrams-cairo#38](http://github.com/diagrams/diagrams-cairo/issues/38).  This wasn't actually causing any
      observable problems in the SVG backend output, but this seems a
      better/more robust way to do things in any case.

[diagrams-builder 0.4.2](http://hackage.haskell.org/package/diagrams-builder-0.4.2)
==========================

- Build expressions of type `Diagram b v` *or* `IO (Diagram b v)`.
  This means that expressions interpreted by `diagrams-builder` (via
  *e.g.* `diagrams-haddock` or `BlogLiterately-diagrams`) can do some
  `IO` to build a diagram.

diagrams 1.0: 25 November 2013
==============================

[dual-tree 0.2](http://hackage.haskell.org/package/dual-tree-0.2)
----------------

- Expose internal d-annotations via `foldDUAL`.

[diagrams-core 1.0](http://hackage.haskell.org/package/diagrams-core-1.0)
----------------

* **New features**

    * Delayed subtrees: instead of a primitive, one can now also have
      a delayed subtree at a leaf, containing a continuation which
      generates a `QDiagram` when given the accumulated d-annotation
      at that point in the tree.  Useful for things which need to know
      the final transformation applied to them before deciding what
      diagram to generate.  The prototypical use case is arrows: see
      https://github.com/diagrams/diagrams-lib/issues/112 .  However,
      this may be useful for other things as well: for example,
      diagrams which scale normally until hitting some maximum or
      minimum size, at which point they refuse to scale any further
      (or more generally diagrams which scale as some non-linear
      function of the transformation applied to them).

        The only downside is that the u-annotation must be fixed ahead
        of time---doing otherwise requires a more general solution for
        constraint solving.

    * New function `lookupName` for doing a simple lookup of a named
      subdiagram

    * New module `Diagrams.Core.Compile`, containing a framework for
      compiling `QDiagrams` into a simpler tree type `RTree`, which
      may be used by backends for rendering.

* **New instances**

    * `Qualifiable` instances for `(,)`, `(,,)`, `[]`, `Set`, `Map k`,
      and `(->) e`.

    * `(->) e` instance for `Juxtaposable` (thanks to Carlos Scheidegger)

* **API changes**

    * Export `pointDiagram` function, which creates an otherwise empty
      diagram with a point (not empty) envelope

    * A bunch of stuff now uses machinery from the `lens` library.
	    * `envelope`, `trace`, and `subMap` are now `Lens'`es
        * `Wrapped` instances for `Trace`, `TransInv`, `QDiagram`,
          `SubMap`, `Envelope`, `Style`, `Query`, and `Name` (replaces
          `Newtype` instances)
	    * `Iso`s for `Query`, `Envelope`, `QDiagram`, `SubMap`, `TransInv`


[diagrams-lib 1.0](http://hackage.haskell.org/package/diagrams-lib-1.0)
----------------

* **New features**

    - New modules `Diagrams.TwoD.Arrow` and `Diagrams.TwoD.Arrowheads`
      for creating arrows.
    - New module `Diagrams.Backend.CmdLine`, providing a flexible
      framework for creating command-line-driven diagram rendering executables.
    - New functions in `Diagrams.Offset`: `offsetTrail` and
      `offsetPath` for one-sided offsets of trails and paths;
      `expandTrail` and `expandPath` for "stroking" trails and paths,
      computing a path whose fill corresponds to the stroke of the
      given trail or path.
    - New module `Diagrams.Tangent` for computing tangent and normal
      vectors of segments, trails, and paths.
    - New functions in `Diagrams.Align` to allow diagrams to be aligned by `Trace`
      called `snug`, `snugBy` and `snugCenter`
      and the ability to define other boundary functions for alignment. Functions
      `snugL`, `snugR`, etc. are included in `TwoD.Align`.
    - Lenses from `Control.Lens` are now used consistently for record fields
      throughout the library.
    - New function `angleRatio` for calculating the ratio between two angles.
    - Restricted identity functions `asTurn`, `asRad`, and `asDeg` for
      resolving type ambiguity
    - New miter limit attribute.
    - New function `annularWedge` in `TwoD.Arc`
    - New `avgScale` utility in `TwoD.Transform`, for backends which
      cannot fully implement freezing of line width
    - New function `heptagon`, a vast improvement over the linguistic
      frankenstein `septagon`.
    - New function `lookupName` (re-exported from `diagrams-core`) for
      simple lookups of named subdiagrams
    - New function `angleBetween` to calculate the angle between two
      vectors.
    - New function `arcBetween` to draw an arc between two given
      points.
    - A bunch of new modules containing types, primitives and
      utilities for constructing 3D diagrams: `Diagrams.ThreeD.Align`,
      `.Camera`, `.Light`, `.Shapes`, `.Transform`, `.Types`, and
      `.Vector`.  This is still a "feature preview" (in particular,
      appropriate 3D backends are still under construction).

* **New instances**

    - `AdditiveGroup` and `VectorSpace` instances for `Turn`, `Rad`, `Deg`
    - `Alignable` instance for `(->) e`
	- `Default` instances for `FillRule`, `FillRuleA`, `LineJoin`,
      `LineCap`, `FillColor`
	- `Show` instances for `FillRule`, `FillRuleA`

* **API changes**

    - `e` no longer exported from `Diagrams.Prelude`.
    - `Diagrams.BoundingBox` is no longer exported from `Diagrams.Prelude`.
    - Re-export `Diagrams.Core.pointDiagram` from `Diagrams.Prelude`.
    - Added `fromAlphaColour` method to `Color` class.
    - `&` renamed to `^&`
    - Stop re-exporting `tan`, `over`, and `both` from `Data.Colour`.
	- New coordinate lenses `_x`, `_y`, and `_z` for `R2`, `P2`, `R3`, `P3`
    - Export `fullTurn` from `Diagrams.Prelude`.
    - `Codomain (Located a)` is now `Point (Codomain a)` instead of
      `Located (Codomain a)`.
	- Export `domainBounds` from `Diagrams.Parametric`.
	- Adjusting functionality moved from `Diagrams.Parametric` to its
      own module, `Diagrams.Parametric.Adjust`.
    - Rename `strokeT` (and primed variant) to `strokeTrail`; rename
      `strokeLocT` to `strokeLocTrail`.
    - `ScaleInv` is now in its own module, `Diagrams.TwoD.Transform.ScaleInv`.
	- Re-export `Image` type (but not constructor) from `Diagrams.TwoD`
    - Removed `Floating` and `RealFloat` instances for `Turn` and `Deg`
    - `offsetSegment` now returns a `Located` instead of a tuple.
    - Removed `Num` and `Fractional` instances for `R2`.

* **Dependency/version changes**

    - Remove `newtype` dependency
    - New dependencies on `lens`, `tagged`, `optparse-applicative`,
      `filepath`, `safe`, `vector-space-points`, `MemoTrie`
    - Depend on `intervals >= 0.3 && < 0.5`.

* **Bug fixes**

    - Depend on `intervals 0.3`, which allows diagrams to build on
      Windows, by evading a GHCi linker bug which affects the FFI use in
      previous versions of intervals ([diagrams-contrib#14](https://github.com/diagrams/diagrams-contrib/issues/14))
    - Use point envelope at the origin for text objects instead of an
      empty envelope
      ([#115](https://github.com/diagrams/diagrams-lib/issues/115),
      [#116](https://github.com/diagrams/diagrams-lib/issues/116)).
    - Adjusting the end of a trail now works correctly ([#95](https://github.com/diagrams/diagrams-lib/issues/95)).
    - Only look for miter join on corners in `Diagrams.TwoD.Offset` ([#118](https://github.com/diagrams/diagrams-lib/issues/118)).
    - `wedge` from `Diagrams.TwoD.Arc` is now a loop ([#99](https://github.com/diagrams/diagrams-lib/issues/99)).

* **Performance improvements**

    - `R2` is now strict and `UNPACK`ed
    - Add strictness to `Offset`, `Segment`, `OffsetEnvelope`, and `SizeSpec2D`.
	- Make `getEnvelope` calculation for `Segment` more efficient by
      floating divisions out of the inner calculation.
    - Use a specialized `HasTrie` instance for `R2`.

[diagrams-svg 1.0](http://hackage.haskell.org/package/diagrams-svg-1.0)
----------------

- Re-implement via new backend `RTree` interface, leading to
  smaller output files.
- Use new command-line interface from `diagrams-lib`.
- Export `B` as an alias for `SVG` token

[diagrams-cairo 1.0](http://hackage.haskell.org/package/diagrams-cairo-1.0)
----------------

- Re-implement via new backend `RTree` interface.
- Use new command-line interface from `diagrams-lib`.
- Export `B` as an alias for `Cairo` token.

[diagrams-postscript 1.0](http://hackage.haskell.org/package/diagrams-postscript-1.0)
----------------

- Add support for miter limit attribute.
- Use new command-line interface from `diagrams-lib`.
- Export `B` as an alias for `Postscript` token.

[diagrams-gtk 1.0](http://hackage.haskell.org/package/diagrams-gtk-1.0)
----------------

- Updated to work with diagrams-cairo-1.0.

[diagrams-contrib 1.0](http://hackage.haskell.org/package/diagrams-contrib-1.0)
----------------

* **New features**

    - New module `Diagrams.TwoD.Sunburst`, for drawing sunburst
      charts.
    - New module `Diagrams.TwoD.Path.Metafont`, for specifying
      trails/paths using an API inspired by Metafont.  Like
      `cubicSpline` but gives you control over things like the
      tension/curvature or the tangent at a given point.
    - New module `Diagrams.TwoD.Path.Calligraphic` for making simple
      "calligraphic" strokes.
    - New module `Diagrams.Lens` with lenses for working with
      diagrams.

* **Bug fixes**

    - `Diagrams.TwoD.Factorization.primeLayout` has been rewritten to
      avoid iterated transformations, giving a significant performance
      boost.

[monoid-extras 0.3.2.3](http://hackage.haskell.org/package/monoid-extras-0.3.2.3)
----------------

[SVGFonts 1.4.0.1](http://hackage.haskell.org/package/SVGFonts-1.4.0.1)
----------------

[diagrams-builder 0.4.1](http://hackage.haskell.org/package/diagrams-builder-0.4.1)
----------------

[diagrams-haddock 0.2.1.3](http://hackage.haskell.org/package/diagrams-haddock-0.2.1.3)
----------------

diagrams 0.7: 9 August 2013
===========================

[diagrams-core 0.7](http://hackage.haskell.org/package/diagrams%2Dcore-0.7)
----------------

* **New features**

    - new function `onBasis`, to extract the matrix equivalent of a
      `Transformation`

    - `SubMap`s are now `Deletable`

    - new function `localize` for hiding/deleting names from scope

    - new `IsPrim` class, containing `transformWithFreeze` function.
        This is primarily intended to support scale-invariant primitives
        (*e.g.* arrowheads) but may be useful for other stuff as well.
        The default implementation of `renderDia` now uses
        `transformWithFreeze`.

    - optimized `Transformable` instance for `TransInv`

* **New instances**

    - `Eq`, `Ord`, `Enveloped`, `Traced`, and `Qualifiable` instances
      for `TransInv`

    - `Transformable` instance for functions, which acts by conjugation

* **API changes**

    - `named` and `namePoint` have moved to the `diagrams-lib` package.

* **Dependency/version changes**

    - allow `base-4.7`
    - upgrade to `monoid-extras-0.3`
    - allow `semigroups-0.9`

* **Bug fixes**

    - the `diameter` and `radius` functions now work correctly.

[active 0.1.0.6](http://hackage.haskell.org/package/active-0.1.0.6)
----------------

  - allow `semigroupoids-3.1`
  - allow `base-4.7`
  - allow `QuickCheck-2.6`
  - allow `semigroups-0.9`

[diagrams-lib 0.7](http://hackage.haskell.org/package/diagrams%2Dlib-0.7)
----------------

* **New features**

    - New module `Diagrams.TwoD.Curvature`, for computing the
      curvature of 2D segments at any given point.

    - New module `Diagrams.Offset`, containing an `offsetSegment`
      function that builds a trail a fixed distance from the original
      segment.  This is a precursor to planned functions `offsetTrail`
      and `offsetPath`.

    - New function `Diagrams.TwoD.Transform.onBasis`, for extracting a
      matrix representation of a 2D transformation

    - New functions `extrudeEnvelope` and `intrudeEnvelope`, for
      extending or shrinking an envelope only in a certain direction.

    - Generalize the `Color` class to absolute colors.
      This addresses concerns raised in issue #66 by letting the backend
      choose which color space to render `Color` instances to. Functions are
      provided for backwards compatibility with the old semantics.

    - New function `scaleInvPrim` for creating a diagram from a single
      scale-invariant primitive.

    - New module `Diagrams.Parametric`, containing a collection of
      classes abstracting over "parametric" things: `Parametric`,
      `DomainBounds`, `EndValues`, `Sectionable`, and `HasArcLength`,
      with instances for segments, trails, and related things.

    - A big refactoring of segments and trails:
        - Segments can now be either "closed" or "open".
        - There are now two types of trails: "lines" (which travel
          from point A to point B) or "loops" (closed curves which end
          where they started). `Trail` is now a wrapper type which can
          contain both loops and lines.
        - There is a new `Located` wrapper type for adding locations to
          translation-invariant things.  `Path`s now consist of a
          collection of `Located Trail`s.
        - The `PathLike` class is now renamed to `TrailLike`; the
          `trailLike` function takes a `Located Trail` as input.

    - New convenience functions `boundaryFrom` and `boundaryFromMay`,
      for computing boundaries of subdiagrams.

    - Re-export from `diagrams-lib` a lot of things defined in
      `diagrams-core`, to make them easier for users to find.  Several
      new modules have been created as a result: `Diagrams.Query`,
      `Diagrams.Envelope`, `Diagrams.Trace`, and `Diagrams.Names`.

    - Export the `centroid` function from `Diagrams.Prelude`.

    - `triangle` is now a synonym for `eqTriangle`.

* **New instances**

    - `IsPrim` instances for `Path`, `Ellipsoid`, `Image`, `Text`, and
      `ScaleInv`
    - `Eq`, `Ord`, and `Show` instances for `SizeSpec2D`

* **API changes**

    - `CircleFrac` has been renamed `Turn` (though `CircleFrac` is
      retained as a deprecated synonym).

    - `Diagrams.Coordinates` is no longer exported from
      `Diagrams.Prelude`.  This is for compatibility with `lens`, as `(&)`
      is a rather important lens operator and clashes with
      `Diagrams.Coordinates`.  Users who want the `Coordinates` stuff can import
      `Diagrams.Coordinates` explicitly.

* **Dependency/version changes**

    - allow `base-4.7`
    - allow `semigroups-0.9`
    - allow `NumInstances-1.3`
    - upgrade to `monoid-extras-0.3`
    - depend on `data-default-class` instead of `data-default`
    - Tested with GHC 7.7.

* **Bug fixes**

    - Quadratic solver is now more numerically stable, getting rid of some
      incorrect behavior of `juxtapose`
      ([\#46](https://github.com/diagrams/diagrams-lib/issues/46))

    - Added a special case that was a not handled properly by the
      quadratic solver, resulting in bogus envelopes in certain cases
      ([\#88](https://github.com/diagrams/diagrams-lib/issues/88)).

    - Import only `Data.NumInstances.Tuple` instead of
      `Data.NumInstances`. Previously, `Diagrams.Prelude` exported
      `Eq`, `Show`, and `Num` instances for functions and tuples; now
      it only exports tuple instances. Users wishing to use
      `Diagrams.CubicSpline` with a vector space built over functions
      (!?)  can import `Data.NumInstances.Function`
      themselves. ([\#48](https://github.com/diagrams/diagrams-lib/issues/48))

    - Do scaling on a `Path` *before* constructing a `TrailLike` in
      `rect` ([\#43](https://github.com/diagrams/diagrams-lib/issues/43))

[diagrams-contrib 0.7](http://hackage.haskell.org/package/diagrams%2Dcontrib-0.7)
----------------

* **New features**

    - New module `Diagrams.Color.HSV` with an `hsvBlend` function for
      blending colors in HSV space.

    - Diagrams logo code is now in `Diagrams.Example.Logo`.

    - New symmetric layout algorithm for binary trees in
      `Diagrams.TwoD.Layout.Tree`.

    - New `Diagrams.TwoD.Path.IteratedSubset` module, for constructing
      paths using an "iterated subset" algorithm (repeatedly replacing
      segments with a given path).

    - New `Diagrams.TwoD.Layout.CirclePacking` module for
      circle-packing layout

    - New `Diagrams.TwoD.Factorization` module, for creating
      "factorization diagrams" as seen at
      http://mathlesstraveled.com/2012/11/05/more-factorization-diagrams/
      and on the cover of Hacker Monthly
      (http://mathlesstraveled.com/2012/10/05/factorization-diagrams/).

    - `Diagrams.TwoD.Path.Turtle`: generalize `runTurtle` function,
      and add new functions `drawTurtle` and `sketchTurtle`.
      `drawTurtle` results in a diagram (like the old `runTurtle`),
      and `sketchTurtle` yields a path (ignoring pen style commands).

* **Dependency/version changes**

    - Require `lens-3.8`
    - allow `QuickCheck-2.6`

* **Bug fixes**

    - Fix a bug in `Diagrams.TwoD.Path.Turtle` which sometimes caused
      it to output a doubled path (#13).

* **Documentation**

    - Added lots of example images using `diagrams-haddock`

[diagrams-svg 0.7](http://hackage.haskell.org/package/diagrams%2Dsvg-0.7)
----------------

* **New features**

    - New `renderToSVG` convenience function
    - Vastly improved Haddock documentation

* **New instances**

    - `Show` instance for `Options SVG R2`

* **Dependency/version changes**

    - allow `base-4.7` and `unix-2.7`
    - Upgrade to `monoid-extras-0.3`

[diagrams-postscript 0.7](http://hackage.haskell.org/package/diagrams%2Dpostscript-0.7)
----------------

First release as an officially supported diagrams backend, generating
Postscript via native Haskell. Supports all features except
transparency.

[diagrams-cairo 0.7](http://hackage.haskell.org/package/diagrams%2Dcairo-0.7)
----------------

* **New features**

    - New `renderCairo` function for more convenient use of the cairo
      backend.
    - Lots of Haddock documentation improvements.

* **New instances**

    - `Show` instance for `Options Cairo R2`.

[diagrams-gtk 0.6.0.1](http://hackage.haskell.org/package/diagrams%2Dgtk-0.6.0.1)
----------------

The `diagrams-gtk` package continues to work with no changes;
`0.6.0.1` has been released simply to allow the dependency versions
`base-4.7`, `diagrams-lib-0.7`, and `diagrams-cairo-0.7`.

[diagrams-builder 0.4](http://hackage.haskell.org/package/diagrams%2Dbuilder-0.4)
----------------

* **New features**

    * Add `hsenv` compatibility.

    * Big improvements in the way rebuilding is handled:
        - Strip comments before deciding whether to rebuild, so
          changing only comments does not trigger a rebuild
        - Take local imports into account: if a diagram has an import
          which corresponds to a local file, rebuild if that file has
          changed
        - Rebuild when options (*e.g.* size) change

    * new `diagrams-builder-postscript` tool

    * miscellaneous improvements to `diagrams-latex.sty`

* **Dependency/version changes**

    * depend on `cryptohash >= 0.8 && < 0.10` (use new unified API)
    * remove `base16-bytestring` dependency

[diagrams-haddock 0.1.1.1](http://hackage.haskell.org/package/diagrams%2Dhaddock-0.1.1.1)
----------------

`diagrams-haddock` is a new tool for embedding automatically-generated
diagrams in Haddock documentation.  It has been released for a while,
but `0.1.1.1` is the first version officially included in a major
diagrams release.

[SVGFonts 1.3](http://hackage.haskell.org/package/SVGFonts)
----------------

First release as an officially supported diagrams package, adding nice
support to diagrams for text using fonts in SVG format.

diagrams 0.6: 11 December 2012
==============================

[diagrams-core 0.6](http://hackage.haskell.org/package/diagrams%2Dcore-0.6)
----------------

* **New features**

    - Proper support for subdiagrams: previous versions of
      diagrams-core had a mechanism for associating names with a pair
      of a location and an envelope.  Now, names are associated with
      actual subdiagrams (including their location and envelope, along
      with all the other information stored by a diagram).

        See
        [`Diagrams.Core.Types`](https://github.com/diagrams/diagrams-core/blob/27b275f45cad514caefcd3035e4e261f1b4adf6f/src/Diagrams/Core/Types.hs#L493).

    - Traces: in addition to an envelope, each diagram now stores a
      "trace", which is like an embedded raytracer: given any ray
      (represented by a base point and a vector), the trace computes
      the closest point of intersection with the diagram along the
      ray.  This is useful for determining points on the boundary of a
      diagram, *e.g.* when drawing arrows between diagrams.

        See [`Diagrams.Core.Trace`](https://github.com/diagrams/diagrams-core/blob/2f8727fdfa60cdf46456a23f358c8a771b2cd90d/src/Diagrams/Core/Trace.hs).

* **API changes**

    - The modules have all been renamed to be more consistent with the
      module naming scheme in the rest of the diagrams universe.  In
      particular:

        `Graphics.Rendering.Diagrams`       -->  `Diagrams.Core`
        `Grahpics.Rendering.Diagrams.Core`  -->  `Diagrams.Core.Types`
        `Graphics.Rendering.Diagrams.*`     -->  `Diagrams.Core.*`

    - `Graphics.Rendering.Diagrams.UDTree` has been split out into a
      separate
      [`dual-tree`](http://hackage.haskell.org/package/dual%2Dtree)
      package (which has also been substantially rewritten).

    - `Graphics.Rendering.Diagrams.{Monoids,MList}` have been split
      out into a separate [`monoid-extras`](http://hackage.haskell.org/package/monoid%2Dextras) package.

    - The `names` function now returns a list of names and their
      associated locations, instead of the associated subdiagrams.  In
      particular the output is suitable to be rendered to a `String`
      using `show`.

    - The new `subMap` function fills a similar role that `names` used
      to play, returning the entire mapping from names to subdiagrams.

    - New functions `envelope[VP]May`

        `envelopeV` and `envelopeP` return the zero vector and origin,
        respectively, when called on an empty envelope.  However,
        sometimes it's useful to actually know whether the envelope was
        empty or not (the zero vector and the origin are legitimate
        outputs from non-empty envelopes).  The new functions have their
        return type wrapped in `Maybe` for this purpose.

    - New functions `envelopeS` and `envelopeSMay`

        Like `envelope[VP](May)`, but returning a scalar multiple of
                the input vector.

    - The `Graphics.Rendering.Diagrams.Util` module has been removed,
      along with the `withLength` function.  Calls to `withLength` can
      be replaced using

        `withLength s v = s *^ normalized v`

    - Add needed constraints `(InnerSpace v, OrderedField (Scalar v),
      Monoid' m)` to the type of the `renderDias` method in the
      `MultiBackend` class.

    - Generalized `Transformable` instances for pairs and tuples

                Previously, the components of the tuples were required to have
                the same type; but everything still works as long as they all
                share the same vector space.  This is actually useful in
                practice: say, if we wanted to pair a diagram with a path and
                then apply the same transformation to both.

* **Improvements**

    - More efficient implementation of `diameter`

* **Dependency/version changes**

    - Tested with GHC 7.6.1
    - allow `base-4.6`
    - allow `containers-0.5.*`
    - allow `MemoTrie-0.6.1`

* **Bug fixes**

    - juxtaposeDefault now correctly handles empty envelopes (#37)

        `juxtaposeDefault` is now the identity on the second object if
        either one has an empty envelope.  In particular this means that
        `mempty` is now an identity element for `beside` and friends.

[active 0.1.0.2](http://hackage.haskell.org/package/active-0.1.0.2)
----------------

Bump dependency upper bounds:

- `semigroupoids` < 3.1
- `base` < 4.7
- `QuickCheck` < 2.6

[diagrams-lib 0.6](http://hackage.haskell.org/package/diagrams%2Dlib-0.6)
----------------

* **New features**

    - `boundingRect` function for constructing a bounding rectangle

    - `bg` function for "setting the background color" (*i.e.* placing
      atop a colored bounding rectangle)

    - New functions `setDefault2DAttributes` and `adjustDiaSize2D`.
      `adjustDia2D` does both --- so the behavior of `adjustDia2D` has
      not changed, but it is now possible to apply just one of the two
      adjustments using the new functions.

    - `Diagrams.TwoD.Transform` now exports a `ScaleInv` type for creating
      scale-invariant objects, which are only affected by rotational
      and translational components of transformations.

    - The new `Diagrams.Coordinates` module provides nicer syntax for
      constructing and pattern-matching point and vector literals.

    - New `fromFixedSeg` function in `Diagrams.Segment`, which
      decomposes a `FixedSegment` into a starting point and a `Segment`.

    - New `withTrace` function for setting the `Trace` of a diagram.

    - Three new size-related functions:

        - New `sized` function for scaling an object to a particular size.
          One particularly nice use of this is to obviate the need to keep
          fiddling with the line width to get diagrams to "look right";
          just set the line width relative to some arbitrary scale
          (*e.g.* assuming the final diagram will fit into a 1x1 box) and
          then apply `sized` to the final diagram to make it that given
          arbitrary size.  It can also be used for easily making something
          (a diagram, path, trail, ...) the same size as something else,
          with the help of the new `sizeSpec2D` function.

        - New `sizedAs` function, for setting the size of some object to
          be "the same as" some other object.

        - New `sizeSpec2D` function for conveniently calculating the size
          of an object as a `SizeSpec2D` value (for use with the new `sized`
          funtion).

    - New `extrudeEnvelope` and `intrudeEnvelope` functions for
      modifying envelopes in a single direction only, as well as new
      functions `extrude{Left,Right,Bottom,Top}` specializing
      `extrudeEnvelope` to 2D.

    - `arcCW` draws clockwise arcs; `arc'` draws arcs counterclockwise
      or clockwise as the radius is positive or negative,
      respectively.

    - fill color attribute is generalized to support "recommended" and
      "committed" colors; text objects use a recommended fill color of
      black.

* **New instances**

    - The `Show` instance for `R2` now produces something like `2 & 6`
      instead of `R2 { unR2 = (2,6) }`.  The `Read` instance has also
      been changed to match, so `read . show = id`.

    - `Enveloped` instance for `FixedSegment`

    - `Traced` instances for `Segment`, `FixedSegment`, `Trail`, and `Path`

    - New derived `Eq` instances for `LineCapA`, `LineJoinA`, `Dashing`,
      `DashingA`, `FillRule`, `Font`, `FontSize`, `FontSlant`, `FontSlantA`,
      `FontWeight`, and `FontWeightA`

    - `Renderable Ellipsoid NullBackend` instance

* **API changes**

    - `Data.Colour` (minus `atop` and `AffineSpace`) is now re-exported
      from Diagrams.Prelude for convenience.

    - The `beneath` function is now infixl 6.

    - The `BoundingBox` module has had a complete overhaul.  There is
      now a special empty bounding box, and bounding boxes are an
      instance of `Monoid`.

    - The type of `withEnvelope` has been slightly generalized.

    - `Diagrams.TwoD.Adjust.adjustSize` is now deprecated; it has been
      renamed and moved to `Diagrams.TwoD.Size.requiredScaleT`.

    - `expandPath` has been renamed to `scalePath`.

* **Dependency/version changes**

    - Allow `data-default` 0.4 and 0.5
    - Allow `base`-4.6
    - Allow `containers`-0.5

* **Bug fixes**

    - `arc` and `arcT` functions now always produce counterclockwise arcs,
      as claimed.

[diagrams-contrib 0.6](http://hackage.haskell.org/package/diagrams-contrib-0.6)
----------------

Version bumped to 0.6 to match other diagrams packages.

* **New features**

    - New pure implementation of Turtle library, in `Turtle.Internals`

    - `Diagrams.TwoD.Layout.Tree`:

        - New `renderTree'` function which gives
          the edge-drawing function access to the values stored at the
          nodes instead of just the node positions.

        - The type of `renderTree` is generalized to work with any
          `QDiagram b R2 m`.

* **Bug fixes**

    - Tiling generation code in `Diagrams.TwoD.Tilings` wasn't actually
      checking whether vertexes had been already visited.

* **Dependency/version changes**

    - Switch from `fclabels` to `lens`

[diagrams-cairo 0.6](http://hackage.haskell.org/package/diagrams-cairo-0.6)
----------------

* **New features**

    - New `--list` option for `multiMain` to list all available diagrams

    - Major documentation improvements

    - New modules:

        + `Diagrams.Backend.Cairo.Ptr`, for rendering directly to buffers
          in memory

        + `Diagrams.Backend.Cairo.List`, for rendering to a list of lists
          of pixels.

* **API changes**

    - Removal of `StyleParam` from `Diagrams.Backend.Cairo.Text`, change
      functions in that module to accept `Style R2`.  Usage can be fixed
      by applying these style functions to `mempty`.

    - GTK rendering has been split out into a new package, diagrams-gtk.

        + The `Diagrams.Backend.Cairo.Gtk` module is now
          `Diagrams.Backend.Gtk` in the `diagrams-gtk` package.

        + The `CairoOptions` record has a new boolean `cairoBypassAdjust`
          option; when set, the backend should bypass calling `adjustDia2D`.

        + The GTK output type is gone.

        + There is a new `RenderOnly` output type, for when you don't
          care about the `IO` action but only want the cairo `Render` action.

* **Dependency/version changes**

    - Upper bounds relaxed to allow
      `base`-4.6, `unix`-2.6, `cmdargs`-0.10, `split`-0.2.*, `mtl`-2.1

    - Add a dependency on `time`, and conditional compilation to use
      either ClockTime or UTCTime depending on the version of the
      `directory` package

    - Add dependency on `colour`

    - Lower bound on `cairo` raised to 0.12.4

* **Bug fixes**

    - Fixed looped compile mode, which was repeatedly trying to compile
      when the code contained errors, instead of trying once and then
      waiting for a change.

    - Fix a bug where default attributes were not being set when using
      the "bypass" mode used by the gtk backend. ([\#16](https://github.com/diagrams/diagrams-cairo/pull/16))

[diagrams-gtk 0.6](http://hackage.haskell.org/package/diagrams-gtk-0.6)
----------------

Initial release.  Split out into a separate package from
`diagrams-cairo`.

[diagrams-svg 0.6](http://hackage.haskell.org/package/diagrams-svg-0.6)
----------------

First "officially supported" release.

Features still not implemented:

- text alignment
- inline images

As of this release everything else Should Work (tm).

[diagrams-builder 0.2.1.0](http://hackage.haskell.org/package/diagrams-builder-0.2.1.0)
----------------

+ new `diagrams-builder-svg` tool
+ deal properly with an empty list of sources ([\#2](https://github.com/diagrams/diagrams-builder/issues/2))
+ put cached diagrams in `.diagrams_cache` instead of `diagrams` by default
+ bug fix: cached files should use same extension as requested output, not "png"
+ bug fix: create output directory for cached images if it doesn't exist
+ new module `Diagrams.Builder.CmdLine`; factor out common utilities
  for command-line tools
+ depend on 0.6 versions of diagrams libs

diagrams 0.5: 9 March 2012
==========================

[diagrams-core](http://hackage.haskell.org/package/diagrams%2Dcore) 0.5
-----------------

  * New features:
    - New `Juxtaposable` class
    - New `NullBackend` and `D` types, for conveniently giving a
      monomorphic type to diagrams when we don't care which one it is.
    - [\#27](http://code.google.com/p/diagrams/issues/detail?id=27): Change type of `adjustDia` to return a new options record
      (with an explicitly filled-in size)

  * New instances:
    - `Enveloped`, `HasOrigin`, `Juxtaposable`, `HasStyle`, and `Transformable`
      instances for `Set`s and tuples
    - `V Double = Double`
    - `Juxtaposable` and `Boundable` instances for `Map`

  * API changes
    - `AnnDiagram` renamed to `QDiagram`
    - [\#61](http://code.google.com/p/diagrams/issues/detail?id=61): terminology change from "bounds" to "envelope"
        + `boundary` -> `envelopeP`
        + "bounding region" -> "envelope"
        + `Bounds` -> `Envelope`
        + `Boundable` -> `Enveloped`
        + `getBounds` -> `getEnvelope`
        + *etc.*
    - Split out definition of `Point` into separate package
      ([`vector-space-points`](http://hackage.haskell.org/package/vector%2Dspace%2Dpoints))
    - The `Point` constructor `P` is no longer exported from
      `Graphics.Rendering.Diagrams`.  See the `Diagrams.TwoD.Types` module
      from `diagrams-lib` for new tools for working with abstract 2D
      points.  If you really need the `P` constructor, import
      `Graphics.Rendering.Diagrams.Points`.
    - Name-related functions now return "located bounding functions"
      instead of pairs of points and bounds, to allow for future
      expansion.

  * Dependency/version changes:
    - `vector-space` 0.8 is now required.
    - Bump base upper bound to allow 4.5; now tested with GHC 7.4.1.

  * Bug fixes:
    - Bug fix related to empty envelopes

[active](http://hackage.haskell.org/package/active) 0.1.0.0
--------------

  * Initial release

[diagrams-lib](http://hackage.haskell.org/package/diagrams%2Dlib) 0.5
------------

  * New features:
    - `mkSizeSpec` function for constructing a `SizeSpec2D` from two `Maybe Double`s
    - `beneath` as convenient synonym for `flip atop`
    - Improvements and extensions to rounded rectangles by Peter Hall:
        + `roundedRect'` allows rounded rectangles with a different radius
          specified for each corner
        + both `roundedRect'` and `roundedRect` now allow negative radii,
          resulting in "inverted" circular corners
    - [\#64](http://code.google.com/p/diagrams/issues/detail?id=64): New `Alignable` class for things that can be aligned.
    - `explodeTrail` and `explodePath` have been generalized to return any
      `PathLike` type.
    - New path functions `pathCentroid` (compute the centroid of a
      path's vertices) and `expandPath` (scale a path about its centroid).
    - Generalized `centroid` function now exported from new module
      `Diagrams.Points`.
    - Initial (experimental) support for animation:
        + `Animation` and `QAnimation` defined as synonyms for `Active`
          diagrams (see `active` package)
        + Instances for `Active`: `V`, `HasOrigin`, `Transformable`,
          `HasStyle`, `PathLike`, `Juxtaposable`, `Alignable`
        + `animEnvelope` and `animRect` functions for automatic bounding of
          animations
    - `addClosingSegment` function for making the implicit closing
      segment of a closed trail explicit
    - Improvements to `BoundingBox` module from Michael Sloan: querying
      of `BoundingBox` bounds, corners, extents, and transformation of
      objects to fit within a given box.
    - Text alignment options from Michael Sloan
    - `view` function for restricting a diagram's envelope to a
      rectangular region.
    - `iterateN` function for iterating a finite number of times
    - `atAngle` for placing two diagrams next to each other along a
      specified angle.
    - `padX` and `padY` functions for padding in the X- and Y-directions
      independently.
    - generalized `showOrigin` function from Ian Ross
    - [\#40](http://code.google.com/p/diagrams/issues/detail?id=40): add shears to `Diagrams.TwoD.Transform`

  * Performance improvements
    - Use a balanced folding scheme for `cat'`, reducing time in some
      cases from \\(O(n^2)\\) to \\(O(n \\log n)\\)
    - More efficient implementation of `beside`

  * New instances:
    - `Alignable` instances for `QDiagram`, `Path`, `Envelope`, `Active`, `Set`,
      `Map`, `[]`
    - `Renderable` instances for `NullBackend` (`Trail`, `Path`, `Segment`,
      `Image`, `Text`)
    - Instances for `Active`: `V`, `HasOrigin`, `Transformable`,
      `HasStyle`, `PathLike`, `Juxtaposable`, `Alignable`

  * API changes
    - `R2` used to be a synonym for `(Double, Double)` but is now
      abstract. To convert between pairs of `Doubles` and `R2`, use the
      new functions `r2` and `unr2`.  There are two reasons for this
      change:
          1. to allow for future changes to the implementation of `R2`;
          2. `(Double, Double)` was an awkward special case getting in the
           way of useful tuple instances for classes like `HasOrigin`,
           `Enveloped`, and so on.
    - `circlePath` has been removed; its functionality has been
      subsumed by `circle`.
    - `adjustSegment` now takes an extra tolerance option.
    - Ellipses are now represented using Bezier approximations rather
      than a separate special type.
    - `BoundingBox` no longer has a `Transformable` instance; the old
      instance was misleading at best.
    - Change semantics of `beside` (hence also `(|||)` and `(===)`) so the
      result's origin is the same as that of the first argument.
    - `adjustDia2D` now takes a `SizeSpec2D`.
    - `beside` and related functions are now implemented in terms of
      `juxtapose`.
    - Instead of taking an `R2`, `roundedRect` now takes a pair of
      `Double`s, to be more consistent with `rect`.

  * Dependency/version changes
    - Support for GHC 7.4.1:
        + depend on `colour` >= 2.3.2
        + update `base` and `array` upper bounds
    - bump `vector-space` upper bound

  * Bug fixes
    - Avoid scale by zero error in `showOrigin`.
    - Base `adjustDia2D` translation on output size rather than diagram size.

[diagrams-contrib](http://hackage.haskell.org/package/diagrams%2Dcontrib) 0.1.0.0
----------------

  * Initial release

[diagrams-cairo](http://hackage.haskell.org/package/diagrams%2Dcairo) 0.5
--------------

  * New features
    - New `Diagrams.Backend.Cairo.Text` module by Michael Sloan, with
      functions for creating appropriately sized text objects by
      querying cairo for the size, and related supporting functions.
    - Basic support for animation with `animMain` function, by
      generating frames sampled at regular intervals.
    - Proper vertical alignment of default text based on font
      parameters (Michael Sloan).
    - Requesting just a width or height now causes the other to be
      computed appropriately.

  * API changes
    - Move `Diagrams.Backend.Cairo` to
      `Diagrams.Backend.Cairo.Internal` and export everything.
      `Diagrams.Backend.Cairo` now just re-exports selected functions
      from `Internal`.  This allows anyone who wants access to the
      helper/utility functions to import `Internal`.

  * Dependency/version changes
    - relax `cmdargs` upper bound
    - GHC 7.4.1 compatibility: update `base`, `filepath`, and
      `old-time` upper bounds

  * Bug fixes
    - [\#54](http://code.google.com/p/diagrams/issues/detail?id=54): Generate warning for missing image files (Ian Ross).
