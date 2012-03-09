---
title: Releases
---

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
