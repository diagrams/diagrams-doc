---
title: Releases
---

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
