Introduction
============

The system diagrams uses for constructing paths can seem overly complex
at first---there are many different types involved, and a plethora of
ways to convert between them. However, it is the result of deep thought
and careful design---that's not to say it is perfect, but for the most
part, things are the way they are for a reason. The concept of a "path"
has quite a few inherent subtleties!

The goal of this tutorial, therefore, is to give you proficiency in
working with trails and paths via practice on numerous exercises. This
tutorial is not intended to be a comprehensive reference; for that,
consult the [user manual](manual.html#trails-and-paths). As a
prerequisite, you should already be familiar with the material in the
[tutorial on vectors and points](vector.html).

Segments
========

The basic building block of diagrams paths is the *segment*. For the
purposes of this tutorial, however, it is enough to know that a segment
can be either a straight line segment or a (cubic Bézier) curve, and
that the lines, loops, trails, and paths we will meet in the following
sections are all built out of segments. Typically, in creating diagrams,
one does not need to work directly with segments. Consult the [user
manual](manual.html#segments) if you wish to know more about them.

Lines
=====

The first foundational concept to understand is that of a *line*, which
is a *translation-invariant path through space*, consisting of a
sequence of segments end-to-end. Note that a line can be arbitrarily
kinked and curved. Think not of a "straight line", but rather of a
"train line" or a "subway line".

A line is a kind of *trail* (we will meet the other kind in the
following section), and has type `Trail' Line v`{.hs} for some vector
type `v`{.hs} (typically `V2 Double`{.hs}).

Constructing lines
------------------

Lines are an instance of the `TrailLike`{.hs} class, so to construct a
line, you can use any function with a return type like `TrailLike t =>
... -> t`{.hs}. Examples of such functions include `fromOffsets`{.hs},
`fromVertices`{.hs}, `fromSegments`{.hs}, `(~~)`{.hs}, `circle`{.hs},
`arc`{.hs}, `triangle`{.hs}, `square`{.hs}, `pentagon`{.hs},
`rect`{.hs}, `roundedRect`{.hs}, `polygon`{.hs}, `arc`{.hs}, and
`cubicSpline`{.hs}, among many others (click a function name to see its
type, its documentation, and other nearby functions).

A line can be turned into a diagram with `strokeLine`{.hs}. Since lines
are *translation-invariant*, they have no definite location in space,
and `strokeLine`{.hs} must arbitrarily pick a location; by default, the
origin is chosen as the starting location of the line. (Of course, in
many situations this does not matter.)

<div class="warning">

Lines are never filled, so setting a fill color on a line has no effect.
(For things that do get filled, see the next section on `Loops`{.hs}\_.)

</div>

Below are a series of diagrams that you should attempt to reproduce.
Each exercise lists functions that you may find useful in completing it.
If you are really stuck or want to check your answers, you can find the
source code for this tutorial on github.

<div class="exercises">

1.  `fromOffsets`{.hs}

    ``` {.diagram}
    example
      = fromOffsets [unitX, 2 *^ unitY, 2 *^ unitX]
      # strokeLine
      # centerXY # pad 1.1
    ```

2.  `lineOffset`{.hs}, `direction`{.hs}

    ``` {.diagram}
    ln :: Trail' Line V2 Double
    ln    = fromOffsets [unitX, 2 *^ unitY, 2 *^ unitX]
    theta = angleBetween unitX (lineOffset ln)
    example
      = ln # rotate theta
      # strokeLine
      # centerXY # pad 1.1
    ```

3.  `fromVertices`{.hs}

    ``` {.diagram}
    pts = [ p2 (x,y) | x <- [0 .. 4], y <- [0, 1] ]
    example = pts # fromVertices # strokeLine
            # centerXY # pad 1.1
    ```

4.  `pentagon`{.hs}

    ``` {.diagram}
    example = pentagon 1 # strokeLine # centerXY # pad 1.1
    ```

5.  `pentagon`{.hs}, `onLineSegments`{.hs}

    ``` {.diagram}
    example = pentagon 1 # onLineSegments tail # strokeLine
            # centerXY # pad 1.1
    ```

</div>

Composing lines
---------------

A very important feature of lines is that they are an instance of
`Monoid`{.hs}, with the empty line (containing no segments) as
`mempty`{.hs}, and concatenation of lines as `mappend`{.hs} (aka
`<>`{.hs}).

<div class="exercises">

1.  `mappend`{.hs}

    ``` {.diagram}
    p = pentagon 1 # onLineSegments init
    example = (p <> p) # strokeLine
            # centerXY # pad 1.1
    ```

2.  `iterateN`{.hs}, `rotateBy`{.hs}, `mconcat`{.hs}

    ``` {.diagram}
    p = pentagon 1 # onLineSegments init
    example = iterateN 5 (rotateBy (1/5)) p
       # mconcat # strokeLine # centerXY # pad 1.1 # sized (Width 2)
    ```

3.  `reverseLine`{.hs}

    ``` {.diagram}
    hanoi 0 = mempty
    hanoi n = mconcat
      [ h' # rotateBy (-1/3) # reverseLine
      , fromOffsets [unitX] # rotateBy (1/6)
      , h'
      , fromOffsets [unitX] # rotateBy (-1/6)
      , h' # rotateBy (1/3) # reverseLine
      ]
      where h' = hanoi (n-1)

    example = hanoi 4 # strokeLine
      # centerXY # pad 1.1 # sized (Width 2)
    ```

</div>

Loops
=====

A *loop* is another kind of trail, with type `Trail' Loop v n`{.hs}.
Loops are like lines, except for the fact that they are "closed": they
end in the same place where they start, and have an "inside" and an
"outside".

Constructing loops
------------------

Loops are also an instance of `TrailLike`{.hs}, so many of the same
functions mentioned in the previous section for constructing lines can
also be used to construct loops.

Loops can be turned into diagrams with `strokeLoop`{.hs}.

<div class="exercises">

1.  `strokeLoop`{.hs}

    ``` {.diagram}
    example = pentagon 1 # strokeLoop # fc blue # centerXY # pad 1.1
    ```

2.  Change `strokeLoop`{.hs} to `strokeLine`{.hs} in your solution to
    the previous exercise. Explain the difference in the output.

</div>

Converting between lines and loops
----------------------------------

There are two functions which allow converting a line into a loop. The
first is `glueLine`{.hs}. It simply assumes that the line ends in the
same place that it starts, and "glues" the line closed. (If the line
does not end in the same place that it starts, the final segment will be
altered so that it does.)

Note that unlike lines, loops *do not* have a `Monoid`{.hs} instance.
One common pattern for constructing complicated loops is to concatenate
some lines and then call `glueLine`{.hs} on the result. You try:

<div class="exercises">

1.  `glueLine`{.hs}

    ``` {.diagram}
    p = pentagon 1 # onLineSegments init
    example = iterateN 5 (rotateBy (1/5)) p
       # mconcat # glueLine # strokeLoop
       # fc green # centerXY # pad 1.1 # sized (Width 2)
    ```

2.  ``` {.diagram}
    step = fromOffsets [unitY, unitX]
    steps n = mconcat (replicate n step)
           <> step # rotateBy (1/2) # scale (fromIntegral n)
    example = steps 5 # glueLine # strokeLoop # fc red
      # centerXY # pad 1.1 # sized (Width 2)
    ```

3.  ``` {.diagram}
    andThen t1 t2 = t1 <> t2 # rotate (angleBetween d1 d2)
      where
        d1 = tangentAtEnd t1
        d2 = tangentAtStart t2

    str = fromOffsets [unitX]
    cap = arc xDir (1/2 @@ turn)
    arm = str `andThen` cap `andThen` str

    armUnit = arm `andThen` (arc xDir (3/10 @@ turn) # reflectX)

    example = foldr andThen mempty (replicate 5 armUnit)
      # glueLine # strokeLoop # fc blue
      # rotateBy (1/20)
      # centerXY # pad 1.1 # sized (Width 2)
    ```

    You may find this function useful:

    ``` {.haskell}
    andThen t1 t2 = t1 <> t2 # rotate (d1 ^-^ d2)
      where
        d1 = direction (tangentAtEnd t1)
        d2 = direction (tangentAtStart t2)
    ```

</div>

The second function for converting from lines to loops,
`closeLine`{.hs}, adds an extra (linear) segment from the end of the
line to the beginning.

<div class="exercises">

1.  `closeLine`{.hs}

    ``` {.diagram}
    trap = fromOffsets
             [ unitY # rotateBy (-1/15)
             , unitX
             , unit_Y # rotateBy (1/15)
             ]
           # closeLine
    example = trap # strokeLoop # centerXY # pad 1.1
    ```

2.  ``` {.diagram}
    s = fromOffsets [unitY # rotateBy (-1/30)]
    tine = s <> s # reflectX # reverseLine
    tines = mconcat . replicate 10 $ tine
    comb = s <> tines <> s # reflectX # reverseLine
    example = comb # closeLine # strokeLoop # fc yellow
      # centerXY # pad 1.1 # sized (Width 2)
    ```

</div>

Finally, to convert from a loop to a line, use `cutLoop`{.hs}, which
"cuts" a loop at its shared start/end point, resulting in a line which
"just happens" to end where it starts. It is harder to come up with
exercises requiring the use of `cutLoop`{.hs}; in most cases where you
might think of using it, you could simply construct a line in the first
place. For example,

``` {.haskell}
(square 1 :: Trail' Loop V2 Double) # cutLoop :: Trail' Line V2 Double
```

is exactly the same as `square 1 :: Trail' Line V2 Double`{.hs}. So
there are no exercises here; it's simply useful to be aware that in any
situation where something that is naturally a loop is interpreted as a
line (for example, `square 1 :: Trail' Line V2 Double`{.hs}),
`cutLoop`{.hs} is being used under the hood.

Trails
======

We have now seen both types of trails. The `Trail`{.hs} type is simply a
wrapper around both lines and loops. That is, something of type
`Trail v n`{.hs} is either a line or a loop, wrapped up so the type does
not tell you which it is (though it is possible to recover the
information dynamically, using functions like `withTrail`{.hs}). To make
a line or loop into a `Trail`{.hs}, use `wrapLine`{.hs} or
`wrapLoop`{.hs}, respectively. Many of the functions we have seen on
lines and loops have corresponding versions that operate on
`Trail`{.hs}s, such as `strokeTrail`{.hs}, `glueTrail`{.hs},
`closeTrail`{.hs}, `reverseTrail`{.hs}, and `cutTrail`{.hs}.

Located
=======

The `Located`{.hs} wrapper associates a point location with an object,
turning translation-invariant things into located things.

To give a location to something, use `at :: a -> Point (V a) (N a) ->
Located a`{.hs}. Located lines, loops, and trails can be turned into
diagrams with `strokeLocLine`{.hs}, `strokeLocLoop`{.hs}, and
`strokeLocTrail`{.hs} respectively.

One reason you may sometimes want to work with `Located`{.hs} trails is
when using `explodeTrail`{.hs} to turn a trail into a collection of
`Located`{.hs} trails, one for each individual segment. Using
`Located`{.hs} in this way remembers the locations of the segments
relative to one another.

<div class="exercises">

1.  `explodeTrail`{.hs}, `mapLoc`{.hs}

    ``` {.diagram}
    example
      = explodeTrail (heptagon 1)
      # map (strokeLocTrail . mapLoc (rotateBy (1/20)))
      # mconcat
      # centerXY # pad 1.1 # sized (Width 2)
    ```

2.  ``` {.diagram}
    sqTrail :: Trail' Line V2 Double
    sqTrail = iterateN 4 (rotateBy (1/4))
                (fromOffsets (replicate 4 unitX))
              # mconcat
    example
      = sqTrail
      # wrapLine # (`at` origin)
      # explodeTrail
      # map strokeLocTrail
      # zipWith lc (cycle [red, blue])
      # mconcat
      # lwG 0.05
      # centerXY # pad 1.1 # sized (Width 2)
    ```

</div>

Paths
=====

A *path* is simply a collection of located trails.

<div class="exercises">

1.  `star`{.hs}, `pathTrails`{.hs}

    ``` {.diagram}
    s :: Path V2 Double
    s = star (StarSkip 5) (regPoly 30 1)

    example
      = s
      # pathTrails
      # map strokeLocTrail
      # zipWith lc [red,orange,yellow,blue,green,purple]
      # mconcat
      # lwG 0.03
      # centerXY # pad 1.1 # sized (Width 2)
    ```

2.  `atPoints`{.hs}, `fillRule`{.hs}

    ``` {.diagram}
    innerCircles :: Path V2 Double
    innerCircles = atPoints (trailVertices $ hexagon 2) (repeat (circle 1)) <> circle 1

    example = (innerCircles <> circle 3) # stroke # fc blue # fillRule EvenOdd
    ```

</div>