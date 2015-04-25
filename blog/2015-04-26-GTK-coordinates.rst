.. role:: pkg(literal)
.. role:: hs(literal)
.. role:: mod(literal)
.. role:: repo(literal)

.. default-role:: hs

================================================
Diagrams + Cairo + Gtk + Mouse picking, reloaded
================================================

A little over a year ago, Christopher Mears wrote `a nice article on
how to match up mouse clicks in a GTK window with parts of a
diagram`__.  The only downside was that to make it work, you had to
explicitly construct the diagram in such a way that its coordinate
system precisely matched the coordinates of the window you wanted to
use, so that there was essentially no "translation" to do.  This was
unfortunate, since constructing a diagram in a particular global
coordinate system is not a very "diagrams-y" sort of thing to do.
However, the 1.3 release of diagrams includes a new feature that makes
matching up mouse clicks and diagrams much easier and more idiomatic,
and I thought it would be worth updating Chris's original example to
work more idiomatically in diagrams 1.3. The complete code is listed
at the end.

__ http://www.cmears.id.au/articles/diagrams-gtk-mouse.html

First, here's how we construct the house.  This is quite different
from the way Chris did it; I have tried to make it more idiomatic by
focusing on local relationships of constituent pieces, rather than
putting everything at absolute global coordinates.  We first create
all the constituent pieces:

.. class:: lhs

::

> -- The diagram to be drawn, with features tagged by strings.
> prettyHouse :: QDiagram Cairo V2 Double [String]
> prettyHouse = house
>   where
>     roof    = triangle 1   # scaleToY 0.75 # centerY # fc blue
>     door    = rect 0.2 0.4 # fc red
>     handle  = circle 0.02  # fc black
>     wall    = square 1     # fc yellow
>     chimney = fromOffsets [0 ^& 0.25, 0.1 ^& 0, 0 ^& (-0.4)]
>             # closeTrail # strokeT # fc green
>             # centerX
>             # named "chimney"
>     smoke = mconcat
>       [ circle 0.05 # translate v
>       | v <- [ zero, 0.05 ^& 0.15 ]
>       ]
>       # fc grey

We then put the pieces together, labelling each by its name with the
`value` function. Diagrams can be valuated by any monoid; when two
diagrams are combined, the value at each point will be the `mappend`
of the values of the two component diagrams.  In this case, each point
in the final diagram will accumulate a list of `String`\s
corresponding to the pieces of the house which are under that point.
Note how we make use of combinators like `vcat` and `mconcat`,
alignments like `alignB`, `snugL` and `snugR`, and the use of a named
subdiagram (the chimney) to position the components relative to each
other. (You can click on any of the above function names to go to
their documentation!)

.. class:: lhs

::

>     house = vcat
>       [ mconcat
>         [ roof    # snugR                   # value ["roof"]
>         , chimney # snugL                   # value ["chimney"]
>         ]
>         # centerX
>       , mconcat
>         [ handle  # translate (0.05 ^& 0.2) # value ["handle"]
>         , door    # alignB                  # value ["door"]
>         , wall    # alignB                  # value ["wall"]
>         ]
>       ]
>       # withName "chimney" (\chim ->
>           atop (smoke # moveTo (location chim) # translateY 0.4
>                       # value ["smoke"]
>                )
>         )

.. class:: dia

::

> prettyHouse :: QDiagram B V2 Double [String]
> prettyHouse = house
>   where
>     roof    = triangle 1   # scaleToY 0.75 # centerY # fc blue
>     door    = rect 0.2 0.4 # fc red
>     handle  = circle 0.02  # fc black
>     wall    = square 1     # fc yellow
>     chimney = fromOffsets [0 ^& 0.25, 0.1 ^& 0, 0 ^& (-0.4)]
>             # closeTrail # strokeT # fc green
>             # centerX
>             # named "chimney"
>     smoke = mconcat
>       [ circle 0.05 # translate v
>       | v <- [ zero, 0.05 ^& 0.15 ]
>       ]
>       # fc grey
>     house = vcat
>       [ mconcat
>         [ roof    # snugR                   # value ["roof"]
>         , chimney # snugL                   # value ["chimney"]
>         ]
>         # centerX
>       , mconcat
>         [ handle  # translate (0.05 ^& 0.2) # value ["handle"]
>         , door    # alignB                  # value ["door"]
>         , wall    # alignB                  # value ["wall"]
>         ]
>       ]
>       # withName "chimney" (\chim ->
>           atop (smoke # moveTo (location chim) # translateY 0.4
>                       # value ["smoke"]
>                )
>         )
>
> example = prettyHouse # resetValue

Now, when we render the diagram to a GTK window, we can get diagrams
to give us an affine transformation that mediates between the
diagram's local coordinates and the GTK window's coordinates.  I'll
just highlight a few pieces of the code; the complete listing can be
found at the end of the post.  We first create an ``IORef`` to hold
the transformation:

.. class:: lhs

::

>   gtk2DiaRef <- (newIORef mempty :: IO (IORef (T2 Double)))

We initialize it with `mempty`.  We use the `renderDiaT` function to
get not only a rendering action but also the transformation from
diagram to GTK coordinates; we save the inverse of the transformation
in the ``IORef`` (since we will want to convert from GTK to diagram
coordinates):

.. class:: lhs

::

>     let (dia2gtk, (_,r)) = renderDiaT Cairo
>                              (CairoOptions "" (mkWidth 250) PNG False)
>                              prettyHouse
>
>     -- store the inverse of the diagram -> window coordinate transformation
>     -- for later use in interpreting mouse clicks
>     writeIORef gtk2DiaRef (inv dia2gtk)

(Note that if it is possible for the first motion notify event to
happen before the expose event, then such mouse motions will be
computed to correspond to the wrong part of the diagram, but who
cares.)  Now, when we receive a mouse click, we apply the stored
transformation to convert to a point in diagram coordinates, and pass
it to the `sample` function to extract a list of house components at
that location.

.. class:: lhs

::

>     (x,y) <- eventCoordinates
>
>     -- transform the mouse click back into diagram coordinates.
>     gtk2Dia <- liftIO $ readIORef gtk2DiaRef
>     let pt' = transform gtk2Dia (p2 (x,y))
>
>     liftIO $ do
>       putStrLn $ show (x,y) ++ ": "
>                    ++ intercalate " " (sample prettyHouse pt')

The final product ends up looking and behaving identically to the
`video that Chris made`__.

__ https://www.youtube.com/watch?v=vwf9aVdDipo

Finally, here's the complete code.  A lot of it is just boring
standard GTK setup.

.. class:: lhs

::

> import           Control.Monad                   (void)
> import           Control.Monad.IO.Class          (liftIO)
> import           Data.IORef
> import           Data.List                       (intercalate)
> import           Diagrams.Backend.Cairo
> import           Diagrams.Backend.Cairo.Internal
> import           Diagrams.Prelude
> import           Graphics.UI.Gtk
>
> main :: IO ()
> main = do
>   -- Ordinary Gtk setup.
>   void initGUI
>   w <- windowNew
>   da <- drawingAreaNew
>   w `containerAdd` da
>   void $ w `on` deleteEvent $ liftIO mainQuit >> return True
>
>   -- Make an IORef to hold the transformation from window to diagram
>   -- coordinates.
>   gtk2DiaRef <- (newIORef mempty :: IO (IORef (T2 Double)))
>
>   -- Render the diagram on the drawing area and save the transformation.
>   void $ da `on` exposeEvent $ liftIO $ do
>     dw <- widgetGetDrawWindow da
>
>     -- renderDiaT returns both a rendering result as well as the
>     -- transformation from diagram to output coordinates.
>     let (dia2gtk, (_,r)) = renderDiaT Cairo
>                              (CairoOptions "" (mkWidth 250) PNG False)
>                              prettyHouse
>
>     -- store the inverse of the diagram -> window coordinate transformation
>     -- for later use in interpreting mouse clicks
>     writeIORef gtk2DiaRef (inv dia2gtk)
>
>     renderWithDrawable dw r
>     return True
>
>   -- When the mouse moves, show the coordinates and the objects under
>   -- the pointer.
>   void $ da `on` motionNotifyEvent $ do
>     (x,y) <- eventCoordinates
>
>     -- transform the mouse click back into diagram coordinates.
>     gtk2Dia <- liftIO $ readIORef gtk2DiaRef
>     let pt' = transform gtk2Dia (p2 (x,y))
>
>     liftIO $ do
>       putStrLn $ show (x,y) ++ ": "
>                    ++ intercalate " " (sample prettyHouse pt')
>       return True
>
>   -- Run the Gtk main loop.
>   da `widgetAddEvents` [PointerMotionMask]
>   widgetShowAll w
>   mainGUI
>
> -- The diagram to be drawn, with features tagged by strings.
> prettyHouse :: QDiagram Cairo V2 Double [String]
> prettyHouse = house
>   where
>     roof    = triangle 1   # scaleToY 0.75 # centerY # fc blue
>     door    = rect 0.2 0.4 # fc red
>     handle  = circle 0.02  # fc black
>     wall    = square 1     # fc yellow
>     chimney = fromOffsets [0 ^& 0.25, 0.1 ^& 0, 0 ^& (-0.4)]
>             # closeTrail # strokeT # fc green
>             # centerX
>             # named "chimney"
>     smoke = mconcat
>       [ circle 0.05 # translate v
>       | v <- [ zero, 0.05 ^& 0.15 ]
>       ]
>       # fc grey
>     house = vcat
>       [ mconcat
>         [ roof    # snugR                  # value ["roof"]
>         , chimney # snugL                  # value ["chimney"]
>         ]
>         # centerX
>       , mconcat
>         [ handle  # translate (0.05 ^& 0.2) # value ["handle"]
>         , door    # alignB                  # value ["door"]
>         , wall    # alignB                  # value ["wall"]
>         ]
>       ]
>       # withName "chimney" (\chim ->
>           atop (smoke # moveTo (location chim) # translateY 0.4
>                       # value ["smoke"]
>                )
>         )
