.. role:: pkg(literal)
.. role:: hs(literal)
.. role:: mod(literal)
.. role:: repo(literal)

.. default-role:: hs
.. sectnum:: :depth: 2

.. contents:: :depth: 2

Introduction
============

A common pattern when using ``diagrams`` is to make a program that generates a
single diagram.  Often there are details about the diagram that we would like
to delay in fixing.  In particular the output file, image format, and final
size of the diagram are all standard options we would expect for making a
diagram that are relatively independent of the image being made.  With the
`Diagrams.Backend.SVG.CmdLine`:mod: module (and related modules in other
backends) we provide easy creation of a command-line interface that supports
standard options as well as easy customization for additional parameters.

To give some concrete examples, the following are programs one can
write using the tools already provided, and their interaction on the
command-line.  For an example of a custom extension to this framework,
see the `User Extensions`_ section below.

First is the simplest case of generating a single diagram:

.. class:: lhs

::

> -- Simple
>
> d :: Diagram SVG V2 Double
> d = ...
>
> main = mainWith d

Here we just have a diagram and the standard options.  We can invoke
with just a width and the height is made to match the diagram.

::

    $ ./Simple -o simple.svg -w 100

If we have multiple diagrams with names we can use `mainWith` to give an
interface that allows the selection of a particular diagram by name.

.. class:: lhs

::

> -- Multiple
>
> d1, d2, d3 :: Diagram SVG V2 Double
> ...
>
> main = mainWith [("First", d1),("Second", d2),("Third", d3)]

The ``--list`` option just lists the available diagrams to render and
the ``-S`` selection option takes a name and renders the associated
diagram with the standard options.

::

    $ ./Multiple --list
    Available diagrams:
      First Second Third
    $ ./Multiple -o d1.svg -w 100 -S First

Some backends support rendering animations (typically as individually indexed
files of frames).

.. class:: lhs

::

> -- Animation
>
> a :: Animation Cairo V2 Double
> a = ...
>
> main = mainWith a

The default options are expected, but the output file has an index appended
to the name for each frame.  The ``--fpu`` option indicates the desired number
of frames per unit of time.  If the ``a`` animation above is one second long
then the following will create files ``a01.png`` through ``a24.png``.

::

   $ ./Animation -o a.png -w 100 --fpu 24

In backends that support multiple pages we can list all the diagrams and
have each render on its own page.

.. class:: lhs

::

> -- Pages
>
> d1, d2, d3 :: Diagram Postscript V2 Double
> ...
>
> main = mainWith [d1,d2,d3]

We only need the default options here and the interface is the same as a
single diagram.

::

    $ ./Pages -o pages.ps -w 400

To make things more interesting we could require additional arguments to
build a diagram.  We can take a function to build a diagram from some
parameters and create an interface that fills those parameters with
arguments from the command-line.

.. class:: lhs

::

> -- Function
>
> f :: Colour Double -> Double -> Diagram SVG V2 Double
> f c x = ...
>
> main = mainWith f

In addition to the standard flags, we can provide arguments ``blue`` and ``42.0`` which
will be passed along to ``f``.

::

    $ ./Function -o blue.svg -w 400 blue 42.0

In addition to `Colour`\s and `Double`\s, default command-line parsers are
provided for `Int`, `String`, and `AlphaColour` arguments.  You can
also easily define your own parsers for additional argument types; see
the `User Extensions`_ section below.

Diagrams that are the result of executing an IO action can also be handled
by `mainWith`.  This can be useful for reading input files or doing other
IO that the diagram depends on.

.. class:: lhs

::

> -- IO-diagram
>
> d :: FilePath -> IO (Diagram SVG V2 Double)
> d file = do
>     f <- readFile file
>     ...
>
> main = mainWith d

The program will expect a file name on the command-line which it reads to
generate a diagram.

::

    $ ./IO-diagram -o file.svg -w 400 ~/data.log

See the `Clock Example`_ section below.


Standard Options
================

The standard options for diagram creation are found in the
`Diagrams.Backend.CmdLine`:mod: module of `diagrams-lib`:pkg: and are
represented with the following record:

.. class:: lhs

::

> data DiagramOpts = DiagramOpts
>   { _width     :: Maybe Int
>   , _height    :: Maybe Int
>   , _output    :: FilePath
>   }

This models having an optional width and height and a required `FilePath`.
We also need to have a parser for command-line arguments that results in a
value of this record.  We use the `optparse-applicative`:pkg: package for
command-line parsing and the parser for the standard options is the following:

.. class:: lhs

::

> diagramOpts :: Parser DiagramOpts
> diagramOpts = DiagramOpts
>     <$> (optional . option auto)
>         ( long "width" <> short 'w'
>        <> metavar "WIDTH"
>        <> help "Desired WIDTH of the output image")
>     <*> (optional . option auto)
>         ( long "height" <> short 'h'
>        <> metavar "HEIGHT"
>        <> help "Desired HEIGHT of the output image")
>     <*> strOption
>         ( long "output" <> short 'o'
>        <> value ""
>        <> metavar "OUTPUT"
>        <> help "OUTPUT file")


This is written in applicative form, `Constructor <$> ... <*> ... <*> ...`,
where the values we fill in are the parsers for the fields of the record.  The
parsers in `optparse-applicative`:pkg: take as an argument a collection of
parameters.

Abstracting Main
================

This section walks through and motivates the design of the abstraction
mechanisms that make possible the examples shown above.  If you want
to create your own custom command-line-driven diagram generation
executables, you will likely find it helpful to understand this
section.  The truly impatient, however, may wish to skip directly to
`User Extensions`_ and return to this section as necessary.

What work does the backend need to do to render a diagram?  It depends
on the backend, of course, but there are several common tasks given
the standard options.  To start with we need to parse the command-line
arguments.  The `optparse-applicative`:pkg: package provides all the
tools we need for this.  Next we will need to translate the standard
arguments into something backend specific.  Typically the extension on
the output filename will drive the format of the output and some
combination of the supplied width and height will dictate the final
scale of the diagram.  Let's look at a full example of a backend doing
this work and try to see what parts we can abstract out for general
use (we will use the `Cairo` backend for this example).

.. class:: lhs

::

> defaultMain :: Diagram Cairo V2 Double -> IO ()
> defaultMain d = do
>   prog <- getProgName
>   let p = info (helper' <*> diagramOpts)
>               ( fullDesc
>              <> progDesc "Command-line diagram generation."
>              <> header prog)
>   opts <- execParser p
>   chooseRender opts d
>
> chooseRender :: DiagramOpts -> Diagram Cairo V2 Double -> IO ()
> chooseRender opts d =
>   case splitOn "." (output opts) of
>     [""] -> putStrLn "No output file given."
>     ps | last ps `elem` ["png", "ps", "pdf", "svg"] -> do
>            let outTy = case last ps of
>                  "png" -> PNG
>                  "ps"  -> PS
>                  "pdf" -> PDF
>                  "svg" -> SVG
>                  _     -> PDF
>            fst $ renderDia
>                    Cairo
>                    ( CairoOptions
>                      (output opts)
>                      (mkSizeSpec
>                        (fromIntegral <$> width opts)
>                        (fromIntegral <$> height opts)
>                      )
>                      outTy
>                      False
>                    )
>                    d
>        | otherwise -> putStrLn $ "Unknown file type: " ++ last ps

There are several things that make this structuring of the program inflexible.
Let's consider building a `main` where we accept a function that can produce a
diagram.

.. class:: lhs

::

> functionMain :: (a -> Diagram Cairo V2 Double) -> IO ()

Clearly we cannot use the given function as we have no way to produce an `a`.
So we provide a type class called `Parseable` for associating a parser with the
type that it parses:

.. class:: lhs

::

> class Parseable a where
>    parser :: Parser a

Now we can make more progress.

.. class:: lhs

::

> functionMain :: Parseable a => (a -> Diagram Cairo V2 Double) -> IO ()
> functionMain f = do
>   prog <- getProgName
>   let p = info (helper' <*> ((,) <$> diagramOpts <*> parser))
>               ( fullDesc
>              <> progDesc "Command-line diagram generation."
>              <> header prog)
>   (opts,a) <- execParser p
>   chooseRender opts (f a)

The only parts so far that are backend specific are the type of the final
diagram and `chooseRender`, though we may want other parts may be subject to
customization.  We will split this into four parts, the type of the options
needed, the action of parsing the command-line, the backend specific rendering,
and an entry point for the library consumer.  We will give this the brilliant
name `Mainable`.

.. class:: lhs

::

> class Mainable d where
>    type MainOpts d :: *
>
>    mainArgs   :: Parseable (MainOpts d) => d -> IO (MainOpts d)
>    mainRender :: MainOpts d -> d -> IO ()
>    mainWith   :: Parseable (MainOpts d) => d -> IO ()

There is one associated type and three class methods.  Let's consider the
instance of `Mainable` for a simple diagram with type `Diagram Cairo V2 Double`:

.. class:: lhs

::

> instance Mainable (Diagram Cairo V2 Double) where

The associated type indicates what options we will want to be parsed
from the command-line.  In this case we will just use the standard
options:

.. class:: lhs

::

>     type MainOpts (Diagram Cairo V2 Double) = DiagramOpts

The `mainArgs` method is nearly what we had before.  In this case there isn't
anything backend specific, so instead of an instance implementation we will
show the default implementation for `mainArgs`.  Instead of a specific parser
`diagramOpts` we have a constraint `Parseable (MainOpts d)` allowing us to use
`parser` where we had `diagramsOpts`.  The parser from the constraint is combined with some
additional standard configuration for the program name and the right kind of
help message.  Running the `mainArgs` IO action results in either the program
quitting with a parse error or help message, or the program continuing with the
parsed value.  Also note that we need the diagram to be passed to `mainArgs`
only to fix the type so we can use our associated type function `MainOpts`.

.. class:: lhs

::

>     mainArgs :: Parseable (MainOpts d) => d -> IO (MainOpts d)
>     mainArgs _ = do
>       prog <- getProgName
>       let p = info (helper' <*> parser)
>                   ( fullDesc
>                  <> progDesc "Command-line diagram generation."
>                  <> header prog)
>       execParser p

The next method to implement is the `mainRender` method.  Here we can just use
the `chooseRender` function we had before, handling all the backend specific
interpretation of the standard arguments.

.. class:: lhs

::

>     mainRender :: DiagramOpts -> Diagram Cairo V2 Double -> IO ()
>     mainRender = chooseRender

Finally we have `mainWith` which joins the previous parts to make an entry point
for users of the backend to build their programs.  In this example we take as an
argument the `Diagram Cairo V2 Double` and result in a complete program.  Again, we can
get away with the default implementation.

.. class:: lhs

::

>     mainWith :: Parseable (MainOpts d) => d -> IO ()
>     mainWith d = do
>         opts <- mainArgs d
>         mainRender opts d

Now let's try a much harder instance.  We want to be able to handle
functions whose final result has a `Mainable` instance, but require
some `Parseable` arguments first.  The tricky part of this instance is
that we need to know up front what *all* our arguments are going to be
in order to be able to parse them.  It sounds tempting to peel off one
argument at a time, parse, apply, and recurse with one less argument;
but this does not work.  To facilitate collecting the arguments, we
make a new type class that has associated types for all the
arguments of the type and the final result of the type.  It will also
contain a function to perform the application of all the arguments and
give the final result.

.. class:: lhs

::

> class ToResult d where
>     type Args d :: *
>     type ResultOf d :: *
>
>     toResult :: d -> Args d -> ResultOf d

We will need a base case for when we have reached the final result.  It needs
no arguments so we use the unit type for `Args` and the final result is just
the diagram itself.

.. class:: lhs

::

> instance ToResult (Diagram b v n) where
>     type Args (Diagram b v n) = ()
>     type ResultOf (Diagram b v n) = Diagram b v n
>
>     toResult d _ = d

Now we can write the inductive case of a function resulting in something with
a `ToResult` instance.

.. class:: lhs

::

> instance ToResult d => ToResult (a -> d) where
>     type Args (a -> d) = (a, Args d)
>     type ResultOf (a -> d) = ResultOf d
>
>     toResult f (a,args) = toResult (f a) args

Here `Args` is the product of the argument and any arguments that `d` demands.
The final result is the final result of `d` and to produce a result we apply
one argument and recurse to `d`'s `ToResult` instance.

Now that we have `ToResult` to work with, we can write the type for the instance
of `Mainable` that we want:

.. class:: lhs

::

> instance (Parseable a, Parseable (Args d), ToResult d, Mainable (ResultOf d))
>         => Mainable (a -> d) where

.. container:: exercises

  #. Think about this type for a bit.

Now we need a type for `MainOpts (a -> d)` and at least an implementation for
`mainRender`.  Remember the purpose of `MainOpts` is to give a type for all
the arguments needed.  We will need the `MainOpts` from the final result and
some structure containing all the function arguments.  Note that we rely on
having a `Parseable` instance for products.

.. class:: lhs

::

>     type MainOpts (a -> d) = (MainOpts (ResultOf (a -> d)), Args (a -> d))

Our `mainRender` will be handed a value of this type and a function of our
instance type.  We can use `toResult` to apply the second part of the pair
to the function and hand the final result off to the final result's `Mainable`
instance along with its required options.

.. class:: lhs

::

>     mainRender (opts, a) f = mainRender opts (toResult f a)

Now we compile and cross our fingers!

We can also handle IO with a couple more instances.  First we will need a
`ToResult` instance that handles IO actions:

.. class:: lhs

::

> instance ToResult d => ToResult (IO d) where
>    type Args (IO d) = Args d
>    type ResultOf (IO d) = IO (ResultOf d)
>
>    toResult d args = flip toResult args <$> d

This states that the needed arguments are not affected by this being
an IO action and the final result is an IO action producing the final
result of the action's result type.  Our `Mainable` instance can now
be written:

.. class:: lhs

::

> instance Mainable d => Mainable (IO d) where
>     type MainOpts (IO d) = MainOpts d
>
>     mainRender opts dio = dio >>= mainRender opts

Here we merely perform the diagram creating action and bind its value
to the `Mainable` instance that can handle it.  For an example of using
these instances see the `Clock Example`_ section below.


User Extensions
===============

You can easily build on top of this framework to create executables
taking your own custom command-line flags.  This section walks through
a simple example.

Suppose we want to make "flippable" diagrams: a single executable that
can render either a diagram or its mirror image, depending on a
command-line flag.  Of course we also want to support all the usual
command-line options like ``--width``, ``--height``, ``--output``, and
so on.  The framework described above---together with the
composability of `optparse-applicative`:pkg:\-based command-line
parsers---makes this very easy to do.

First, some pragmas and imports:

.. class:: lhs

::

> {-# LANGUAGE FlexibleInstances         #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}
> {-# LANGUAGE TypeFamilies              #-}
>
> import           Diagrams.Backend.CmdLine
> import           Diagrams.Backend.SVG.CmdLine
> import           Diagrams.Prelude             hiding ((<>))
> import           Options.Applicative

(Unfortunately, `Options.Applicative`:mod: re-exports the `(<>)` from
``Data.Monoid``, whereas `Diagrams.Prelude`:mod: re-exports the one
from ``Data.Semigroup``.)

We now create a newtype for "flippable" things:

.. class:: lhs

::

> newtype Flippable a = Flippable a

We need a newtype since we need to make a `Mainable` instance which is
different than the default instance for `Diagram SVG V2 Double`.

We create a data structure to contain our new command-line options,
along with a `Parseable` instance for it.  In this case we just want a
single `Bool` value, corresponding to a new command-line switch
``--flipped`` along with an appropriate help message

.. class:: lhs

::

> data FlipOpts = FlipOpts Bool
>
> instance Parseable FlipOpts where
>   parser = FlipOpts <$> switch (long "flipped" <> help "Flip the diagram L-R")

For help on constructing command-line parsers, see the documentation
for the `optparse-applicative`:pkg: package; you can also look at the
source code of `Diagrams.Backend.CmdLine`:mod: for some examples.

Finally, we create a `Mainable` instance for flippable diagrams.  The
`MainOpts` for flippable diagrams consists of a pair of our new
`FlipOpts` along with the `MainOpts` for diagrams.  To implement
`mainRender`, we take in our options and a flippable diagram, and pass
the diagram-specific options along to the `mainRender` method for
diagrams, flipping the diagram appropriately.

.. class:: lhs

::

> instance Mainable (Flippable (Diagram SVG V2 Double)) where
>   type MainOpts (Flippable (Diagram SVG V2 Double)) = (MainOpts (Diagram SVG V2 Double), FlipOpts)
>
>   mainRender (opts, FlipOpts f) (Flippable d) = mainRender opts ((if f then reflectX else id) d)

Let's try it out!

.. class:: lhs

::

> d :: Diagram SVG V2 Double
> d = square 1 # fc red ||| square 1 # fc blue
>
> main = mainWith (Flippable d)

Note the ``--flipped`` option in the help message:

::

  $ ./Flippable --help

  Flippable

  Usage: Flippable [-w|--width WIDTH] [-h|--height HEIGHT] [-o|--output OUTPUT] [-l|--loop] [-s|--src ARG] [-i|--interval INTERVAL] [--flipped]
    Command-line diagram generation.

  Available options:
    -?,--help                Show this help text
    -w,--width WIDTH         Desired WIDTH of the output image (default 400)
    -h,--height HEIGHT       Desired HEIGHT of the output image (default 400)
    -o,--output OUTPUT       OUTPUT file
    -l,--loop                Run in a self-recompiling loop
    -s,--src ARG             Source file to watch
    -i,--interval INTERVAL   When running in a loop, check for changes every INTERVAL seconds.
    --flipped                Flip the diagram L-R

And running it yields:

::

    $ ./Flippable -o Flippable.svg -w 400

.. class:: dia

::

> example = square 1 # fc red ||| square 1 # fc blue

::

    $ ./Flippable -o Flippable.svg -w 400 --flipped

.. class:: dia

::

> example = square 1 # fc blue ||| square 1 # fc red

It works!

It is also worth noting that for this simple example, *we actually did
not need the* `Flippable` *wrapper or* `Mainable` *instance at all*!
Given only the `FlipOpts` type and its `Parseable` instance, we can
simply write

.. class:: lhs

::

> main = mainWith (\(FlipOpts f) -> (if f then reflectX else id) d)

which gives us *exactly the same program*!  Indeed, if you squint at
the function instance for `Mainable` and the instance we wrote for
`Flippable`, you can see that our instance is a direct specialization
of the more general one.

Clock Example
=============

We may want to build diagrams based on the state of the world.  For
instance, if we want to build a diagram of a clock we will want to
know what time it is.  Consider the following program.

.. class:: lhs

::

> import Diagrams.Prelude
> import Diagrams.Coordinates
> import Data.Time
>
> clock :: UTCTime -> Diagram B
> clock t = circle 0.35 # fc silver # lwG 0
>        <> bigHand # f 12 h <> littleHand # f 60 m
>        <> circle 1  # fc black # lwG 0
>        <> circle 11 # lwG 1.5 # lc slategray # fc lightsteelblue
>   where
>     s = realToFrac $ utctDayTime t :: Double
>     m = s / 60
>     h = m / 60
>
>     bigHand    = (0 ^& (-1.5)) ~~ (0 ^& 7.5) # lwG 0.5
>     littleHand = (0 ^& (-2))   ~~ (0 ^& 9.5) # lwG 0.2
>     f n v = rotate (- v / n @@ turn)
>
> main = mainWith (clock <$> getCurrentTime)

Running we get:

.. class:: dia

::

> import Diagrams.Prelude
> import Diagrams.Coordinates
> import Data.Time
>
> clock :: UTCTime -> Diagram B
> clock t = circle 0.35 # fc silver # lwG 0
>        <> bigHand # f 12 h <> littleHand # f 60 m
>        <> circle 1  # fc black # lwG 0
>        <> circle 11 # lwG 1.5 # lc slategray # fc lightsteelblue
>   where
>     s = realToFrac $ utctDayTime t :: Double
>     m = s / 60
>     h = m / 60
>
>     bigHand    = (0 ^& (-1.5)) ~~ (0 ^& 7.5) # lwG 0.5
>     littleHand = (0 ^& (-2))   ~~ (0 ^& 9.5) # lwG 0.2
>     f n v = rotate (- v / n @@ turn)
>
> example = clock $ read "2013-11-19 03:14:15.926535 UTC"

This uses the `Mainable d => Mainable (IO d)` instance to allow our
effectful clock generator.  However, we could have just as well avoided
this instance, writing instead:

.. class:: lhs

::

> main = do
>     t <- getCurrentTime
>     mainWith (clock t)

This instance is quite convenient, however, especially when we want our IO action
to depend on some command-line option.  The following exercises should be helpful
in gaining practice working with IO and options by modifying the clock example into
a useful clock making program.

.. container:: exercises

   #. Modify the example so it can take a time as a command-line
      option, but if one is not given, it uses the current time.

   #. Modify `clock` to take a `ClockStyle` argument that includes options for
      various visual styles for the clock.  For instance `ClockStyle` could
      include a color for the clock background, a flag for turning on hour
      marks, or a flag for including a second hand.
