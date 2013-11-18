.. role:: pkg(literal)
.. role:: hs(literal)
.. role:: mod(literal)
.. role:: repo(literal)

.. default-role:: hs
.. sectnum:: :depth: 2

===============================
 Command-line Diagram Creation
===============================

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

To give a concrete example of what we will see, the following are examples
of programs we will be able to write and their interaction on the command-line.
First is the simplest case of generating a single diagram:

.. class:: lhs

::

> -- Simple
>
> d :: Diagram SVG R2
> d = ...
>
> main = mainWith d

Here we just have a diagram and the standard options.  We can invoke
with just a width and the height is made to match the diagram.

.. code-block:: sh

    $ ./Simple -o simple.svg -w 100

If we have multiple diagrams with names we can use `mainWith` to give an
interface that allows the selection of a particular diagram by name.

.. class:: lhs

::

> -- Multiple
>
> d1, d2, d3 :: Diagram SVG R2
> ...
>
> main = mainWith [("First", d1),("Second", d2),("Third", d3)]

The ``--list`` option just lists the available diagrams to render and
the ``-s`` selection option takes a name and renders the associated
diagram with the standard options.

.. code-block:: sh

    $ ./Multiple --list
    Available diagrams:
      one two three
    $ ./Multiple -o d1.svg -w 100 -s First

Some backends support rendering animations (typically as individually indexed
files of frames).

.. class:: lhs

::

> -- Animation
>
> a :: Animation Cairo R2
> a = ...
>
> main = mainWith a

The default options are expected, but the output file has an index appended
to the name for each frame.  The ``--fpu`` option indicates the desired number
of frames per unit of time.  If the ``a`` animation above is one second long
then the following will create files ``a01.png`` through ``a24.png``.

.. code-block:: sh

   $ ./Animation -o a.png -w 100 --fpu 24

In backends that support multiple pages we can list all the diagrams and 
have each render on its own page.

.. class:: lhs

::

> -- Pages
>
> d1, d2, d3 :: Diagram Postscript R2
> ...
>
> main = mainWith [d1,d2,d3]

We only need the default options here and the interface is the same as a
single diagram.

.. code-block:: sh

    $ ./Pages -o pages.ps -w 400

To make things more interesting we could require additional arguments to
build a diagram.  We can take a function to build a diagram from some
parameters and build an interface that fills those parameters with 
arguments from the command-line.

.. class:: lhs

::

> -- Function
>
> f :: Colour Double -> Double -> Diagram SVG R2
> f c x = ...
>
> main = mainWith f

In addition to the standard arguments we have ``blue`` and ``42.0`` which
will be provided as arguments to ``f``.

.. code-block:: sh

    $ ./Function -o blue.svg -w 400 blue 42.0


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
>     <$> (optional . option)
>         ( long "width" <> short 'w'
>        <> value 400
>        <> metavar "WIDTH"
>        <> help "Desired WIDTH of the output image (default 400)")
>     <*> (optional . option)
>         ( long "height" <> short 'h'
>        <> value 400
>        <> metavar "HEIGHT"
>        <> help "Desired HEIGHT of the output image (default 400)")
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

.. container:: todo

  Add a short paragraph here giving an overview of what this section
  is about.  Below, when introducing each type class and so on,
  link to the module containing the definition.

What work does the backend need to do to render a diagram?  It depends on the
backend but there are several common tasks given the standard options.  To
start with we need to parse the command-line arguments.  The `optparse-applicative`:pkg:
package provides all the tools we need for this.  Next we will need to translate the
standard arguments into something backend specific.  Typically the extension
on the output filename will drive the format of the output and some combination
of the supplied width and height will dictate the final scale of the diagram.
Let's look at a full example of a backend doing this work and try to see what
parts we can abstract out for general use (we will use the `Cairo` backend
for this example).

.. class:: lhs

::

> defaultMain :: Diagram Cairo R2 -> IO ()
> defaultMain d = do
>   prog <- getProgName
>   let p = info (helper' <*> diagramOpts)
>               ( fullDesc
>              <> progDesc "Command-line diagram generation."
>              <> header prog)
>   opts <- execParser p
>   chooseRender opts d
>
> chooseRender :: DiagramOpts -> Diagram Cairo R2 -> IO ()
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

> functionMain :: (a -> Diagram Cairo R2) -> IO ()

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

> functionMain :: Parseable a => (a -> Diagram Cairo R2) -> IO ()
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
instance of `Mainable` for a simple diagram with type `Diagram Cairo R2`:

.. class:: lhs

::

> instance Mainable (Diagram Cairo R2) where

The associated type indicates what options we will want to be parsed
from the command-line.  In this case we will just use the standard
options:

.. class:: lhs

::

>     type MainOpts (Diagram Cairo R2) = DiagramOpts

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

>     mainRender :: DiagramOpts -> Diagram Cairo R2 -> IO ()
>     mainRender = chooseRender

Finally we have `mainWith` which joins the previous parts to make an entry point
for users of the backend to build their programs.  In this example we take as an
argument the `Diagram Cairo R2` and result in a complete program.  Again, we can
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

> instance ToResult (Diagram b v) where
>     type Args (Diagram b v) = ()
>     type ResultOf (Diagram b v) = Diagram b v
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

User Extensions
===============

You can easily build on top of this framework to create your own
executables taking your own custom command-line flags.  This section
walks through a simple example.  Although unrealistic, it should
provide you with a template for more realistic extensions.

.. container:: todo

  Consider a newtype wrapper `newtype Flippable a = Flippable a`.
  Write an instance for `Mainable (Flippable (Diagram Cairo R2))` that
  takes an an additional flag ``--flip`` which specifies that the
  diagram should be flipped horizontally (or drawn normally if the
  ``--flip`` flag is not given).
