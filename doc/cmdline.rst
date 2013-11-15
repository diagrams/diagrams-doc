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
will be applied to ``f``.

.. code-block:: sh

    $ ./Function -o pages.ps -w 400 blue 42.0


Standard Options
================

The standard options for diagram creation are found in the
`Diagrams.Backend.CmdLine`:mod: of the `diagrams-lib`:pkg: and are represented
with the following record:

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
parameters.  We provide a type class called `Parseable` for associating a
parser with the type that it parses:

.. class:: lhs

::

> class Parseable a where
>    parser :: Parser a

And a type class `Mainable` for associating a type with a command-line
behavior:

.. class:: lhs

::

> class Mainable d where
>    type MainOpts d :: *
>
>    mainArgs   :: (Parseable a, Parseable (MainOpts d)) 
>               => d -> IO (MainOpts d, a)
>    mainRender :: MainOpts d -> d -> IO ()
>    mainWith   :: Parseable (MainOpts d) => d -> IO ()

There is one associated type and three class methods.  Lets consider the
instance of `Mainable` for a simple diagram with type `Diagram SVG R2`:

.. class:: lhs

::

> instance Mainable (Diagram SVG R2) where

The associated type indicates what options we will want to be parsed
from the command-line.  In this case we will just use the standard
options:

.. class:: lhs

::

>     type MainOpts (Diagram SVG R2) = DiagramOpts

Now we need to actually parse the arguments.  The `mainArgs` method
has a default implementation that covers our use here.  Specifically
it looks for `Parseable` instances for the associated type (`Parseable (MainOpts d)`)
and for some other value (`Parseable a`) and pairs the two together
and runs that parser with some additional standard configuration for 
the program name and kind of help message in `defaultOpts`.  Running
the `mainArgs` IO action results in either the program quiting with
a parse error or help message, or the program continuing with the
parsed value for the associated type and any additional command-line
options parsed to the value of type `a`.  It may become clearer later
why we want this additional value.  Also note that we need the 
diagram to be passed to `mainArgs` only to fix the type so we can
use our associated type function `MainOpts`.

.. class:: lhs

::

>     mainArgs :: (Parseable a, Parseable (MainOpts d))
>              => d -> IO (MainOpts d, a)
>     mainArgs _ = defaultOpts ((,) <$> parser <*> parser)

The next method to implement is the `mainRender` method.  This
method takes some already parsed options and a diagram and does
the work of rendering the diagram to the specified file.  This
is where the backend specific work will happen.  In our case we
have a function `chooseRender` that will do all this work for
us given a diagram and the standard options:

.. class:: lhs

::

> mainRender :: MainOpts d -> d -> IO ()
> mainRender opts d = chooseRender opts d
