Introduction
============

Diagrams can be used to easily include pictures in your $\LaTeX$
documents, using either the `diagrams-cairo`{.pkg} or
`diagrams-postscript`{.pkg} backend (and, soon, the `diagrams-pgf`
backend). You directly embed diagrams code in your $\LaTeX$ source,
within special `\begin{diagram} ... \end{diagram}` blocks; the
`diagrams-latex` package then takes care of automatically running your
diagrams code and including the generated images in the output, so you
don't need to worry about maintaining a bunch of source files or running
special preprocessing tools.

Installation
============

First, you will need the `diagrams-builder`{.pkg} package, which
`diagrams-latex` depends on. `diagrams-builder`{.pkg} does not come
automatically as part of a basic diagrams installation, so you will need
to install it separately. You can install it with either of the commands

    cabal install -fps diagrams-builder

*or*

    cabal install -fcairo diagrams-builder

Use the flag (`-fps` or `-fcairo`) corresponding to the backend you
would like to use with `diagrams-latex`. If you wish, you can use both.

These flags (`-fps` and `-fcairo`) cause executables to be installed
(`diagrams-builder-ps` and `diagrams-builder-cairo`, respectively); make
sure the appropriate executable is installed and accessible via your
path.

Finally, download
[diagramsllatexssty](https://github.com/diagrams/diagrams-builder/blob/master/latex/diagrams-latex.sty)
and put it somewhere $\LaTeX$ can find it. One such place is in the same
directory as your `.tex` file. If you plan to use `diagrams-latex`
often, you may prefer putting it in a more global location; if you don't
know how to do that, a good place to start is [this StackExchange
question](http://tex.stackexchange.com/questions/1137/where-do-i-place-my-own-sty-files-to-make-them-available-to-all-my-tex-files).

`diagrams-latex` package options
================================

The first step is to include the `diagrams-latex` package. In the
preamble of your $\LaTeX$ file, add

    \usepackage{diagrams-latex}
    \usepackage{graphicx}

Note inclusion of the `graphicx` package is currently required. You can
also pass some options to the `diagrams-latex` package:

-   `outputdir` specifies the name of directory where intermediate and
    final diagram files will be written. The directory will be created
    if it does not already exist. By default, intermediate files will be
    written in the same directory as the $\LaTeX$ document, which may
    not be desirable.
-   `backend` specifies the backend to be used. Possible values include
    `cairo` and `ps`. The default is `cairo`.
-   `extension` specifies the file extension which should be used for
    compiled diagrams. The default is `pdf`. Be sure to use an extension
    which is compatible with the chosen backend; forexample, if you
    specify `backend=ps` you likely want to also specify
    `extension=eps`.

As a complete example, to specify diagrams compiled with the
`postscript` backend, using the `diagrams` directory, one would write

    \usepackage[backend=ps, extension=eps, outputdir=diagrams]{diagrams-latex}
    \usepackage{graphicx}

Diagram blocks
==============

To include a diagram in your document, create a `diagram` block, like
so:

    \begin{diagram}[width=300,height=200]
      {-# LANGUAGE FlexibleContexts #-}
      import Data.List

      foo = circle 1 # fc green
      dia = foo ||| foo
    \end{diagram}

The `width` and `height` arguments work as you would expect; they are
both optional. Within the block, you can place an arbitrary Haskell
module (including imports and `LANGUAGE` pragmas, as illustrated above).
Note that the indentation does not matter, as long as it is consistent.
Imports of `Diagrams.Prelude`{.mod} and whichever backend you are using
will be added automatically. The module must contain a definition of a
diagram named `dia`{.hs}, which will be rendered. It may also contain
any other arbitrary helper definitions.

Running
=======

To compile your en-diagrammed $\LaTeX$ document, simply run `latex`,
`pdflatex`, *etc.* as usual. However, you **must also pass the**
`--enable-write18` **command-line flag**. For example,

    pdflatex --enable-write18 mydocument.tex

This is because in order to compile the embedded diagrams, $\LaTeX$ must
call out to the `diagrams-builder` executable via the shell, and this
requires special permission.

Tips and tricks
===============

Each of the `diagram` blocks is a completely independent scope. If you
have definitions that should be shared among multiple diagrams (as is
common), simply put the definitions in a separate Haskell module in the
same directory, and import this module in each `diagram` block.
`diagrams-builder`{.pkg} is smart enough to recompile a diagram when its
imports have changed. In general, `diagrams-builder`{.pkg} tries hard to
avoid recompiling diagrams when recompilation is not necessary, in order
to speed build times.

Using `diagrams-latex` with Beamer
==================================

`diagrams-latex` works well in conjunction with `beamer` for making
slide decks with embedded images. However:

<div class="warning">

Every `frame` containing a diagram *must* be marked as `[fragile]`!

</div>

Non-`[fragile]` frames with diagrams in them cause $\TeX$ to choke with
horrendous, inscrutable errors. If you are using `pandoc` to generate
slides, you can "trick" `pandoc` into emitting `[fragile]` annotations
by inserting an empty code block into each slide with a diagram.

See [this Stack Exchange
answer](<http://tex.stackexchange.com/questions/11328/beamers-fragile-frame-as-default>)
explaining how to define an alternate `frame` environment which is
fragile by default. It boils down to something like

    \newenvironment{xframe}[1][]
      {\begin{frame}[fragile,environment=xframe,#1]}
      {\end{frame}}

and then using `xframe` instead of `frame` (it's probably possible to
replace `frame` entirely with some clever renaming, but I haven't
figured out how to do it).