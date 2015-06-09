.. role:: pkg(literal)
.. role:: hs(literal)
.. role:: mod(literal)
.. role:: repo(literal)

.. default-role:: hs
.. sectnum:: :depth: 2

.. contents:: :depth: 2

Introduction
============

Diagrams can be used to easily include pictures in your `\LaTeX`:math:
documents, using the `diagrams-cairo`:pkg:,
`diagrams-postscript`:pkg:, or `diagrams-pgf`:pkg: backends.  You
directly embed diagrams code in your `\LaTeX`:math: source, within
special ``\begin{diagram} ... \end{diagram}`` blocks; the
``diagrams-latex`` package then takes care of automatically running
your diagrams code and including the generated images in the output,
so you don't need to worry about maintaining a bunch of source files
or running special preprocessing tools.

Installation
============

First, you will need the `diagrams-builder`:pkg: package, which
``diagrams-latex`` depends on.  `diagrams-builder`:pkg: does not come
automatically as part of a basic diagrams installation, so you will
need to install it separately.  You can install it with a command like

::

  cabal install -fps diagrams-builder

Instead of ``-fps`` you may specify whichever backend you plan to use
(``-fps``, ``-fcairo``, or ``-fpgf``).  If you wish, you may specify
more than one.

These flags (``-fps``, ``-fcairo``, or ``-fpgf``) cause executables to
be installed (``diagrams-builder-ps``, ``diagrams-builder-cairo``, and
``diagrams-builder-pgf`` respectively); make sure the appropriate
executable(s) are installed and accessible via your ``PATH``.

Finally, download diagrams-latex.sty__ and put it somewhere `\LaTeX`:math:
can find it. One such place is in the same directory as your ``.tex``
file. If you plan to use ``diagrams-latex`` often, you may prefer
putting it in a more global location; if you don't know how to do that,
a good place to start is `this StackExchange question`_.

__ https://github.com/diagrams/diagrams-builder/blob/master/latex/diagrams-latex.sty
.. _`this StackExchange question`: http://tex.stackexchange.com/questions/1137/where-do-i-place-my-own-sty-files-to-make-them-available-to-all-my-tex-files

``diagrams-latex`` package options
==================================

The first step is to include the ``diagrams-latex`` package.  In the
preamble of your `\LaTeX`:math: file, add

::

  \usepackage{diagrams-latex}
  \usepackage{graphicx}

Note inclusion of the ``graphicx`` package is currently required if
you are using the PostScript or Cairo backends.  You can also pass
some options to the ``diagrams-latex`` package:

* ``outputdir`` specifies the name of directory where intermediate and
  final diagram files will be written.  The directory will be created
  if it does not already exist.  By default, intermediate files will
  be written in the same directory as the `\LaTeX`:math: document, which may
  not be desirable.

* ``backend`` specifies the backend to be used.  Possible values
  include ``cairo``, ``ps``, and ``pgf``.  The default is ``cairo``.

* ``extension`` specifies the file extension which should be used for
  compiled diagrams.  The default is ``pdf``. Be sure to use an
  extension which is compatible with the chosen backend; forexample,
  if you specify ``backend=ps`` you likely want to also specify
  ``extension=eps``; for ``backend=pgf`` you likely want
  ``extension=pgf``.

* The ``input`` flag causes the generated diagrams to be included in
  the document via ``\input`` (instead of the default, which is
  ``\includegraphics``).  This option should be used with the PGF
  backend.

As a complete example, to specify diagrams compiled with the
``postscript`` backend, using the ``diagrams`` directory, one would
write

::

  \usepackage[backend=ps, extension=eps, outputdir=diagrams]{diagrams-latex}
  \usepackage{graphicx}

To use the ``pgf`` backend, one might write

::

  \usepackage[backend=pgf, extension=pgf, outputdir=diagrams, input]{diagrams-latex}

Diagram blocks
==============

To include a diagram in your document, create a ``diagram`` block,
like so:

::

  \begin{diagram}[width=300,height=200]
    {-# LANGUAGE FlexibleContexts #-}
    import Data.List

    foo = circle 1 # fc green
    dia = foo ||| foo
  \end{diagram}

The ``width`` and ``height`` arguments work as you would expect; they
are both optional.  Within the block, you can place an arbitrary
Haskell module (including imports and ``LANGUAGE`` pragmas, as
illustrated above).  Note that the indentation does not matter, as
long as it is consistent.  Imports of `Diagrams.Prelude`:mod: and whichever
backend you are using will be added automatically.  The module must
contain a definition of a diagram named `dia`, which will be
rendered.  It may also contain any other arbitrary helper definitions.

Running
=======

To compile your en-diagrammed `\LaTeX`:math: document, simply run ``latex``,
``pdflatex``, *etc.* as usual.  However, you **must also pass the**
``--enable-write18`` **command-line flag**.  For example,

::

  pdflatex --enable-write18 mydocument.tex

This is because in order to compile the embedded diagrams, `\LaTeX`:math: must
call out to the ``diagrams-builder`` executable via the shell, and
this requires special permission.

Tips and tricks
===============

Each of the ``diagram`` blocks is a completely independent scope.  If
you have definitions that should be shared among multiple diagrams (as
is common), simply put the definitions in a separate Haskell module in
the same directory, and import this module in each ``diagram`` block.
`diagrams-builder`:pkg: is smart enough to recompile a diagram when
its imports have changed.  In general, `diagrams-builder`:pkg: tries
hard to avoid recompiling diagrams when recompilation is not
necessary, in order to speed build times.

If you use the PGF backend, any `text` diagrams will be typeset by
`\LaTeX`:math:.  In particular, if your text contains any sections
surrounded by dollar signs, these will be typeset in math mode.  For
example,

::

  \begin{diagram}[width=300,height=200]
    dia = text "hi $\\sqrt{\\pi}$" # scale 0.5 <> circle 1 # fc yellow
  \end{diagram}

will produce a yellow circle with "hi `\sqrt{\pi}`:math:" in the
middle.

Using ``diagrams-latex`` with Beamer
====================================

``diagrams-latex`` works well in conjunction with ``beamer`` for
making slide decks with embedded images.  However:

.. container:: warning

  Every ``frame`` containing a diagram *must* be marked as ``[fragile]``!

Non-``[fragile]`` frames with diagrams in them cause `\TeX`:math: to
choke with horrendous, inscrutable errors.  If you are using
``pandoc`` to generate slides, you can "trick" ``pandoc`` into
emitting ``[fragile]`` annotations by inserting an empty code block
into each slide with a diagram.

See `this StackExchange answer`_
explaining how to define an alternate ``frame`` environment which is
fragile by default.  It boils down to something like

::

  \newenvironment{xframe}[1][]
    {\begin{frame}[fragile,environment=xframe,#1]}
    {\end{frame}}

and then using ``xframe`` instead of ``frame`` (it's probably possible
to replace ``frame`` entirely with some clever renaming, but I haven't
figured out how to do it).

.. _`this StackExchange answer`: http://tex.stackexchange.com/questions/11328/beamers-fragile-frame-as-default
