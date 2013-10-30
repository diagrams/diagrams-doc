.. role:: pkg(literal)
.. role:: hs(literal)
.. role:: mod(literal)
.. role:: repo(literal)

.. default-role:: hs
.. sectnum:: :depth: 2

====================
 Diagrams and LaTeX
====================

.. contents:: :depth: 2

Introduction
============

Diagrams can be used to easily include pictures in your `\LaTeX`:math:
documents, using either the `diagrams-cairo`:pkg: or
`diagrams-postscript`:pkg: backend (and, soon, the ``diagrams-tikz``
backend).  You directly embed diagrams code in your `\LaTeX`:math:
source, within special ``\begin{diagram} ... \end{diagram}`` blocks;
the ``diagrams-latex`` package then takes care of automatically
running your diagrams code and including the generated images in the
output, so you don't need to worry about maintaining a bunch of source
files or running special preprocessing tools.

Installation
============

First, you will need the `diagrams-builder`:pkg: package, which
``diagrams-latex`` depends on.  `diagrams-builder`:pkg: does not come
automatically as part of a basic diagrams installation, so you will
need to install it separately.  You can install it with either of the
commands

::

> cabal install -fps diagrams-builder

*or*

::

> cabal install -fcairo diagrams-builder

Use the flag (``-fps`` or ``-fcairo``) corresponding to the backend
you would like to use with ``diagrams-latex``.  If you wish, you can
use both.

These flags (``-fps`` and ``-fcairo``) cause executables to be
installed (``diagrams-builder-postscript`` and
``diagrams-builder-cairo``, respectively); make sure the appropriate
executable is installed and accessible via your path.

Finally, download ``diagrams-latex.sty``__ and put it somewhere `\LaTeX`:math:
can find it. One such place is in the same directory as your ``.tex``
file. If you plan to use ``diagrams-latex`` often, you may prefer
putting it in a more global location; if you don't know how to do that
a good place to start is `this StackExchange question`_.

__ : https://github.com/diagrams/diagrams-builder/blob/master/latex/diagrams-latex.sty
_`this StackExchange question`: http://tex.stackexchange.com/questions/1137/where-do-i-place-my-own-sty-files-to-make-them-available-to-all-my-tex-files

Using ``diagrams-latex``
========================

The first step is to include the ``diagrams-latex`` package.  In the
preamble of your LaTeX file, add

::

> \usepackage{diagrams-latex}

You can also pass some options to ``diagrams-latex``:

* ``outputdir``

Include diagrams-latex package.  Options.

Make ``diagram`` block.  Options.

Run LaTeX with ``--enable-write18``.

Tips and tricks
===============

Tip: include common definitions in a ``.hs`` file in the same dir.
`diagrams-builder`:pkg: will notice when it changes.

