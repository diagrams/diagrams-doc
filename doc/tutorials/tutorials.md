---
title: How to write diagrams tutorials
...

Know how to explain a certain technique? Have a cool diagram which makes
for a good, illustrative example but is too long to go in the gallery?
Write a tutorial!

Getting started
===============

Tutorials on the diagrams website (and the user manual) are written
using the [reStructuredText
format](http://docutils.sourceforge.net/rst.html). Though it is a large
and complex format, there's not a lot you need to know in order to write
a tutorial.

The most important advice is to take a look at the source for the user
manual and other tutorials for examples, and copy and paste one to get
started (for example, there is some routine boilerplate that must go at
the top). Tutorials go in the diagrams-doc repository, in the `doc`
subdirectory. For example, the user manual is `doc/manual.md`; the
source for this tutorial is `doc/tutorials.md`. Copy one of those into
`doc/your-tutorial-name.md`, and off you go! You also need a title
metadata, containing the line `title: Your Title Here`. To get your
tutorial linked on the main diagrams web page edit the file
`web/documentation.md` by adding a line to the *Tutorials* section that
looks like

`* [Name of your tutorial] (/doc/your-tutorial-name.html) --- description`

Basic formatting
================

In addition to the below notes, you can refer to the [quick
reference](http://johnmacfarlane.net/pandoc/README.html) for more
details.

-   *Italics* can be produced with `*asterisks*`.
-   Make bulleted lists by preceding the items with `*`.
-   Everything can be nested; you simply have to be careful with
    indentation (as in Haskell, indentation is used to determine the
    extent of each scope).
-   The format for hyperlinks is a bit strange; see the user manual for
    examples, or refer to the [quick
    reference](http://docutils.sourceforge.net/docs/user/rst/quickref.html).
-   Headings levels are indicated by underlining heading titles, with a
    different character for each level. From top (biggest) to bottom
    (smallest), the heading characters are `=`, `-`, `~`, and `+`. The
    overall title of the tutorial should not be included in the document
    itself, but in the associated `.html.metadata` file (for rendering
    by hakyll).

Diagrams-specific formatting
============================

The diagrams documentation toolchain is set up to recognize and handle
some special markup. Inline markup is accomplished by surrounding some
text in curly brackets, followed by a tag with a prepending dot, like
`` `some text here`{.tag} ``.

-   Haskell expressions should be enclosed in single backquotes. Such
    expressions will be linked to Haddock documentation as appropriate.
    For example, `` `circle 1 ``\` is typeset like this: `circle 1`.
-   For other literal expressions which should be typeset using a
    monospaced font, use double backquotes, like ``foo``.
-   For embedded Latex like $2 + \sqrt{\pi}$, use a surrounding `$`,
    as in `$2 + \sqrt{\pi}$. Math is typeset using
    [MathJax](http://www.mathjax.org/).
-   For names of packages on Hackage, use the `pkg` tag, as in ``
    `diagrams-lib`{.pkg} ``. Package names tagged with `pkg` will
    automatically turn into Hackage links, like this:
    `diagrams-lib`{.pkg}.
-   For names of diagrams-related modules, use the `mod` tag, as in ``
    `Diagrams.Path`{.mod} ``. This will turn into a link to the Haddock
    documentation *hosted on the diagrams website* (not Hackage), like
    this: `Diagrams.Path`{.mod}. Hence, this should only be used for
    modules which are hosted on the diagrams website.
-   To link to a repository in the `diagrams` github organization, use
    the `repo` tag. For example, `` `diagrams-lib`{.repo} `` turns into
    diagrams-lib.

Special containers
==================

Currently, there are three special types of "containers" defined, which
are introduced with the syntax

```
<div class=type>
Content
</div>
```

In place of `<type>` you can put `todo`, `warning`, or `exercises`,
which produce boxes like the following.

`todo`:

<div class="todo">

Some stuff to do. This tutorial is not finished yet!

</div>

`warning`:

<div class="warning">

Careful! The first three errors you encounter will be due to the fact
that you do not understand this warning!

</div>

`exercises`:

<div class="exercises">

1.  Do 100 jumping jacks.
2.  Now do it again, this time using diagrams.
3.  Generalize this result.

</div>

Code blocks
===========

Literal blocks are indicated by a double colon on a line by itself,
followed by a blank line, followed by an indented block, optionally
using bird tracks (blocks using bird tracks do not have to be indented).
Such blocks will be typeset using a monospaced font.

To assign a "class" to a literal block, prefix the double colon by an
additional blank line and the syntax `.. class:: foo`. For example, a
literal block with the class `lhs` could be specified as follows:

Code blocks are indicated by three or more `\``:

````
``` haskell
x = 13
z = square 2 <> circle 3
```
````

which produces:

``` haskell
x = 13
z = square 2 <> circle 3
```

As you can see, the `haskell` class produces syntax-highlighted and
hyperlinked Haskell source. To include an inline diagram, use the
`diagram` class and define a diagram named `example`. For example:

````
``` diagram

myCircle = circle 1 # fc purple
example = hcat (replicate 4 myCircle)
```
````

produces

``` diagram
myCircle = circle 1 # fc purple
example = hcat (replicate 4 myCircle)
```

When writing `diagram` blocks you can add extra imports and `LANGUAGE`
pragmas as necessary. However, you should not explicitly import
Diagrams.Prelude or any diagrams backend. These imports are added
automatically when the documentation is built, depending on what backend
is in use.

Finally, using `diagram-code` will produce a diagram *and* typeset the code
below it. For example, changing `diagram` to `diagram-code` above produces

``` diagram-code

myCircle = circle 1 # fc purple
example = hcat (replicate 4 myCircle)
```

Static images
=============

![image](static/phone.png)

If you have some static images (that is, images which are not generated
by diagrams code, but are to be stored explicitly in the diagrams-doc
repository), you can put them in the `doc/static` directory, and
reference them in several ways. To include a static image in a tutorial,
as above, do something like

```
![image](static/phone.png)
```

(this is the code used for the image above); note the leading slash. To
reference an image from a diagram, do something like

````
``` diagram

no = (circle 1 <> hrule 2 # rotateBy (1/8))
   # lwG 0.2 # lc red
example = no <> image (uncheckedImageRef "doc/static/phone.png" 470 377)
```
````

(note the *lack* of a leading slash), which produces

``` diagram

no = (circle 1 <> hrule 2 # rotateBy (1/8))
   # lwG 0.2 # lc red
example = no <> image (uncheckedImageRef "doc/static/phone.png" 470 377)
```

Building
========

It's ideal to continuously rebuild the diagrams website locally while
you're writing, so you can see what the final product looks like. The
website takes a bit of effort to build initially, but once you have it
built, incremental rebuilds are very fast (thanks to shake and hakyll),
making for a tight edit-compile-view feedback loop. For instructions on
how to build the diagrams website, see the [diagrams-doc
README](https://github.com/diagrams/diagrams-doc/blob/master/README.md).
