[![Build Status](https://secure.travis-ci.org/diagrams/diagrams-doc.png)](http://travis-ci.org/diagrams/diagrams-doc)

This repository holds all documentation for the
[diagrams framework](http://projects.haskell.org/diagrams), including

* the [website](http://projects.haskell.org/diagrams) source
* [tutorials](http://projects.haskell.org/diagrams/documentation.html)
* [user manual](http://projects.haskell.org/diagrams/doc/manual.html)
* IRC logs
* blog posts
* related papers
* release checklists

and more.

## Building

To build the website (which includes the gallery, user manual, and
tutorials), you will need:

* GHC 7.4 or 7.6
* the diagrams framework itself (including the
  [contrib package](http://github.com/diagrams/diagrams-contrib),
  [SVGFonts package](http://github.com/diagrams/SVGFonts), and
  [diagrams-builder package](http://github.com/diagrams/diagrams-builder)).
* *Either* the [cairo
  backend](http://github.com/diagrams/diagrams-cairo) (recommended)
  *or* the [SVG backend](http://github.com/diagrams/diagrams-svg) (if
  you cannot build the cairo backend).  See below for more
  information re: using the SVG backend.
* the python [docutils suite](http://docutils.sourceforge.net/) (in
  particular `rst2xml` should be on your PATH).
* the Haskell [docutils package](http://github.com/diagrams/docutils)
  (note it is not on Hackage; just clone the `master` branch from
  github and `cabal install` it).
* the latest version of [shake](http://hackage.haskell.org/package/shake).
* [hakyll](http://hackage.haskell.org/package/hakyll)-4.x.
* [safe](http://hackage.haskell.org/package/safe).

Once you have all the dependencies, simply do

    ghc --make Shake -threaded
    ./Shake +RTS -N4 -RTS preview

which will build the user manual and website, and run a web server on
port 8000 serving a preview of the webiste. In place of `-N4` you
should use `-NX` where X is the number of cores you have. By default,
the build system will use all but one of your available cores; if you
wish to set the number to something else, set the `DIA_DOC_THREADS`
environment variable.

Point your browser at `localhost:8000` to view it.  It works well to
leave this invocation of `Shake preview` running, and then start
another process calling `Shake +RTS -NX -RTS build` repeatedly in a
loop.  The website will now automatically be rebuilt any time any
source files change.

For example, on a four-core Ubuntu machine, assuming you already have
the Haskell Platform installed, and have cloned the relevant diagrams
repositories from github, and want to use the cairo backend, you could
issue the commands

```
sudo apt-get install libgtk-3-dev libcairo2-dev python-docutils
cabal install hsenv
hsenv --name=dia
.hsenv_dia/bin/activate
cabal install gtk2hs-buildtools
cabal install diagrams-core/ diagrams-lib/ diagrams-cairo/ diagrams-contrib/ diagrams-builder/ SVGFonts/ docutils/ shake hakyll safe
cd diagrams-doc
ghc --make Shake -threaded
./Shake +RTS -N4 -RTS preview
```

## Building with `diagrams-svg`

The build system will first check for an installed `diagrams-cairo`
package.  If none is found, it will fall back to using the
`diagrams-svg` package.  This almost works, except for a few small
issues:

* There are a few examples in the user manual and other tutorials
  which cannot build with `diagrams-svg`, because they use embedded
  images, which `diagrams-svg` cannot yet handle.  As a workaround,
  the build system passes a `--keepgoing` flag to the utility that
  builds diagrams embedded in the user manual and tutorials, causing
  it to report success even when an example fails.

* There is also an explicit list of gallery examples which are
  excluded from the build when using `diagrams-svg`, since they only
  build with `diagrams-cairo`.

These issues should not cause too much trouble but are simply good to
be aware of.  Using `diagrams-svg` should be perfectly adequate for
ensuring that your contributions to the documentation compile properly
and look the way you expect.
