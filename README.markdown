[![Build Status](https://secure.travis-ci.org/diagrams/diagrams-doc.png)](http://travis-ci.org/diagrams/diagrams-doc)

This repository holds all documentation for the
[diagrams framework](http://projects.haskell.org/diagrams), including

* the [website](http://projects.haskell.org/diagrams) source
* [tutorials](http://projects.haskell.org/diagrams/documentation.html)
* [user manual](http://projects.haskell.org/diagrams/doc/manual.html)
* IRC logs
* blog posts
* related papers and talks
* release checklists

and more.

## Building

To build the website (which includes the gallery, user manual, and
tutorials), you will need:

* GHC 7.8
* the diagrams framework itself (including the
  [contrib package](http://github.com/diagrams/diagrams-contrib),
  [SVGFonts package](http://github.com/diagrams/SVGFonts),
  [palette package](http://github.com/diagrams/palette), and
  [diagrams-builder package](http://github.com/diagrams/diagrams-builder)).
* *Either* the [rasterific
  backend](http://github.com/diagrams/diagrams-rasterific) (recommended)
  *or* the [SVG backend](http://github.com/diagrams/diagrams-svg).  See below for more
  information re: using the SVG backend.
* the python [docutils suite](http://docutils.sourceforge.net/) (in
  particular `rst2xml.py` should be on your PATH).
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
the Haskell Platform installed and want to use the rasterific backend, you could
first clone the relevant diagrams repositories from github:

* [diagrams-core](https://github.com/diagrams/diagrams-core/)
* [diagrams-solve](https://github.com/diagrams/diagrams-solve/)
* [active](https://github.com/diagrams/active/)
* [diagrams-lib](https://github.com/diagrams/diagrams-lib/)
* [diagrams-cairo](https://github.com/diagrams/diagrams-rasterific/)
* [diagrams-contrib](https://github.com/diagrams/diagrams-contrib/)
* [diagrams-builder](https://github.com/diagrams/diagrams-builder/)
* [diagrams-rasterific](https://github.com/diagrams/diagrams-rasterific/)
* [SVGFonts](https://github.com/diagrams/SVGFonts/)
* [palette](https://github.com/diagrams/palette/)
* [docutils](https://github.com/diagrams/docutils/)

Then run the following commands:
```
sudo apt-get install python-docutils
cabal sandbox init
cabal install diagrams-core/ diagrams-solve/ diagrams-lib/ diagrams-rasterific/ diagrams-contrib/ diagrams-builder/ SVGFonts/ palette/ docutils/ shake hakyll safe
cabal exec bash
cd diagrams-doc
ghc --make Shake -threaded
./Shake +RTS -N4 -RTS preview
```

## Building with `diagrams-svg`

The build system will first check for an installed `diagrams-rasterific`
package.  If none is found, it will fall back to using the
`diagrams-svg` package.  As of right now, there are no known issues
with using `diagrams-svg` for building the website (other than the
fact that some of the output ``.svg`` files are large).
