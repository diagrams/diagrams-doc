[![Build Status](https://secure.travis-ci.org/diagrams/diagrams-doc.png)](http://travis-ci.org/diagrams/diagrams-doc)

This repository holds all documentation for the
[diagrams framework](http://projects.haskell.org/diagrams), including

* the [website](http://projects.haskell.org) source
* tutorials
* user manual
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
  [cairo backend](http://github.com/diagrams/diagrams-cairo),
  [SVGFonts package](http://github.com/diagrams/SVGFonts), and
  [diagrams-builder package](http://github.com/diagrams/diagrams-builder)).
* the python [docutils suite](http://docutils.sourceforge.net/) (in
  particular `rst2xml` should be on your PATH),
* the Haskell [docutils package](http://github.com/diagrams/docutils), and
* the latest version of [shake](http://hackage.haskell.org/package/shake).
* [hakyll](http://hackage.haskell.org/package/hakyll)-4.x.

Once you have all the dependencies, simply do

    ghc --make Shake -threaded
    ./Shake +RTS -N4 -RTS preview

which will build the user manual and website, and run a web server on
port 8000 serving a preview of the webiste. (In place of `-N4` you
should use `-NX` where X is the number of cores you have.)  Point your
browser at `localhost:8000` to view it.  It works well to leave this
invocation of `Shake preview` running, and then start another process
calling `Shake +RTS -NX -RTS build` repeatedly in a loop.  The
website will now automatically be rebuilt any time any source files
change.

For example, on a four-core Ubuntu machine, assuming you already have
the Haskell Platform installed, and have cloned the relevant diagrams
repositories from github, you could issue the commands

```
sudo apt-get install libgtk-3-dev libcairo2-dev python-docutils
cabal install hsenv
hsenv --name=dia
.hsenv_dia/bin/activate
cabal install gtk2hs-buildtools shake hakyll
cabal install diagrams-core/ diagrams-lib/ diagrams-cairo/ diagrams-contrib/ diagrams-builder/ SVGFonts/ docutils/
cd diagrams-doc
ghc --make Shake -threaded
./Shake +RTS -N4 -RTS preview
```
