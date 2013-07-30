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

To build the user manual, you will need:

* GHC 7.4 or 7.6
* the diagrams framework itself (including the
  [contrib package](http://github.com/diagrams/diagrams-contrib),
  [cairo backend](http://github.com/diagrams/diagrams-cairo), and
  [diagrams-builder package](http://github.com/diagrams/diagrams-builder)
  installed with the `-fcairo` flag),
* the python [docutils suite](http://docutils.sourceforge.net/) (in
  particular `rst2xml` should be on your PATH),
* the Haskell [docutils package](http://github.com/diagrams/docutils), and
* the latest version of [shake](http://hackage.haskell.org/package/shake).

To build the website, you will additionally need

* [hakyll](http://hackage.haskell.org/package/hakyll)-3.x (the
  diagrams website has not yet been ported to hakyll-4).

Once you have all the dependencies, simply do

    ghc --make Shake
    ./Shake preview

which will build the user manual and website, and run a web server on
port 8000 serving a preview of the webiste.  Point your browser at
`localhost:8000` to view it.
