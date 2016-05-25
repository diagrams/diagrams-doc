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

* [stack](http://github.com/commercialhaskell/stack)
* the python [docutils suite](http://docutils.sourceforge.net/) (in
  particular `rst2xml.py` should be on your PATH).

Once you have all the dependencies, simply do

    stack build
    stack exec diagrams-doc -- +RTS -N4 -RTS preview

which will build the user manual and website, and run a web server on
port 8000 serving a preview of the website. In place of `-N4` you
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
stack installed and want to use the rasterific backend, you could
run the following commands:

```
sudo apt-get install python-docutils
stack build
stack exec diagrams-doc -- +RTS -N4 -RTS preview
```
