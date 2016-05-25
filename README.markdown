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

### Step 1: Get dependencies

To build the website (which includes the gallery, user manual, and
tutorials), you will need:

* [stack](http://github.com/commercialhaskell/stack)
* the python [docutils suite](http://docutils.sourceforge.net/) (in
  particular `rst2xml.py` should be on your PATH).

On Ubuntu, the docutils suite can be installed using
```bash
sudo apt-get install python-docutils
```

### Step 2: Generate the `stack.yaml` file

Run the command

```bash
./generate-stack-yaml.hs
```

This will generate a `stack.yaml` file that contains the necessary
dependencies to build this project. This includes each Diagrams Github repo,
so that you can build the documentation against the latest versions of each
project. If new commits get pushed to the Diagrams repos, you may need to
re-run `./generate-stack-yaml.hs`.

### Step 3: Run the builder

Once you have all the dependencies, simply do

```bash
stack build
stack exec diagrams-doc -- +RTS -N4 -RTS preview
```

which will build the user manual and website, and run a web server on
port 8000 serving a preview of the website. In place of `-N4` you
should use `-NX` where X is the number of cores you have. By default,
the build system will use all but one of your available cores; if you
wish to set the number to something else, set the `DIA_DOC_THREADS`
environment variable.

Point your browser at `localhost:8000` to view it.  It works well to
leave this invocation of `stack exec diagrams-doc -- preview` running, and then start
another process calling `stack exec diagrams-doc -- build` repeatedly in a
loop.  The website will now automatically be rebuilt any time any
source files change.
