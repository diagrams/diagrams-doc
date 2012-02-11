Have a fun example or useful tool to contribute to the
`diagrams-contrib` package?  Want to help [knock out some
bugs](http://code.google.com/p/diagrams/issues/list) and feature
requests? Great!  This tutorial will help get you up to speed
contributing to `diagrams`.  It assumes a bit of familiarity with the
diagrams framework itself (*i.e.* you have used it to create some
diagrams), but no familiarity with darcs.

As a running example, let's suppose you want to develop an extension
module providing a primitive "house" shape. XXX picture?  We'll walk
through the entire process of creating, testing, and submitting such
an extension.  I encourage you to follow along, either by copying and
pasting the code shown here, or you can just use it as a guideline for
making your own extension.

Before we begin, here are some other resources which may be useful to
you during this process:

  * XXX

If you have any questions or run into any problems while following
this tutorial, feel free to ask on the [diagrams mailing list](XXX) XXX,
or in the `#diagrams` channel on irc.freenode.net.

Getting started
===============

Getting the latest darcs sources
--------------------------------

The first step in hacking on diagrams is to get the latest development
sources using [darcs](http://darcs.net/). `darcs` is an elegant
distributed revision control system used to track the diagrams source
code.  If you already have the darcs sources for diagrams, you can
skip this section.

First, of course, you will have to install darcs itself; it probably
already exists as a package for your operating system's package
manager.  Otherwise, you can grab a tarball from
[darcs.net](http://darcs.net/).

Next, create a directory where you will store the diagrams
repositories (you will need several, so it's probably
best to collect them all as subdirectories of a single directory):

    [brent@archimedes:~]$ mkdir diagrams
    [brent@archimedes:~]$ cd diagrams
    [brent@archimedes:~/diagrams]$

Now, use `darcs get` to create new directories and download the
current repositories into them. At the very least you will need `diagrams-core`, `diagrams-lib`, `active`, some backend such as `diagrams-cairo`, and `diagrams-contrib`.

    [brent@archimedes:~/diagrams]$ darcs get http://patch-tag.com/r/byorgey/diagrams-core core
    Copying patches, to get lazy repository hit ctrl-C...
    Finished getting.
    [brent@archimedes:~/diagrams]$ darcs get http://patch-tag.com/r/byorgey/diagrams-lib lib
    Copying patches, to get lazy repository hit ctrl-C...
    Finished getting.
    [brent@archimedes:~/diagrams]$ darcs get http://patch-tag.com/r/byorgey/active
    Copying patches, to get lazy repository hit ctrl-C...
    Finished getting.
    [brent@archimedes:~/diagrams]$ darcs get http://patch-tag.com/r/byorgey/diagrams-cairo cairo
    Copying patches, to get lazy repository hit ctrl-C...
    Finished getting.
    [brent@archimedes:~/diagrams]$ darcs get http://patch-tag.com/r/byorgey/diagrams-contrib contrib
    Copying patches, to get lazy repository hit ctrl-C...
    Finished getting.
    [brent@archimedes:~/diagrams]$ ls
    active cairo  contrib  core  lib

Be patient, it might take a while to download the entire repositories
the first time.  After the initial download, however, subsequent pulls
should be much faster, since darcs will only need to download any new
patches.

Congratulations, you now have the latest diagrams sources!

Staying up-to-date
------------------

To keep your local repositories up-to-date, you should periodically
run the command `darcs pull` in each directory in order to download
any new patches, especially before starting any new development.
Darcs is generally good at merging patches, but it's best not to make
it work any harder than necessary.

Building
--------

To build the latest development versions of
just issue the usual incantations:

 [brent@archimedes:~/diagrams]$ cd xmonad/
 [brent@archimedes:~/diagrams/xmonad]$ cabal install
 Configuring xmonad-0.9.1...
 Preprocessing library xmonad-0.9.1...
 Preprocessing executables for xmonad-0.9.1...
 Building xmonad-0.9.1...
 ...blah blah...
 Writing new package config file... done.
 [brent@archimedes:~/diagrams/xmonad]$ cd ../XMonadContrib/
 [brent@archimedes:~/diagrams/XMonadContrib]$ cabal install
 Configuring xmonad-contrib-0.9.1...
 Preprocessing library xmonad-contrib-0.9.1...
 Building xmonad-contrib-0.9.1...
 ...blah blah BLAH...
 Writing new package config file... done.

Now restart xmonad (run `touch ~/.xmonad/xmonad.hs`, then hit mod-q).  Note, if you are upgrading from a previous version of xmonad, you may need to first unregister the old packages with ghc.  Type `ghc-pkg list xmonad` and `ghc-pkg list xmonad-contrib` at a prompt; if multiple versions are listed you need to unregister all but the newest.  For example:

 [brent@archimedes:~/diagrams]$ ghc-pkg unregister xmonad-0.5 --user
 Saving old package config file... done.
 Writing new package config file... done.
 [brent@archimedes:~/diagrams]$ ghc-pkg unregister xmonad-contrib-0.5 --user
 Saving old package config file... done.
 Writing new package config file... done.

=== Important note on rebuilding ===

If you pull new patches into the xmonad repository or make changes to it, and rebuild the xmonad library, you should do a clean build of the xmonad-contrib library afterwards (`cabal clean && cabal install`).  Otherwise, the Cabal build process for xmonad-contrib may not notice that the xmonad library (which xmonad-contrib depends on) has changed, and xmonad-contrib will not get rebuilt properly, leading to possible crashes at runtime.  Annoying, but necessary.

=== An important Haddock/documentation note ===

The version of [[Haddock]] which is still bundled in several OS package systems (Debian unstable, for example) is too old to handle some of the features used in xmonad.  The most recent version of haddock (as of 15 November 2010) is 2.8.1.  Until it is updated in your OS's package system, you will have to install the latest version of haddock manually; you can download it from [http://hackage.haskell.org Hackage].  Or, if you have the fabulous [http://hackage.haskell.org/cgi-bin/hackage-scripts/package/cabal%2Dinstall cabal-install tool], you can just type `cabal install haddock` at a prompt.

== Creating an extension ==

=== Adding the module file ===

So, let's add that module!  We'll call it XMonad.Actions.HelloWorld, so it needs to go in the XMonad/Actions/ subdirectory of the XMonadContrib repository:

 [brent@archimedes:~/diagrams/XMonadContrib]$ $EDITOR XMonad/Actions/HelloWorld.hs &

To avoid flame wars, I won't reveal my favorite editor here. =)

Now we've got... a nice, blank module!  What now?  Well, let's begin by adding some standard header stuff.  In practice you can just open up some other module and copy and paste, changing the stuff that's appropriate to change.

<haskell>
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.HelloWorld
-- Copyright   :  (c) 2008 Brent Yorgey
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Brent Yorgey <byorgey@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides an action to pop up a \"hello, world\" window using xmessage.
--
-----------------------------------------------------------------------------

module XMonad.Actions.HelloWorld where
</haskell>

OK, that's a good start.  Now let's go to a prompt and see what we've done so far:

 [brent@archimedes:~/diagrams/XMonadContrib]$ darcs whatsnew
 No changes!

Huh? No changes?  Well, that's because we haven't added HelloWorld.hs to the repository yet, so darcs ignores it.  Let's do that now:

 [brent@archimedes:~/diagrams/XMonadContrib]$ darcs add XMonad/Actions/HelloWorld.hs
 [brent@archimedes:~/diagrams/XMonadContrib]$ darcs whatsnew
 {
 addfile ./XMonad/Actions/HelloWorld.hs
 hunk ./XMonad/Actions/HelloWorld.hs 1
 +-----------------------------------------------------------------------------
 +-- |
 +-- Module      :  XMonad.Actions.HelloWorld
 ...

Ah, much better!  In general, the `darcs whatsnew` command shows any changes to files tracked by darcs that are currently unrecorded.  To finish things off for now, we add the <hask>helloWorld</hask> function, and a required import (<hask>XMonad.Core</hask> is where we get the <hask>spawn</hask> function).

<haskell>
import XMonad.Core

helloWorld :: X ()
helloWorld = spawn "xmessage 'Hello, world!'"
</haskell>

=== Adding our new module to the xmonad-contrib library ===

Our new module is great, but it won't get compiled as part of the xmonad-contrib library unless we add a reference to it in xmonad-contrib.cabal.

 [brent@archimedes:~/diagrams/XMonadContrib]$ $EDITOR xmonad-contrib.cabal
 [brent@archimedes:~/diagrams/XMonadContrib]$ darcs whatsnew -u xmonad-contrib.cabal
 What's new in "xmonad-contrib.cabal":
 
 {
 hunk ./xmonad-contrib.cabal 74
                          XMonad.Actions.FlexibleResize
                          XMonad.Actions.FloatKeys
                          XMonad.Actions.FocusNth
 +                        XMonad.Actions.HelloWorld
                          XMonad.Actions.MouseGestures
                          XMonad.Actions.MouseResize
                          XMonad.Actions.NoBorders
 }

The + indicates the line that we added to the .cabal file. (Note that the module list is generally kept in alphabetical order.) Be sure to indent with spaces, not tabs!  Since indentation is important in .cabal files (just as in Python or Haskell), tabs are not allowed.

=== Building and testing ===

We first rebuild and reinstall the xmonad-contrib library.

 [brent@archimedes:~/diagrams/XMonadContrib]$ cabal install
 Configuring xmonad-contrib-0.9.1...
 Preprocessing library xmonad-contrib-0.9.1...
 Building xmonad-contrib-0.9.1...
 [ 91 of 112] Compiling XMonad.Actions.HelloWorld ( XMonad/Actions/HelloWorld.hs, dist/build/XMonad/Actions/HelloWorld.o )
 /usr/bin/ar: creating dist/build/libHSxmonad-contrib-0.9.1.a
 ...

Now, to test out our new module, we can just import it into xmonad.hs, add a keybinding for the <hask>helloWorld</hask> action, and restart with mod-q. Now (assuming you have the xmessage utility installed) hitting the selected keybinding should pop up a little "Hello, world" window in the upper-left corner of the screen.  Neat!

=== Clean-up and coding standards ===

Well, our module works, but it's in no state to be distributed to the wider world yet!  We'll have to clean it up to conform to a few standards for xmonad-contrib source code.  In particular:

* All exported functions should have Haddock documentation and explicit type signatures.  It's a good idea to give documentation and explicit type signatures for unexported functions, too, to make it easier for others to understand or modify your code.
* A "Usage" section should be added in the comments, explaining the purpose of the module and giving examples of its use.
* It's usual to explicitly list the module's exports, rather than implicitly exporting everything.

There are [http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Doc-Developing.html#8 other standards, too], but these are the most relevant to us at the moment.

Here's our module after making these updates (not including the header comments):

<haskell>
module XMonad.Actions.HelloWorld (
                                  -- * Usage
                                  -- $usage

                                  helloWorld

                                 ) where

import XMonad.Core

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Actions.HelloWorld
--
-- Then add a keybinding for 'helloWorld':
--
-- >   , ((modMask x .|. controlMask, xK_h), helloWorld)
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

-- | Pop up a \"hello, world\" window using @xmessage@.  You /must/ have
--   the @xmessage@ utility installed for this to work.
helloWorld :: X ()
helloWorld = spawn "xmessage 'Hello, world!'"
</haskell>

Note how we added a type signature and Haddock documentation for <hask>helloWorld</hask> (the pipe character | indicates a Haddock comment), and some usage information which will get included in the generated Haddock documentation (Haddock generates documentation for exported functions in the order they are listed in the export list between parentheses following the module declaration, along with any extra comments included there).  Some quick notes about Haddock documentation format:

* Use @ symbols to surround code which should be printed in a `verbatim style`.
* Use 'single quotes' around function names to generate links to their documentation.  For example, see how 'helloWorld' has single quotes in the usage information shown above.
* Use "double quotes" around module names to generate links to their documentation.
* Use /front slashes/ to italicize.
* Escape literal quotes and frontslashes with a backslash.
* Literal code blocks can be included with "bird tracks", i.e. greater-than symbols preceding the code.  Be sure to put a blank line on either side of such code blocks.

For more information, see the [http://www.haskell.org/haddock/doc/html/index.html Haddock documentation].

=== Testing documentation ===

It's really important to test the generated documentation before submitting our new extension.  A module with poor or missing documentation (or great documentation which is not included because of parse errors) will not be very useful to people.  Conversely, a module with excellent documentation and good examples will be a pleasure to use, easier to modify and extend, and a help to others trying to use the code as an example for creating their own extension!

To generate the documentation, first rebuild the xmonad-contrib library, then use `cabal haddock`:

 [brent@archimedes:~/diagrams/XMonadContrib]$ cabal install
 Configuring xmonad-contrib-0.9.1...
 ...blah blah...
 [brent@archimedes:~/diagrams/XMonadContrib]$ cabal haddock
 Preprocessing library xmonad-contrib-0.9.1...
 Running Haddock for xmonad-contrib-0.9.1...
 ...lots of warnings that can be ignored...
 Documentation created: dist/doc/html/xmonad-contrib/index.html

Generating the Haddock documentation will generate a ton of warnings such as "could not find link destinations for: M.Map"; you can ignore these.  The important line is the last one, which says whether the documentation was successfully created, and where it was generated.

In this case, to view the generated documentation, you should point your browser to something like `file:///path/to/XMonadContrib/dist/doc/html/xmonad-contrib/index.html`, then navigate to the documentation for your module. Check that the formatting, links, and so on are correct.  For example, when first writing this tutorial, I forgot to escape the double quotes around "hello world" in the comments for the <hask>helloWorld</hask> function, which therefore showed up as a link to the (nonexistent) "hello world" module, instead of showing literal double quote characters.

If you make any changes to your documentation, you can simply rerun `cabal haddock` to regenerate; there's no need to rebuild everything as long as you only changed documentation.

Once you are satisfied with your documentation, it's probably a good idea to rebuild the xmonad-contrib library, restart xmonad, and test your extension one more time.

====Source links====
On newer versions of Cabal, install the hscolour package, and build your docs using the following command to include links to source from each function, type, etc.

`cabal haddock --hyperlink-source`

====Dialing in documentation for a single module====

While it's no huge chore to build all the Haddock documentation, and you should do that before submitting your extension, sometimes you just want to build documentation for one or a few modules that aren't necessarily even finished yet. The following bash alias is a quick and dirty way to build html documentation in a draft document directory for the files specified on the command line.

`
alias "hddck."='tmp=~/tmp/haddock; <nowiki>[[</nowiki> -d $tmp/html ]] || mkdir $tmp/html; base=`pwd`; haddock --html --source-module file://'$base'/%F --odir $tmp/html '
`

== Submitting your extension ==

=== Recording your patches ===

The first step is to record our changes as one or more darcs patches.  First, let's see what we've changed:

 [brent@archimedes:~/diagrams/XMonadContrib]$ darcs whatsnew -s
 A ./XMonad/Actions/HelloWorld.hs
 M ./xmonad-contrib.cabal +1

The `-s` option makes darcs display a summary; if you leave the -s option off it will show more details about each patch.

To record the patches, type `darcs record` at a prompt.  If this is your first time recording any patches, it will first ask you for your name and e-mail address.  Then it will prompt you for each change, asking whether you would like to record it.  In general, you will find that darcs has a very nice interface, prompting you about most things and giving you a lot of flexibility in choosing what exactly you would like to do, without having to memorize lots of complicated command-line flags.  In this particular case, it's nice that you can hand-pick which changes should be recorded as a patch -- so you could be working on several things at once, and record them separately as you finish, without messing up the other things.

Note that for the xmonad-contrib repository, darcs is configured to run some tests before committing a patch.  In particular, it will try compiling everything from scratch and generating all the documentation.  If there are any errors, you must fix them before you will be allowed to record your patch.  It can be a bit tiresome waiting for the tests to complete (especially if you are on a slower computer), but it ensures that no one can accidentally push a patch which breaks the repository.  The repository should always be in a compilable state.

After recording a patch, running `darcs changes --last 1` should show you that your patch is recorded.  At this point `darcs whatsnew` will once again say 'No changes!' because there are no longer any unrecorded changes.

=== Sending your patch ===

And now, the moment of truth: sending your patch to the xmonad mailing list for inclusion in the main repository!  First make sure you are subscribed; as an anti-spam measure, only list subscribers are allowed to post.

Now, if you have a mail agent configured correctly, all you have to do is type `darcs send` at the prompt, and it will send the patches you select as attachments to an e-mail to the xmonad list (`xmonad@haskell.org`).  Alternatively, you can `darcs send` to an output file with the `-o` option:

 [brent@archimedes:~/diagrams/XMonadContrib]$ darcs send -o ../helloWorld.dpatch
 Creating patch to "http://code.haskell.org/XMonadContrib"...

 Wed Feb 20 15:12:19 EST 2008  Brent Yorgey <byorgey@gmail.com>
  * new contrib module: XMonad.Actions.HelloWorld, for popping up a hello world message
 Shall I send this patch? (1/1)  [ynWvpxqadjk], or ? for help: y

Then write an e-mail to the [http://www.haskell.org/mailman/listinfo/xmonad xmonad mailing list] and manually attach the generated patch file.

That's it! Happy xmonad hacking!

== Postscript ==

Questions? Suggestions? Confusions? Edit this tutorial yourself (it's a wiki, after all!), send an email to the [http://www.haskell.org/mailman/listinfo/xmonad xmonad mailing list], or join the `#xmonad` channel on irc.freenode.net.

For further reading, check out the [http://haskell.org/haskellwiki/Xmonad/Guided_tour_of_the_xmonad_source guided tour of the xmonad source code], or browse the [http://www.xmonad.org/xmonad-docs/ xmonad and xmonad-contrib documentation].