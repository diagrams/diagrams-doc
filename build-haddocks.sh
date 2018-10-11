#!/bin/zsh

# prerequisites:
# - standalone-haddock installed.
# - Release packages on Hackage.
# - Have all the repos checked out at the release version.
# - cd into root of diagrams-doc repo.
# - All other repos are in directories that are siblings of the
#   diagrams-doc repo.

# After running:
# - creates diagrams-doc/standalone-sandbox
# - creates standalone-haddock directory (sibling of all repo dirs)
#   with standalone haddocks

INSTALL_PKGS="-fcairo -fgtk -fsvg -fps -frasterific -fcanvas diagrams diagrams-builder diagrams-graphviz diagrams-haddock diagrams-html5 diagrams-pgf palette SVGFonts"

DOC_DIRS="active builder cairo canvas contrib core dual-tree force-layout graphviz gtk haddock html5 lib monoid-extras palette pgf postscript rasterific solve svg SVGFonts"

cabal update

rm -rf ../standalone-haddock
mkdir -p standalone-sandbox
cd standalone-sandbox
cabal sandbox delete
cabal sandbox init
echo 'documentation: True' > cabal.config
cabal install alex gtk2hs-buildtools
cabal install ${=INSTALL_PKGS}
PKGDB=`cabal exec printenv CABAL_SANDBOX_PACKAGE_PATH | cut -d ':' -f 1`
cd ../..
standalone-haddock --hyperlink-source -o standalone-haddock --package-db=$PKGDB ${=DOC_DIRS}
sed -i -e 's/Standalone Haskell documentation/The diagrams framework/' standalone-haddock/index.html
