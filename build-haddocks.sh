#!/bin/zsh

# prerequisites:
# - darcs, git, hub, hsenv installed.
# - Release packages on Hackage.
# - Have all the repos checked out at the release version.
# - cd to the root, under which all the repo directories live.

mkdir -p haddocks-tmp
cd haddocks-tmp
rm -r *
rm -r .hsenv_haddocks
hsenv --name=haddocks
source .hsenv_haddocks/bin/activate
echo 'documentation: True' >> .hsenv_haddocks/cabal/config
cd ..
# hub clone byorgey/cabal
# cd cabal
# git checkout cabal-1.16-haddock-fix
# cabal install cabal-install/ -j8
# cd ../..
cabal install gtk2hs-buildtools
cabal install diagrams diagrams-postscript diagrams-cairo diagrams-gtk diagrams-builder diagrams-haddock SVGFonts palette -j8
for f in vector-space-points monoid-extras dual-tree active core lib svg postscript cairo gtk contrib SVGFonts builder haddock palette
do
  cd $f
  cabal sandbox delete  # be sure to remove any sandboxes so we use the global hsenv one instead
  cabal configure
  cd ..
done
cd haddocks-tmp
darcs get --lazy http://hub.darcs.net/byorgey/hproj
cabal install hproj/ -j8
cd ..
mkdir -p haddocks-tmp/haddock
hproj doc -o haddocks-tmp/haddock -t 'The diagrams framework' vector-space-points monoid-extras dual-tree active core lib svg postscript cairo gtk contrib SVGFonts builder haddock palette
mkdir -p haddocks-tmp/haddock/diagrams
for f in core lib contrib SVGFonts haddock
do
  cp $f/diagrams/*.svg haddocks-tmp/haddock/diagrams/
done

