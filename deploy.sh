ghc --make Shake
./Shake build
rsync -r _site/* byorgey@projects.haskell.org:/srv/projects/diagrams/
ssh byorgey@projects.haskell.org 'chmod -R o+rX /srv/projects/diagrams/*'
