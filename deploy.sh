ghc --make Shake
./Shake buildh
rsync -r web/_site/* byorgey@projects.haskell.org:/srv/projects/diagrams/
ssh byorgey@projects.haskell.org 'chmod -R o+rX /srv/projects/diagrams/*'
