stack build
stack exec diagrams-doc -- +RTS -N4 -RTS buildh
rsync -r web/_site/* byorgey@projects.haskell.org:/srv/projects/diagrams/
ssh byorgey@projects.haskell.org 'chmod -R o+rX /srv/projects/diagrams/*'
