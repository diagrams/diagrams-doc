hakyll : hakyll.hs
	ghc --make hakyll.hs

preview : hakyll
	./hakyll rebuild
	./hakyll preview

deploy : hakyll
	./hakyll rebuild
	rsync -r _site/* byorgey@projects.haskell.org:/srv/projects/diagrams/
	ssh byorgey@projects.haskell.org chmod -R o+rX /srv/projects/diagrams/*