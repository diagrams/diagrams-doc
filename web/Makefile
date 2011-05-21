hakyll : hakyll.hs
	ghc --make hakyll.hs

images :
	make -C gallery

preview : hakyll images
	./hakyll rebuild
	./hakyll preview

deploy : hakyll images
	./hakyll rebuild
	rsync -r _site/* byorgey@projects.haskell.org:/srv/projects/diagrams/
	ssh byorgey@projects.haskell.org chmod -R o+rX /srv/projects/diagrams/*