hakyll.exe : hakyll.hs
	ghc --make hakyll.hs -o hakyll.exe

manual-cp :
	make -C ../manual deploy-web

gallery/Build.exe : gallery/Build.hs
	make -C gallery

preview : hakyll.exe gallery/Build.exe manual-cp
	./hakyll.exe rebuild
	./hakyll.exe preview

deploy : hakyll.exe gallery/Build.exe manual-cp
	./hakyll.exe rebuild
	rsync -r _site/* byorgey@projects.haskell.org:/srv/projects/diagrams/
	ssh byorgey@projects.haskell.org chmod -R o+rX /srv/projects/diagrams/*