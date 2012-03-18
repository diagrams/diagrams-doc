WEB = ${HOME}/public_html/diagrams-manual

default : diagrams-manual.html

xml2html.exe : xml2html.hs
	ghc --make -O2 $< -o $@

%.html : %.xml xml2html.exe build-icons
	./xml2html.exe $< $@

build-icons :
	make -C icons

%.xml : %.rst
	rst2xml.py --input-encoding=utf8 $< $@

.SECONDARY : xml2html.exe diagrams-manual.xml

clean : 
	rm -f *.exe *.html *.o *.hi *.xml

imgclean :
	rm -f images/*.png

veryclean : clean imgclean

preview : diagrams-manual.html
	cp diagrams-manual.{html,rst} $(WEB)
	mkdir -p $(WEB)/images
	cp images/* $(WEB)/images
	mkdir -p $(WEB)/icons
	cp icons/*.png $(WEB)/icons
	chmod -R go+rX $(WEB)

deploy-web : diagrams-manual.html
	mkdir -p ../web/manual
	cp diagrams-manual.{html,rst} ../web/manual
	mkdir -p ../web/manual/static
	cp static/* ../web/manual/static
	mkdir -p ../web/manual/images
	cp images/* ../web/manual/images
	mkdir -p ../web/manual/icons
	cp icons/*.png ../web/manual/icons
