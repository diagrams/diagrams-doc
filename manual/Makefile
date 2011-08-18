WEB = ${HOME}/public_html/diagrams-manual

default : diagrams-manual.html

xml2html.exe : xml2html.hs Build.hs
	ghc --make -O2 $< -o $@

%.html : %.xml xml2html.exe build-icons
	./xml2html.exe $< $@

build-icons :
	make -C icons

%.xml : %.rst
	rst2xml.py $< $@

.SECONDARY : xml2html.exe diagrams-manual.xml

clean : 
	rm -f *.exe *.html *.o *.hi *.xml

imgclean :
	rm -f images/*.png

veryclean : clean imgclean

preview : diagrams-manual.html
	cp diagrams-manual.{html,rst} $(WEB)
	cp *.css $(WEB)
	cp images/* $(WEB)/images
	chmod -R go+rX $(WEB)