default : diagrams-manual.html

%.exe : %.hs
	ghc --make $< -o $@

%.html : %.xml xml2html.exe
	./xml2html.exe $< $@

%.xml : %.rst
	rst2xml.py $< $@

%-du.html : %.rst
	rst2html.py $< $@

.SECONDARY : xml2html.exe

clean : 
	rm -f *.exe *.html *.o *.hi