default : diagrams-manual.html

xml2html.exe : xml2html.hs Build.hs
	ghc --make $< -o $@

%.html : %.xml xml2html.exe
	./xml2html.exe $< $@

%.xml : %.rst
	./dia-rst2xml.py $< | sed -e 's/http:\/\/docutils.sourceforge.net\/docs\/ref\/docutils.dtd/\/home\/brent\/downloads\/docutils\/docs\/ref\/docutils.dtd/' > $@

%-du.html : %.rst
	rst2html.py $< $@

.SECONDARY : xml2html.exe diagrams-manual.xml

clean : 
	rm -f *.exe *.html *.o *.hi *.xml

veryclean : clean
	rm -f images/*.png
