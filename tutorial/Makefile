# To get pandoc, use
#   cabal install -fhighlighting pandoc

default : DiagramsTutorial.html

%.html : %.lhs
	pandoc -f markdown+lhs -t html+lhs -s -o $@ $<