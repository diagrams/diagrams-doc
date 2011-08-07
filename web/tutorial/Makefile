# To get pandoc, use
#   cabal install -fhighlighting pandoc

default : DiagramsTutorial.html

%.html : %.lhs
	pandoc -f markdown+lhs -t html+lhs -s -o $@ $<

exp : DiagramsTutorial.html DiagramsTutorial.lhs
	scp DiagramsTutorial.lhs DiagramsTutorial.html byorgey@projects.haskell.org:/srv/projects/diagrams/tutorial/