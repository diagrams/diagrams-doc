EXAMPLES = $(shell ls *.lhs)
IMAGES   = $(EXAMPLES:.lhs=.png)

all : $(IMAGES)

Build : Build.hs
	ghc --make Build

%.png : %.lhs
	./Build $(basename $<)

