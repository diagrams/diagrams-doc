ICONS = $(patsubst %.hs,%.png,$(wildcard *.hs))

all : $(ICONS)

%.exe : %.hs
	ghc --make $< -o $@

%.png : %.exe
	./$< -w 40 -h 40 -o $@

clean :
	rm -f *.exe *.o *.hi