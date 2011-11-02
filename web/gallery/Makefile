# EXAMPLES = $(shell ls *.lhs)
# IMAGES   = $(EXAMPLES:.lhs=.png)

all : Build.exe

Build.exe : Build.hs
	ghc --make Build -o Build.exe

# %.png : %.lhs Build.exe
# 	./Build.exe $< $@

# clean:
#	rm -f *.exe
#	rm -f *.png
#	rm -f *~
#	rm -f *.hi

# NOTE: all the above is commented out because we generate .pngs from
# hakyll.hs now.