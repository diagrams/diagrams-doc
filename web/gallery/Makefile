# EXAMPLES = $(shell ls *.lhs)
# IMAGES   = $(EXAMPLES:.lhs=.png)

all : Build

Build.exe : Build.hs
	ghc --make Build -o Build.exe

# %.png : %.lhs Build
# 	./Build $(basename $<)

