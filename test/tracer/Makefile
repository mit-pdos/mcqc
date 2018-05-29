.PHONY=all

CPP=clang++
CCFLAGS?=-g -Wall -DNDEBUG -std=c++17

all: main
Unbind.hs: unbind.v
	coqc $^
comp: comp.hs Unbind.hs
	ghc $< -o $@
main: comp
	./$< > main.cpp
	${CPP} ${CCFLAGS} main.cpp -o $@

clean:
	-rm -f main comp Unbind.hs Top.hs Datatypes.hs main.cpp *.hi *.glob *.vo *.json *.o
	-rm -rf *.dSYM
