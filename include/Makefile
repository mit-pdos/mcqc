.PHONY=all

CPP=clang++
CCFLAGS?=-g -DNDEBUG -std=c++17 -Wno-switch-bool -lc

OPT=opt-6.0
OPTFLAGS?=-O3 -always-inline -dot-callgraph

all: main
pdf: main.pdf

main: main.cpp
	${CPP} ${CCFLAGS} $< -o $@
main.pdf: main.cpp
	${CPP} ${CCFLAGS} -emit-llvm -c $< -o $<.bc
	${OPT} ${OPTFLAGS} $<.bc -o $<.opt.bc
	dot -Tpdf callgraph.dot -o $@
	rm callgraph.dot $<.bc $<.opt.bc

clean:
	- rm -rf main *.pdf *.gch *.dSYM
