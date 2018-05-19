.PHONY=all

CPP=clang++-6.0
CCFLAGS?=-g -Wall -DNDEBUG -std=c++17

OPT=opt-6.0
OPTFLAGS?=-O3 -always-inline -dot-callgraph

all: highorder opcodes
pdf: highorder.pdf
coq: Unbind.hs Bind.json

Unbind.hs: unbind.v
	coqc $^
opcodes: Unbind.hs
	ghc opcodes.hs -o $@

highorder: highorder.cpp
	${CPP} ${CCFLAGS} $< -o $@
highorder.pdf: highorder.cpp
	${CPP} ${CCFLAGS} -emit-llvm -c $< -o $<.bc
	${OPT} ${OPTFLAGS} $<.bc -o $<.opt.bc
	dot -Tpdf callgraph.dot -o $@
	rm callgraph.dot $<.bc $<.opt.bc

Bind.json: Bind.v
	coqc $^

clean:
	rm -f fib fib_typ opcodes Top.hs Datatypes.hs *.hi *.glob *.vo *.json *.o highorder
	rm -rf *.dSYM

