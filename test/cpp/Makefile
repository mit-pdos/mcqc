.PHONY=all

CPP=clang++-6.0
CCFLAGS?=-g -Wall -DNDEBUG -std=c++17

OPT=opt-6.0
OPTFLAGS?=-O3 -always-inline -dot-callgraph

all: curry visit test
pdf: curry.pdf visit.pdf test.pdf

curry: curry.cpp
	${CPP} ${CCFLAGS} $< -o $@
curry.pdf: curry.cpp
	${CPP} ${CCFLAGS} -emit-llvm -c $< -o $<.bc
	${OPT} ${OPTFLAGS} $<.bc -o $<.opt.bc
	dot -Tpdf callgraph.dot -o $@
	rm callgraph.dot $<.bc $<.opt.bc

visit: visit.cpp
	${CPP} ${CCFLAGS} $< -o $@
visit.pdf: visit.cpp
	${CPP} ${CCFLAGS} -emit-llvm -c $< -o $<.bc
	${OPT} ${OPTFLAGS} $<.bc -o $<.opt.bc
	dot -Tpdf callgraph.dot -o $@
	rm callgraph.dot $<.bc $<.opt.bc

test: test.cpp
	${CPP} ${CCFLAGS} $< -o $@
test.pdf: test.cpp
	${CPP} ${CCFLAGS} -emit-llvm -c $< -o $<.bc
	${OPT} ${OPTFLAGS} $<.bc -o $<.opt.bc
	dot -Tpdf callgraph.dot -o $@
	rm callgraph.dot $<.bc $<.opt.bc

clean:
	rm -f *.o *.gch *.pdf *.dot *.bc *.ll visit variadic test curry
	rm -rf *.dSYM
