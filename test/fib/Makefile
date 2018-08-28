.PHONY=all

CXX=clang++
CXXFLAGS?=-O3 -fno-stack-protector -pedantic -Wno-switch-bool -std=c++17 -I../../include

all: fib

Fib.json: Fib.v
	coqc $<

Fib.cpp: Fib.json
	stack exec machcoq -- $<

fib: main.cpp Fib.cpp ../../include/*
	@echo --------------------------------------------------------------------------------
	@echo Compiling $@ ...
	${CXX} ${CXXFLAGS} $< -o $@

bench: FibHs.v Main.hs
	coqc $<
	ghc -O3 Main.hs -o fibhs

prof: fib
	@echo --------------------------------------------------------------------------------
	@echo Profiling $@ ...
	valgrind --leak-check=full ./$<

clean:
	-rm -rf fib Fib.cpp *.dSYM *.glob *.vo *.json *.o
	-rm -rf FibHs.hs Datatypes.hs Nat.hs fibhs *.hi *.o
