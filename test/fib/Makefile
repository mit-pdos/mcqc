.PHONY=all

CXX=clang++
CXXFLAGS?=-g -O3 -fno-stack-protector -pedantic -Wno-switch-bool -std=c++17 -I../../include

all: fib

Fib.json: Fib.v
	coqc $<

Fib.cpp: Fib.json
	stack exec machcoq -- $<

fib: main.cpp Fib.cpp
	@echo --------------------------------------------------------------------------------
	@echo Compiling $@ ...
	${CXX} ${CXXFLAGS} $< -o $@

prof: fib
	@echo --------------------------------------------------------------------------------
	@echo Profiling $@ ...
	valgrind --leak-check=full ./$<

clean:
	-rm -rf fib Fib.cpp *.dSYM *.glob *.vo *.json *.o

