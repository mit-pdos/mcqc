.PHONY=all

CXX=clang++
CXXFLAGS?=-g -O3 -fno-stack-protector -pedantic -Wno-switch-bool -std=c++17 -I../../include

all: t

Tree.json: Tree.v
	coqc -Q ../../classes "" $<

Tree.cpp: Tree.json
	stack exec mcqc -- $<

t: Tree.cpp ../../include/*
	@echo --------------------------------------------------------------------------------
	@echo Compiling $@ ...
	${CXX} ${CXXFLAGS} $< -o $@

prof: t
	@echo --------------------------------------------------------------------------------
	@echo Profiling $@ ...
	valgrind --tool=massif ./$<

clean:
	-rm -rf t Tree.cpp *.dSYM *.glob *.vo *.json *.o

