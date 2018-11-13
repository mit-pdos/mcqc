.PHONY=all

CXX=clang++
CXXFLAGS?=-g -O3 -fno-stack-protector -pedantic -Wno-switch-bool -std=c++17 -I../../include

all: ind

Ind.json: Ind.v
	coqc -Q ../../classes "" $<

Ind.cpp: Ind.json
	stack exec machcoq -- -I../../classes $<

ind: main.cpp Ind.cpp ../../include/*
	@echo --------------------------------------------------------------------------------
	@echo Compiling $@ ...
	${CXX} ${CXXFLAGS} $< -o $@

prof: ind
	@echo --------------------------------------------------------------------------------
	@echo Profiling $@ ...
	valgrind --tool=massif ./$<

clean:
	-rm -rf ind Ind.cpp *.dSYM *.glob *.vo *.json *.o

