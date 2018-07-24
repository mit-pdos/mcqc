.PHONY=all

CXX=clang++
CXXFLAGS?=-g -O3 -fno-stack-protector -pedantic -Wno-switch-bool -std=c++17 -I../../include

all: rev

Rev.json: Rev.v
	coqc $<

Rev.cpp: Rev.json
	stack exec machcoq -- $<

rev: main.cpp Rev.cpp
	@echo --------------------------------------------------------------------------------
	@echo Compiling $@ ...
	${CXX} ${CXXFLAGS} $< -o $@

prof: rev
	@echo --------------------------------------------------------------------------------
	@echo Profiling $@ ...
	valgrind --leak-check=full ./$<

clean:
	-rm -rf rev Rev.cpp *.dSYM *.glob *.vo *.json *.o

