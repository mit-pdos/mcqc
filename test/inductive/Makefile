.PHONY=all

CXX=clang++
CXXFLAGS?=-g -O3 -fno-stack-protector -pedantic -Wno-switch-bool -std=c++17 -I../../include

all: list

List.json: List.v
	coqc -Q ../../classes "" $<

List.cpp: List.json
	stack exec mcqc -- $<

list: main.cpp List.cpp ../../include/*
	@echo --------------------------------------------------------------------------------
	@echo Compiling $@ ...
	${CXX} ${CXXFLAGS} $< -o $@

prof: list
	@echo --------------------------------------------------------------------------------
	@echo Profiling $@ ...
	valgrind --tool=massif ./$<

clean:
	-rm -rf list List.cpp *.dSYM *.glob *.vo *.json *.o

