.PHONY=all

CXX=clang++
CXXFLAGS?=-g -O3 -fno-stack-protector -pedantic -Wno-switch-bool -std=c++17 -I../../include

all: rev

Rev.json: Rev.v
	coqc -Q ../../classes "" $<

Rev.cpp: Rev.json
	stack exec mcqc -- $<

rev: Rev.cpp ../../include/*
	@echo --------------------------------------------------------------------------------
	@echo Compiling $@ ...
	${CXX} ${CXXFLAGS} $< -o $@

prof: rev
	@echo --------------------------------------------------------------------------------
	@echo Profiling $@ ...
	valgrind --tool=massif ./$<

clean:
	-rm -rf rev Rev.cpp *.dSYM *.glob *.vo *.json *.o

