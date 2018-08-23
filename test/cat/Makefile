.PHONY=all

CXX=clang++
CXXFLAGS?=-g -O3 -fno-stack-protector -pedantic -Wno-switch-bool -std=c++17 -I../../include

all: cat

Cat.json: Cat.v
	coqc $<

Cat.cpp: Cat.json
	stack exec machcoq -- $<

cat: main.cpp Cat.cpp ../../include/*
	@echo --------------------------------------------------------------------------------
	@echo Compiling $@ ...
	${CXX} ${CXXFLAGS} $< -o $@


clean:
	-rm -rf cat Cat.cpp *.dSYM *.glob *.vo *.json *.o

