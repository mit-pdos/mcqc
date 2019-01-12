.PHONY=all

CXX=clang++
CXXFLAGS?=-g -O3 -fno-stack-protector -pedantic -Wno-switch-bool -std=c++17 -I../../include

all: hello

Hello.json: Hello.v
	coqc -Q ../../classes "" $<
Hello.cpp: Hello.json
	stack exec machcoq -- $<
hello: Hello.cpp
	${CXX} ${CXXFLAGS} $< -o $@

clean:
	-rm -rf hello *.cpp *.dSYM *.glob *.vo *.json *.o

