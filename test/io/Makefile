.PHONY=all

CXX=clang++
CXXFLAGS?=-g -O3 -fno-stack-protector -pedantic -Wno-switch-bool -std=c++17 -I../../include

all: hello.exe io.exe

Hello.json: Hello.v
	coqc -Q ../../classes "" $<
Hello.cpp: Hello.json
	stack exec mcqc -- $<
hello.exe: Hello.cpp
	${CXX} ${CXXFLAGS} $< -o $@

InputOutput.json: InputOutput.v
	coqc -Q ../../classes "" $<
InputOutput.cpp: InputOutput.json
	stack exec mcqc -- $<
io.exe: InputOutput.cpp
	${CXX} ${CXXFLAGS} $< -o $@

clean:
	-rm -rf hello.exe *.cpp *.dSYM *.glob *.vo *.json *.o
	-rm -rf io.exe *.cpp *.dSYM *.glob *.vo *.json *.o

