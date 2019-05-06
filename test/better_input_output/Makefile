.PHONY: all clean run

CXX=clang++
CXXFLAGS?=-g -O3 -fno-stack-protector -pedantic -Wno-switch-bool -std=c++17 -I../../include

all: InputOutput.exe

InputOutput.json: InputOutput.v
	coqc -Q ../../classes "" $<

InputOutput.cpp: InputOutput.json
	stack exec mcqc -- $<

InputOutput.exe: main.cpp InputOutput.cpp ../../include/*
	@echo --------------------------------------------------------------------------------
	@echo Compiling $@ ...
	${CXX} ${CXXFLAGS} $< -o $@


clean:
	-rm -rf InputOutput.exe InputOutput.cpp *.dSYM *.glob *.vo *.json *.o

run: InputOutput.exe
	./InputOutput.exe input.txt
