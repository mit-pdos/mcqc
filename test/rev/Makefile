.PHONY=all

CXX=clang++
CXXFLAGS?=-g -O3 -fno-stack-protector -pedantic -Wno-switch-bool -std=c++17 -I../../include

all: rev

Rev.json: Rev.v
	coqc $<

Rev.cpp: Rev.json
	cd ../../ && ./dist/build/machcoq/machcoq test/rev/$<
	mv ../../Rev.cpp $@

rev: main.cpp Rev.cpp
	@echo --------------------------------------------------------------------------------
	@echo Compiling $@ ...
	${CXX} ${CXXFLAGS} $< -o $@


clean:
	-rm -rf rev Rev.cpp *.dSYM *.glob *.vo *.json *.o

