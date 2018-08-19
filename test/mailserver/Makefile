.PHONY=all

CXX=clang++
CXXFLAGS?=-g -O3 -fno-stack-protector -pedantic -Wno-switch-bool -std=c++17 -I../../include

all: cat

Mail.json: Mail.v
	coqc $<

mail.cpp: Mail.json
	stack exec machcoq -- $<

cat: main.cpp mail.cpp
	@echo --------------------------------------------------------------------------------
	@echo Compiling $@ ...
	${CXX} ${CXXFLAGS} $< -o $@


clean:
	-rm -rf cat mail.cpp *.dSYM *.glob *.vo *.json *.o

