.PHONY=all

CXX=clang++-3.8
CXXFLAGS?=-g -O3 -DNDEBUG -I../../Mach7/code -pedantic -DXTL_MESSAGE_ENABLED=0 -std=gnu++0x

all: Bind.json

Bind.json: Bind.v
	coqc $^

clean:
	rm -f fib fib_typ  *.glob *.vo *.json *.o visit

