.PHONY=all

CXX=clang++
CXXFLAGS?=-g -O3 -fno-stack-protector -pedantic -Wno-switch-bool -std=c++17 -I../../include

all: fib

Fib.hs: Fib.v
	coqc $<

fib: Main.hs Fib.hs
	ghc -O3 $< -o $@

clean:
	-rm -rf fib Datatypes.hs Fib.hs Nat.hs *.hi *.dSYM *.glob *.vo *.json *.o

