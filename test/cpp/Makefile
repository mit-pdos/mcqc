.PHONY=all

CPP=clang++-6.0
CCFLAGS?=-Wall -O3 -DNDEBUG -std=gnu++17

all: visit variadic

visit: visit.cpp
	${CPP} ${CCFLAGS} $< -o $@

variadic: variadic.cpp terms.hpp proc.hpp
	${CPP} ${CCFLAGS} $< -o $@

clean:
	rm -f *.o visit variadic

