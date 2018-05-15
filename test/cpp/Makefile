.PHONY=all

CPP=clang++-6.0
CCFLAGS?=-Wall -O3 -DNDEBUG -std=gnu++17

all: visit test curry

visit: visit.cpp
	${CPP} ${CCFLAGS} $< -o $@

curry: curry.cpp
	${CPP} ${CCFLAGS} $< -o $@

test: test.cpp terms.hpp proc.hpp
	${CPP} ${CCFLAGS} $< -o $@

clean:
	rm -f *.o *.gch visit variadic test curry

