.PHONY=all

CPP=clang++-6.0
CCFLAGS?=-Wall -O3 -DNDEBUG -std=gnu++17

all: visit

visit: visit.cpp
	${CPP} ${CCFLAGS} $^ -o $@

clean:
	rm -f *.o visit

