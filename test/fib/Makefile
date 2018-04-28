.PHONY=all

CXX=clang++-3.8
CXXFLAGS?=-g -O3 -DNDEBUG -I../../Mach7/code -pedantic -DXTL_MESSAGE_ENABLED=0 -std=gnu++0x

all: fib fib_typ

fib: fib.o
	${CXX} ${CXXFLAGS} $^ -o $@

fib_typ: fib_typ.o
	${CXX} ${CXXFLAGS} $^ -o $@

fib.o: fib.cpp
	@echo --------------------------------------------------------------------------------
	@echo Compiling $@ ...
	$(CXX) $(CXXFLAGS) -o $@ -c $<

fib_typ.o: fib_typ.cpp
	@echo --------------------------------------------------------------------------------
	@echo Compiling $@ ...
	$(CXX) $(CXXFLAGS) -o $@ -c $<


clean:
	rm -f fib *.glob *.vo *.json *.o visit

