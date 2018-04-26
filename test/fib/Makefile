.PHONY=all

CXX=clang++-3.8
CXXFLAGS?=-g -O3 -DNDEBUG -I../../Mach7/code -pedantic -DXTL_MESSAGE_ENABLED=0 -std=gnu++0x

all: fib

fib: fib.o
	${CXX} ${CXXFLAGS} $^ -o $@

%.o: %.cpp *.hpp patterns/*.hpp
	@echo --------------------------------------------------------------------------------
	@echo Compiling $@ ...
	$(CXX) $(CXXFLAGS) -o $@ -c $<


clean:
	rm -f fib *.glob *.vo *.json *.o visit

