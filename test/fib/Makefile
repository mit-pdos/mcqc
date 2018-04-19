.PHONY=all

CPP=clang++-3.8
#CCFLAGS?=-g -Wall -O3 -DNDEBUG -I../../Mach7/code -pedantic -DXTL_MESSAGE_ENABLED=0 -std=gnu++0x
CXXFLAGS?=-g -Wall -O3 -DNDEBUG -I../../Mach7/code -pedantic -std=gnu++0x

all: fib

fib: fib.o
	${CPP} ${CXXFLAGS} $^ -o $@

%.o: %.cpp *.hpp patterns/*.hpp
	@echo --------------------------------------------------------------------------------
	@echo Compiling $@ ...
	$(CXX) $(CXXFLAGS) -o $@ -c $<


clean:
	rm -f *.glob *.vo *.json *.o visit

