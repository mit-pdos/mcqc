all: bench benchfib

ifndef NUM
    NUM := 2000
endif
ifndef FNUM
    FNUM := 39
endif

Sort.hs: Sort.v
	coqc -Q ../classes "" $<
Rev.hs: Rev.v
	coqc -Q ../classes "" $<
Fib.hs: Fib.v
	coqc -Q ../classes "" $<

# Compilers
CXX=clang++
CXXFLAGS?=-O3 -fno-stack-protector -Wno-switch-bool -std=c++17 -I../include
HXX=ghc
HXXFLAGS?=-O3

# Binaries
csort: Sort.cpp
	${CXX} ${CXXFLAGS} $< -o $@
hsort: MainSort.hs Sort.hs
	${HXX} ${HXXFLAGS} $< -o $@
crev: Rev.cpp
	${CXX} ${CXXFLAGS} $< -o $@
hrev: MainRev.hs Rev.hs
	${HXX} ${HXXFLAGS} $< -o $@
cfib: Fib.cpp
	${CXX} ${CXXFLAGS} $< -o $@
hfib: MainFib.hs Fib.hs
	${HXX} ${HXXFLAGS} $< -o $@

bench: csort hsort crev hrev
	for bin in $^; do \
	    time ./$$bin $(NUM) > /dev/null; \
	done

benchfib: cfib hfib
	for bin in $^; do \
	    time ./$$bin $(FNUM) > /dev/null; \
	done

clean:
	-rm ?fib ?sort ?rev *.json *.glob *.vo *.hi *.mli *.o Fib.hs Rev.hs Nat.hs Sort.hs Datatypes.hs Mergesort.hs
