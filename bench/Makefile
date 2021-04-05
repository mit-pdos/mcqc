all: massif bench benchfib benchfact

ifndef NUM
    NUM := 2000
endif
ifndef FNUM
    FNUM := 30
endif
ifndef FACTNUM
    FACTNUM := 8
endif

Sort.hs: Sort.v
	coqc -Q ../classes Mcqc $<
Rev.hs: Rev.v
	coqc -Q ../classes Mcqc $<
Fib.hs: Fib.v
	coqc -Q ../classes Mcqc $<
Fact.hs: Fact.v
	coqc -Q ../classes Mcqc $<

# Compilers
CXX=clang++
CXXFLAGS?=-O3 -fno-stack-protector -Wno-switch-bool -std=c++17 -I../include
HXX=ghc
HXXFLAGS?=-O3 -prof -fprof-auto -rtsopts
# VALFLAGS?=--tool=massif --stacks=yes --massif-out-file
VALFLAGS?=--tool=massif --massif-out-file

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
hfact: MainFact.hs Fact.hs
	${HXX} ${HXXFLAGS} $< -o $@
cfact: Fact.cpp
	${CXX} ${CXXFLAGS} $< -o $@

# Memory benchmark
csort.massif: csort
	valgrind ${VALFLAGS}=$@ ./$< $(NUM)
crev.massif: crev
	valgrind ${VALFLAGS}=$@ ./$< $(NUM)
massif: csort.massif crev.massif

# Time benchmarks
bench: csort hsort crev hrev
	for bin in $^; do \
	    time ./$$bin $(NUM) > /dev/null; \
	done

benchfib: cfib hfib
	for bin in $^; do \
	    time ./$$bin $(FNUM) > /dev/null; \
	done
benchfact: cfact hfact
	for bin in $^; do \
	    time ./$$bin $(FACTNUM) > /dev/null; \
	done

clean:
	-rm -r *.dSYM
	-rm ?fact ?fib ?sort ?rev *.massif *.json *.glob *.vo *.vok *.vos *.aux *.hi *.mli *.o Fact.hs Fib.hs Nat.hs Rev.hs Sort.hs Datatypes.hs Mergesort.hs
