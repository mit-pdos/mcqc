#include <iostream>
#include <ctime>
#include "optional.cpp"
#include "nat.cpp"
#include "list.hpp"
#include "list.cpp"
#include "proc.hpp"
#include "benchmark.h"

#define BMAX 1000

using namespace optional;
using namespace nat;
using namespace list;

// Functional fibonacci
static inline constexpr Nat fib(Nat &a) {
	return match(a,
		[]()	  { return (Nat)1; },
	    [](Nat sm) {
		  return match(sm,
			[]()       { return (Nat)1; },
			[sm](Nat m) { return add(fib(m), fib(sm)); });
		});
}

// Reverse a list
template<typename T>
static inline List<T> rev(List<T> &l) {
	return dmatch(l,
		[]()	{ return List<T>(); },
		[](T h, List<T> ls) { return dapp(rev(ls), List<T>(1, h)); });
}

int main() {

	// For benchmarks
	time_t tstart, tend;

	// Optionals
    Optional<int> o = Optional<int>(42);
	// Switch 1
	match(o,
		[]()      { std::cout << "Empty" << std::endl; },
		[](int m) { std::cout << "Some " << m << std::endl; });

	// Switch 1
    auto a = none<int>();
	match(a,
		[]()	  { std::cout << "Empty" << std::endl; },
		[](int m) { std::cout << "Some " << m << std::endl; });

	// Lists
	int myints[] = {16,2,77,29, 32, 18, 23, 232, 23, 23, 54, 12, 1};
	List<int> foo (myints, myints + sizeof(myints) / sizeof(int) );
	proc::print(foo);
	proc::print(rev(foo));

	// Lists benchmark
	List<int> bar;
	for(int i = 0; i < BMAX; ++i)
		bar.push_back(i);
	proc::print(bar);
	tic();
	proc::print(rev(bar));
	toc();

	// Nats
	std::cout << fib(20) << std::endl;
    return 0;
}

