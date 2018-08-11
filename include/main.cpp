#include <iostream>
#include <ctime>
#include "optional.cpp"
#include "list.cpp"
#include "proc.cpp"
#include "tuple.cpp"
#include "benchmark.h"

#define BMAX 1000

using namespace optional;
using namespace nat;
using namespace list;
using namespace tuple;

template<typename T>
static inline List<T> rev(List<T> l) {
	return list::match(l,
		[]()	{ return List<T>(); },
		[](auto h, auto ls) { return app(rev(ls), List<T>(1, h)); });
}

int main() {
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

	// Lists with initializer_list
	proc::print(rev(List<int>{}));

	// Lists benchmark
	List<int> bar;
	for(int i = 0; i < BMAX; ++i)
		bar.push_back(i);
	tic();
	proc::print(rev(bar));
	toc();

    // Tuples
    auto t = mkTuple(1, 'b', "foo");
	match(t,
		[](int a, char b, const char* c) {
			std::cout << "Tuple expanded: " << a << ", " << b << ", " << c << std::endl; });
	std::cout << "snd(t): " << snd(t) << std::endl;
    return 0;
}

