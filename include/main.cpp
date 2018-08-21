#include <iostream>
#include <ctime>
#include <cassert>
#include "optional.hpp"
#include "list.hpp"
#include "bool.hpp"
#include "proc.hpp"
#include "tuple.hpp"
#include "string.hpp"
#include "curry.h"
#include "benchmark.h"
#include "type_checks.h"

#define BMAX 1000

using namespace optional;
using namespace nat;
using namespace list;
using namespace string;
using namespace tuple;
using namespace boolean;

template<typename T>
static inline List<T> rev(List<T> l) {
    return list::match(l,
        []()    { return List<T>(); },
        [](auto h, auto ls) { return app(rev(ls), List<T>(1, h)); });
}

int main() {

	// Currying
  	auto f = [](auto a, auto b, auto c, auto d) {
    	return a  * b * c * d;
  	};
  	std::cout << "Currying 4! = " << curry(f)(1)(2)(3)(4) << std::endl;
    // Optional Switch
    match(some(42),
        [](int m) { std::cout << "Some " << m << std::endl; },
        []()      { std::cout << "Empty" << std::endl; });

    // Bools
	match((bool)false,
		[]() { throw IOException("Should never be matched, since bool is false"); },
		[]() { std::cout << "Bool matching works" << std::endl; });

    // Lists with initializer_list
    proc::print(rev(List<int>{}));

    // Lists benchmark
    List<int> bar;
    for(int i = 0; i < BMAX; ++i)
        bar.push_back(i);
    tic();
    assert(rev(rev(bar)) == bar);
    toc();

    // Strings
	match(append(String("foo"), String("bar")),
		[]() { throw IOException("Should never be matched, since the str is non-empty"); },
		[](auto h, auto ts) { 
			assert(h == 'f' && ts == "oobar"); 
			std::cout << "String matching works" << std::endl;
		});

	String foo = "foo";
	String baz = "baz";
	proc::print(append(String(foo),baz));
	proc::print(append(String(foo),baz));
	proc::print(foo);

    // Tuples
    auto t = mkTuple(1, 'b', "foo");
    match(t,
        [](int a, char b, const char* c) {
            std::cout << "Tuple expanded: " << a << ", " << b << ", " << c << std::endl; 
        });
    std::cout << "snd(t): " << snd(t) << std::endl;
    std::cout << "fst(t): "; proc::print(fst(t));
    std::cout << "fst(fst(t)): "; proc::print(fst(fst(t)));
    return 0;
}

