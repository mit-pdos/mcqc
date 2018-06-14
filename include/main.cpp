#include <iostream>
#include "optional.cpp"
#include "nat.cpp"

using namespace optional;
using namespace nat;

static inline constexpr Nat fib2(Nat a) {
	switch(a) {
    case 0:  return (Nat)1;     // Call function with no argument
    default: switch(a-1) {    // Call function with m, where S m = a
		case 0:  return (Nat)1;
	    default: return add(fib2(a-1), fib2(a-2));
		}
    }
}


// Functional fibonacci
static inline constexpr Nat fib(Nat a) {
	return match(a,
		[]()	  { return (Nat)1; },
	    [](Nat sm) {
		  return match(sm,
			[]()       { return (Nat)1; },
			[sm](Nat m) { return add(fib(m), fib(sm)); });
		});
}


int main() {

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

	std::cout << fib(42) << std::endl;
    return 0;
}

