#ifndef NAT_H
#define NAT_H
#include <climits>
#include <type_traits>
#include "exception.h"
#include "type_checks.h"

namespace nat {
    // Nat definition
    using Nat = const unsigned int;

    // Matching unsigned int -> Nat
	template<typename Func, typename Func2, typename Ret = std::invoke_result_t<Func>>
    constexpr Ret match(Nat a, Func f, Func2 g){
        static_assert(CallableWith<Func>, "1st argument not callable with void");
        static_assert(CallableWith<Func2, Nat>, "2nd argument not callable with Nat");
        static_assert(std::is_same<std::invoke_result_t<Func>, std::invoke_result_t<Func2, Nat>>::value, "Arg function return types must match");
        switch(a) {
        case 0:  return f();     // Call function with no argument
        default: return g(a-1);  // Call function with m, where S m = a
        }
    }

	// Successor function (adds one)
    constexpr Nat succ(Nat a) {
        // Boundary check
        if (a >= UINT_MAX) {
			throw new OverflowException("Out of UINT_MAX limit");
		}
        return a + 1;
    }

	// Predecessor function (minus one, total)
    constexpr Nat pred(Nat a) {
        // static_assert(a > 0 , "Out of UINT_MAX limit");
        if (a == 0) {
            return 0; // This is what coq does, but maybe we want to throw
        }
        return a - 1;
    }

    // Utility functions
    // Boolean
    constexpr bool even(Nat a) {
        return ~(a & 1);
    }
    constexpr bool odd(Nat a) {
        return (a & 1);
    }

    // Arithmetic
	// Add
    constexpr Nat add(Nat a, Nat b) {
        if (UINT_MAX - a < b || UINT_MAX - b < a) {
			throw new OverflowException("add: Out of UINT_MAX limit");
		}
        return a + b;
    }

	// Subtract
    constexpr Nat sub(Nat a, Nat b) {
        if (a < b) {
            return 0;
        }
        return a - b;
    }

	// Multiply
    constexpr Nat mul(Nat a, Nat b) {
        if (a == 0 || b == 0) {
            return 0;
        } else if (UINT_MAX/a < b || UINT_MAX/b < a) {
			throw new OverflowException("mul: Out of UINT_MAX limit");
        }
		return a * b;
    }

	// total division, does not fail when /0
    constexpr Nat div(Nat a, Nat b) {
        if (b == 0) {
            return 0;
        }
        return a / b;
    }

	// total division, does not fail when /0
    constexpr Nat mod(Nat a, Nat b) {
        if (b == 0) {
            return 0;
        }
        return a % b;
    }
}
#endif
