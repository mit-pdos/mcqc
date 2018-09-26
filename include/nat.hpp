#ifndef NAT_H
#define NAT_H
#include <climits>
#include <type_traits>
#include "exception.h"
#include "type_checks.h"

namespace Nat {
    // nat definition
    using nat = unsigned int;

    // Pattern matching nat
    template<typename Func, typename Func2, typename Ret = std::invoke_result_t<Func>,
             typename = std::enable_if_t<CallableWith<Func>          && "1st argument not callable with void">,
             typename = std::enable_if_t<CallableWith<Func2, nat>    && "2nd argument not callable with nat">,
		     typename = std::enable_if_t<std::is_same_v<Ret, std::invoke_result_t<Func2, nat>> && "Arg function return types must match">>
    constexpr Ret match(nat a, Func f, Func2 g){
        switch(a) {
        case 0:  return f();     // Call function with no argument
        default: return g(a-1);  // Call function with m, where S m = a
        }
    }

	// Successor function (adds one)
    constexpr nat succ(nat a) {
        // Boundary check
        if (a >= UINT_MAX) {
			throw new OverflowException("Out of UINT_MAX limit");
		}
        return a + 1;
    }

	// Predecessor function (minus one, total)
    constexpr nat pred(nat a) {
        // static_assert(a > 0 , "Out of UINT_MAX limit");
        if (a == 0) {
            return 0; // This is what coq does, but maybe we want to throw
        }
        return a - 1;
    }

    // Utility functions
    // Boolean
    constexpr bool even(nat a) {
        return ~(a & 1);
    }
    constexpr bool odd(nat a) {
        return (a & 1);
    }

    // Arithmetic
	// Add
    constexpr nat add(nat a, nat b) {
        if (UINT_MAX - a < b || UINT_MAX - b < a) {
			throw new OverflowException("add: Out of UINT_MAX limit");
		}
        return a + b;
    }

	// Subtract
    constexpr nat sub(nat a, nat b) {
        if (a < b) {
            return 0;
        }
        return a - b;
    }

	// Multiply
    constexpr nat mul(nat a, nat b) {
        if (a == 0 || b == 0) {
            return 0;
        } else if (UINT_MAX/a < b || UINT_MAX/b < a) {
			throw new OverflowException("mul: Out of UINT_MAX limit");
        }
		return a * b;
    }

	// total division, does not fail when /0
    constexpr nat div(nat a, nat b) {
        if (b == 0) {
            return 0;
        }
        return a / b;
    }

	// total division, does not fail when /0
    constexpr nat mod(nat a, nat b) {
        if (b == 0) {
            return 0;
        }
        return a % b;
    }
}
#endif
