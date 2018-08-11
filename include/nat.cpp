#ifndef NAT_CPP
#define NAT_CPP
#include <climits>
#include <cstdlib>
#include "nat.hpp"
#include "exception.hpp"

namespace nat {

    constexpr inline static Nat succ(Nat a) {
        // Boundary check
        if (a >= UINT_MAX) {
			throw new OverflowException("Out of UINT_MAX limit");
		}
        return a + 1;
    }
    constexpr inline static Nat pred(Nat a) {
        // static_assert(a > 0 , "Out of UINT_MAX limit");
        if (a == 0) {
            return 0; // This is what coq does, but maybe we want to throw
        }
        return a - 1;
    }

    // Utility functions
    // Boolean
    constexpr inline static bool even(Nat a) {
        return ~(a & 1);
    }
    constexpr inline static bool odd(Nat a) {
        return (a & 1);
    }
    // Arithmetic
    constexpr inline static Nat add(Nat a, Nat b) {
        if (UINT_MAX - a < b || UINT_MAX - b < a) {
			throw new OverflowException("add: Out of UINT_MAX limit");
		}
        return a + b;
    }

    constexpr inline static Nat sub(Nat a, Nat b) {
        if (a < b) {
            return 0;
        }
        return a - b;
    }

    constexpr inline static Nat mul(Nat a, Nat b) {
        if (a == 0 || b == 0) {
            return 0;
        } else if (UINT_MAX/a < b || UINT_MAX/b < a) {
			throw new OverflowException("mul: Out of UINT_MAX limit");
        }
		return a * b;
    }

	// total division, does not fail when /0
    constexpr inline static Nat div(Nat a, Nat b) {
        if (b == 0) {
            return 0;
        }
        return a / b;
    }

	// total division, does not fail when /0
    constexpr inline static Nat mod(Nat a, Nat b) {
        if (b == 0) {
            return 0;
        }
        return a % b;
    }
}
#endif
