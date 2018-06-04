#include <climits>
#include <cstdlib>
#include "nat.hpp"

using namespace std;

namespace nat {

    constexpr inline static Nat succ(Nat a) {
		// Boundary check
		assert(a < UINT_MAX && "Out of UINT_MAX limit");
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
		assert(UINT_MAX - a > b && "Out of UINT_MAX limit");
		assert(UINT_MAX - b > a && "Out of UINT_MAX limit");
		return a + b;
	}
    constexpr inline static Nat sub(Nat a, Nat b) {
		// static_assert(a >= b, "Below 0 for Nat");
		if (a < b) {
			return 0;
		}
		return a - b;
	}
    constexpr inline static Nat mul(Nat a, Nat b) {
		// TODO: Make safe while still constexpr friendly
		return a * b;
	}

    constexpr inline static Nat div(Nat a, Nat b) {
		// assert(b != 0, "Division by zero");
		if (b == 0) {
			return 0;
		}
		return a / b;
	}
    constexpr inline static Nat mod(Nat a, Nat b) {
		// _assert( b != 0, "Division by zero");
		if (b == 0) {
			return 0;
		}
		return a % b;
	}
}

