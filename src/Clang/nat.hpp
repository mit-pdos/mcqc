#ifndef NAT_H
#define NAT_H
#include <tuple>

namespace nat {
	// Nat definition
	enum NatAtom { O, S };
	using Nat = const unsigned int;
	using MatchTuple = const std::tuple<NatAtom, Nat>;


	// Matching unsigned int -> Nat
	constexpr MatchTuple match(Nat a){
		switch(a) {
		case 0: return {O, 0};
		default: return {S, a-1};
		}
	}

	constexpr inline static Nat succ(Nat a);
	constexpr inline static Nat pred(Nat a);

	// Utility functions
    // Boolean
    constexpr inline static bool even(Nat a);
    constexpr inline static bool odd(Nat a);

    /// Arithmetic
	constexpr inline static Nat add(Nat a, Nat b);
	constexpr inline static Nat sub(Nat a, Nat b);
	constexpr inline static Nat mul(Nat a, Nat b);
	constexpr inline static Nat div(Nat a, Nat b);
	constexpr inline static Nat mod(Nat a, Nat b);
}

#endif
