#ifndef NAT_H
#define NAT_H
#include <type_traits>
#include "type_checks.h"

namespace nat {
    // Nat definition
    using Nat = const unsigned int;

    // Matching unsigned int -> Nat
	template<typename Func, typename Func2, typename Ret = std::invoke_result_t<Func>>
    constexpr inline static const Ret match(Nat a, Func f, Func2 g){
        static_assert(CallableWith<Func>, "1st argument not callable with void");
        static_assert(CallableWith<Func2, Nat>, "2nd argument not callable with Nat");
        static_assert(std::is_same<std::invoke_result_t<Func>, std::invoke_result_t<Func2, Nat>>::value, "Arg function return types must match");
        switch(a) {
        case 0:  return f();     // Call function with no argument
        default: return g(a-1);  // Call function with m, where S m = a
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
