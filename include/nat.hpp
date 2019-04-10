#ifndef NAT_H
#define NAT_H
#include <climits>
#include <type_traits>
#include "exception.h"
#include "type_checks.h"
#include <gmpxx.h>
#include <gmp.h>

namespace Nat {
    // nat definition as bignum
    using nat = mpz_class;

    // Pattern matching nat
    template<typename Func, typename Func2,
             typename = std::enable_if_t<CallableWith<Func>          && "1st argument not callable with void">,
             typename = std::enable_if_t<CallableWith<Func2, nat>    && "2nd argument not callable with nat">>
    static inline auto match(nat a, Func f, Func2 g) {
        if (a == 0) {
            return f();     // Call function with no argument
        } else {
            return g(a-1);  // Call function with m, where S m = a
        }
    }

    // Successor function (adds one)
    static nat succ(nat a) {
        return a + 1;
    }

    // Predecessor function (minus one, total)
    static nat pred(nat a) {
        if (a == 0) {
            return 0;
        }
        return a - 1;
    }

    // Utility functions
    // Boolean
    static bool even(nat a) {
        return mpz_even_p(a.get_mpz_t());
    }
    static bool odd(nat a) {
        return mpz_odd_p(a.get_mpz_t());
    }

    // Arithmetic
    // Add
    static nat add(nat a, nat b) {
        return a + b;
    }

    // Subtract
    static nat sub(nat a, nat b) {
        if (a < b) {
            return 0;
        }
        return a - b;
    }

    // Multiply
    static nat mul(nat a, nat b) {
        return a * b;
    }

    // total division, does not fail when /0
    static nat div(nat a, nat b) {
        if (b == 0) {
            return 0;
        }
        return a / b;
    }

    // total division, does not fail when /0
    static nat modulo(nat a, nat b) {
        if (b == 0) {
            return 0;
        }
        return a % b;
    }
}
#endif
