#ifndef STRING_H
#define STRING_H
#include <tuple>
#include <string>
#include <iostream>
#include "type_checks.h"
#include "nat.hpp"

namespace string {
    // String definition
    enum StringAtom { Nil, Cons };

    using Char = char;
    using String = std::string;

    // Constructive match, l is considered immutable and will be copied safely
    template<typename Func, typename Func2, typename Ret = std::invoke_result_t<Func>>
	inline static const Ret match(String l, Func f, Func2 g) {
        static_assert(CallableWith<Func>, "1st argument not callable with void");
        static_assert(CallableWith<Func2, Char, String>, "2nd argument not callable with (Char, String)");
        static_assert(std::is_same<std::invoke_result_t<Func>, std::invoke_result_t<Func2, Char, String>>::value, "Arg function return types must match");

        switch(l.empty()) {
        case true:  return f();
        case false: {
            auto head = l.begin();
            return g(*head, String(head++, l.end()));
        }
        }
    }

    // Constructive cons, copies l so l can be referenced again
    inline static String cons(String l, Char h);

    // Utility functions
    // Head
    inline static Char head(String l);

    // Constructive tail, l is considered immutable and will be copied safely
    inline static String tail(String l);

    // Fully constructive app, both l1, l2 are immutable and will be copied.
    inline static String append(String l1, String l2);

    // Boolean
    inline static bool empty(String l);
    inline static bool in(String l, Char t);

    /// Arithmetic
    inline static nat::Nat length(String l);
}
#endif
