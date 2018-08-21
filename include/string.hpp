#ifndef STRING_H
#define STRING_H
#include <string>
#include <iostream>
#include "type_checks.h"
#include "nat.hpp"
#include "optional.hpp"

namespace string {
    using String = std::string;

    // Constructive match
    template<typename S=String, typename Func, typename Func2, 
             typename Ret = std::invoke_result_t<Func>,
             typename = std::enable_if_t<is_same_kind_v<S, String> && "Only match on string types">,
             typename = std::enable_if_t<CallableWith<Func>        && "1st argument not callable with void">,
             typename = std::enable_if_t<CallableWith<Func2, char, S> && "2nd argument not callable with (char, String)">,
             typename = std::enable_if_t<std::is_same_v<Ret, std::invoke_result_t<Func2, char, S>> && "Arg function return types must match">>
    static const Ret match(S&& s, Func f, Func2 g) {
        switch(s.empty()) {
        case true:  return f();
        default: {
            auto head = FWD(s.begin());
            return g(*head, String(head++, FWD(s.end())));
        }
        }
    }
    // Constructive concatenation with char
    template<typename S=String, typename = std::enable_if_t<is_same_kind_v<S, String>>>
    static String cons(S&& l, char h) {
        String s = String(FWD(l));
        s.insert(s.begin(), h);
        return l;
    }

    // Head: first character
    template<typename S=String, typename = std::enable_if_t<is_same_kind_v<String, S>>>
    static char head(S&& s) {
        return FWD(s[0]);
    }

    // Destructive tail: l is considered immutable and will be copied safely
    template<typename S=String, typename = std::enable_if_t<is_same_kind_v<String, S>>>
    static String tail(S&& l) {
        l.remove(0);
        return std::forward<S>(l);
    }
    // Destructive append
    template<typename L=String, typename R=String,
             typename = std::enable_if_t<is_same_kind_v<String, L> && is_same_kind_v<String, R>>>
    static String append(L&& l, R&& r) {
        String s = String(FWD(l));
        s.append(FWD(r));
        return s;
    }

    // Boolean
    template<typename S=String, typename = std::enable_if_t<is_same_kind_v<String, S>>>
    static bool empty(S&& s) {
        return std::forward<S>(s).empty();
    }

    /// Arithmetic
    template<typename S=String, typename = std::enable_if_t<is_same_kind_v<String, S>>>
    static nat::Nat length(S&& l) {
        return static_cast<nat::Nat>(std::forward<S>(l).length());
    }
}
#endif
