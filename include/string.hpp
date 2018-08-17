#ifndef STRING_H
#define STRING_H
#include <string>
#include <iostream>
#include "type_checks.h"
#include "nat.hpp"
#include "optional.hpp"

namespace string {
    using String = std::string;

    // Destructive match
    template<typename S=String, typename Func, typename Func2, 
             typename Ret = std::invoke_result_t<Func>,
             typename = std::enable_if_t<is_same_kind_v<S, String>>>
    static const Ret match(S&& l, Func f, Func2 g) {
        static_assert(CallableWith<Func>, "1st argument not callable with void");
        static_assert(CallableWith<Func2, char, String>, "2nd argument not callable with (char, String)");
        static_assert(std::is_same<std::invoke_result_t<Func>, std::invoke_result_t<Func2, char, String>>::value, "Arg function return types must match");
        static_assert(is_same_kind_v<S, String> == true);
        switch(l.empty()) {
        case true:  return f();
        case false: {
            auto head = l.begin();
            return g(*head, String(head++, l.end()));
        }
        }
    }
    // Destructive concatenation with char
    template<typename S=String, typename = std::enable_if_t<is_same_kind_v<S, String>>>
    static String cons(S&& l, char h) {
        l.insert(l.begin(), h);
        return l;
    }

    // Head: first character
    template<typename S=String, typename = std::enable_if_t<is_same_kind_v<String, S>>>
    static char head(S&& l) {
        return std::forward<S>(l)[0];
    }

    // Destructive tail: l is considered immutable and will be copied safely
    template<typename S=String, typename = std::enable_if_t<is_same_kind_v<String, S>>>
    static String tail(S&& l) {
        l.remove(0);
        return std::forward<S>(l);
    }
    // Destructive append
    template<typename L, typename R,
    		 typename = std::enable_if_t<is_same_kind_v<String, L> && is_same_kind_v<String, R>>>
    static String append(L&& l, R&& r) {
        l.append(std::forward<R>(r));
        return std::forward<L>(l);
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
