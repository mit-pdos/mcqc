#ifndef STRING_H
#define STRING_H
#include <string>
#include <iostream>
#include <sstream>
#include "type_checks.h"
#include "nat.hpp"
#include "optional.hpp"
#include "list.hpp"

namespace string {
    using String = std::string;

    // Constructive match
    template<typename S=String, typename Func, typename Func2,
             typename Ret = std::invoke_result_t<Func>,
             typename = std::enable_if_t<is_same_kind_v<S, String>
				&& "Only match on string types">,
             typename = std::enable_if_t<CallableWith<Func>
				&& "1st argument not nullary function">,
             typename = std::enable_if_t<CallableWith<Func2, char, S>
				&& "2nd argument not callable with (char, String)">,
             typename = std::enable_if_t<std::is_same_v<Ret, std::invoke_result_t<Func2, char, S>>
				&& "Argument function return types must match">>
    static const Ret match(S&& s, Func f, Func2 g) {
        if(s.empty()) {
            return f();
        } else {
            auto head = FWD(s.begin());
            return g(*head, String(head++, FWD(s.end())));
        }
    }

    // Constructive append, creates string
    template<typename L=String, typename R=String,
             typename = std::enable_if_t<is_same_kind_v<String, L>>,
             typename = std::enable_if_t<is_same_kind_v<String, R>>>
    static String append(L&& l, R&& r) {
        String s = String(l);
        s.append(FWD(r));
        return s;
    }

    // Get characater in given index of string
    template<typename S=String,
             typename = std::enable_if_t<is_same_kind_v<String, S>>>
    static optional::Optional<char> get(nat::Nat index, S&& s) {
		if (index < FWD(s.length())) {
			return optional::some(FWD(s[index]));
        }
        return optional::none<char>();
    }

    // Get substring based on begin index and length
    template<typename S=String,
             typename = std::enable_if_t<is_same_kind_v<String, S>>>
    static String substring(nat::Nat begin, nat::Nat len, S&& s) {
		if (begin < FWD(s.length()) && (begin + len) < FWD(s.length())) {
            // Copy inside substr
			return FWD(s.substr(begin, len));
        }
        return String();
    }

	 // Concat a list of strings around the given separator
    template<typename S=String, typename L,
             typename = std::enable_if_t<is_same_kind_v<String, S>>,
             typename = std::enable_if_t<is_same_kind_v<list::List<String>, L>>>
    static String concat(S&& sep, L&& l) {
        std::stringstream ss;
        for (auto it = FWD(l.begin()); it != FWD(l.end()); ++it) {
			if (it != FWD(l.begin())) {
				ss << FWD(sep);
            }
            ss << *it;
        }
        return ss.str();
    }

    // Is the given string a prefix of the second string
    template<typename S=String,
             typename L=String,
             typename = std::enable_if_t<is_same_kind_v<String, S>>,
             typename = std::enable_if_t<is_same_kind_v<String, L>>>
    static bool prefix(S&& s, L&& l) {
        // If prefix is longer than string, cannot be prefix
        if (FWD(l.length()) < FWD(s.length())) {
			return false;
		}
        // Else compare
		for (auto its = FWD(s.begin()), itl = FWD(l.begin()); its != FWD(s.end()); ++its, ++itl) {
			if (*its != *itl) {
				return false;
			}
	    }
        return true;
    }

    /// Arithmetic
    template<typename S=String,
             typename = std::enable_if_t<is_same_kind_v<String, S>>>
    static nat::Nat length(S&& l) {
        return static_cast<nat::Nat>(FWD(l.length()));
    }
}
#endif
