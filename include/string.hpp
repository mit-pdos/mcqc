#ifndef STRING_H
#define STRING_H
#include <string>
#include <iostream>
#include <sstream>
#include "type_checks.h"
#include "nat.hpp"
#include "option.hpp"
#include "list.hpp"

using namespace Nat;
using namespace Option;
using namespace List;

namespace String {
    // type allias for std::string
    using string = std::string;

    // Constructive match
    template<typename S=string, typename Func, typename Func2,
             typename Ret = std::invoke_result_t<Func>,
             typename = std::enable_if_t<is_same_kind_v<S, string>
                && "Only match on string types">,
             typename = std::enable_if_t<CallableWith<Func>
                && "1st argument not nullary function">,
             typename = std::enable_if_t<CallableWith<Func2, char, S>
                && "2nd argument not callable with (char, string)">,
             typename = std::enable_if_t<std::is_same_v<Ret, std::invoke_result_t<Func>>
                && "Argument function return types must match">>
    static const Ret match(S&& s, Func f, Func2 g) {
        if(s.empty()) {
            return f();
        } else {
            char head = *s.begin();
            s.erase(s.begin());
            return g(FWD(head), s);
        }
    }

    // Destructive ctor, modifies l and appends an element
    template<typename L=string,
             typename = std::enable_if_t<is_same_kind_v<string, L>>>
    static string& mkstring(char c, L&& l) {
        if constexpr (is_constr_v<L>) {
            auto cp = std::make_unique<string>(l);
            cp->insert(l.begin(), 1, c);
            return *cp;
        } else {
            l.insert(l.begin(), 1, c);
            return l;
        }
    }

    // Constructive append, creates string
    template<typename L=string, typename R=string,
             typename = std::enable_if_t<is_same_kind_v<string, L>>,
             typename = std::enable_if_t<is_same_kind_v<string, R>>>
    static string& append(L&& l, R&& r) noexcept {
        // If constant, create a unique ptr
        if constexpr (is_constr_v<L> && is_constr_v<R>) {
            auto cp = std::make_unique<string>(l);
            cp->insert(l.end(), r.begin(), r.end());
            return *cp;
        } else if constexpr (is_constr_v<L>) {
            r.insert(r.begin(), l.begin(), l.end());
            return r;
        } else {
            l.insert(l.end(), r.begin(), r.end());
            return l;
        }
    }

    // Get characater in given index of string
    template<typename S=string,
             typename = std::enable_if_t<is_same_kind_v<string, S>>>
    static option<char> get(nat index, S&& s) noexcept {
        if (index < s.length()) {
            return some<char>(s[index]);
        }
        return none<char>();
    }

    // Constructive, get substring based on begin index and length
    template<typename S=string,
             typename = std::enable_if_t<is_same_kind_v<string, S>>>
    static string substring(nat begin, nat len, S&& s) noexcept {
        if (begin < s.length() && (begin + len) <= s.length()) {
            // Copy inside substr
            return s.substr(begin, len);
        }
        return string();
    }

     // Constructive, concat a list of strings around the given separator
    template<typename S=string, typename L,
             typename = std::enable_if_t<is_same_kind_v<string, S>>,
             typename = std::enable_if_t<is_same_kind_v<list<string>, L>>>
    static string concat(S&& sep, L&& l) noexcept {
        std::stringstream ss;
        for (auto it = l.begin(); it != l.end(); ++it) {
            if (it != l.begin()) {
                ss << FWD(sep);
            }
            ss << *it;
        }
        return ss.str();
    }

    // Opposite of concat, split a string (constructive obviously)
    template<typename S=string,
             typename = std::enable_if_t<is_same_kind_v<string, S>>>
    static list<string> split(S&& s, char c = ' ') {
        std::stringstream ss(s);
        string buf;

        list<string> l = list<string>{};
        while (getline(ss, buf, c)) {
            l.push_back(buf);
        }
        return l;
    }

    // Is the given string a prefix of the second string
    template<typename S=string,
             typename L=string,
             typename = std::enable_if_t<is_same_kind_v<string, S>>,
             typename = std::enable_if_t<is_same_kind_v<string, L>>>
    static bool prefix(S&& s, L&& l) {
        // If prefix is longer than string, cannot be prefix
        if (l.length() < s.length()) {
            return false;
        }
        // Else compare
        for (auto its = s.begin(), itl = l.begin(); its != s.end(); ++its, ++itl) {
            if (*its != *itl) {
                return false;
            }
        }
        return true;
    }

    /// Arithmetic
    template<typename S=string,
             typename = std::enable_if_t<is_same_kind_v<string, S>>>
    static nat size(S&& l) {
        return static_cast<nat>(l.length());
    }
}
#endif
