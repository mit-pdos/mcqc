#ifndef SHOW_H
#define SHOW_H
#include <iostream>
#include <variant>
#include "pair.hpp"
#include "option.hpp"
#include "string.hpp"
#include "nat.hpp"
#include "type_checks.h"

using namespace Nat;
using namespace String;
using namespace Pair;
using namespace Option;

template <class T, class... TArgs> decltype(void(T{std::declval<TArgs>()...}), std::true_type{}) test_is_braces_constructible(int);
template <class, class...> std::false_type test_is_braces_constructible(...);
template <class T, class... TArgs> using is_braces_constructible = decltype(test_is_braces_constructible<T, TArgs...>(0));

namespace Show {

    // nat -> string
    static inline string show(nat n) {
        return std::to_string(n);
    }

    // bool -> string
    static inline string show(bool b) {
        if (b) { return string("true"); }
        return string("false");;
    }

    // char -> string
    static inline string show(char c) {
        return string(1, c);
    }

    // string -> string
    static inline string show(string s) {
        return FWD(s);
    }

    // option -> string
    template<class T>
    static inline string show(option<T> o) {
        std::stringstream ss;
        if (o.has_value()) {
            ss << "Some " << show(o.value());
        }
        else {
            ss << "None";
        }
        return ss.str();
    }

    // pair (product) -> string
    template<class A, class B>
    static inline string show(pair<A,B> p) {
        std::stringstream ss;
        ss << "(" << p.first << ", " << p.second << ")";
        return ss.str();
    }

    // Forward declaration
    template<class T>
    static inline string show_struct(T&& object) noexcept;

    // pointer (sum type)-> string
    template<class ...Args>
    static inline string show(std::shared_ptr<std::variant<Args...>> p);

    // pointer (sum type)-> string
    template<class ...Args>
    static inline string show(std::shared_ptr<std::variant<Args...>> p) {
        return std::visit([](auto&& arg) {
            return show_struct(FWD(arg));
        }, *p);
    }

    // Guard for structs
    template<class T>
    static inline string show(T&& t) noexcept {
        return show_struct(FWD(t));
    }

    // simple struct reflection
    struct any_type {
        template<class T>
        constexpr operator T(); // non explicit
    };

    // product type -> string
    template<class T>
    static inline string show_struct(T&& object) noexcept {
        using type = std::decay_t<T>;
        std::stringstream ss;

        if constexpr(is_braces_constructible<type, any_type, any_type, any_type, any_type, any_type, any_type>{}) {
            auto&& [p1, p2, p3, p4, p5, p6] = object;
            ss << show(p1) << " " << show(p2) << " " << show(p3) << " " << show(p4) << " " << show(p5) << " " << show(p6);
        } else if constexpr(is_braces_constructible<type, any_type, any_type, any_type, any_type, any_type>{}) {
            auto&& [p1, p2, p3, p4, p5] = object;
            ss << show(p1) << " " << show(p2) << " " << show(p3) << " " << show(p4) << " " << show(p5);
        } else if constexpr(is_braces_constructible<type, any_type, any_type, any_type, any_type>{}) {
            auto&& [p1, p2, p3, p4] = object;
            ss << show(p1) << " " << show(p2) << " " << show(p3) << " " << show(p4);
        } else if constexpr(is_braces_constructible<type, any_type, any_type, any_type>{}) {
            auto&& [p1, p2, p3] = object;
            ss << show(p1) << " " << show(p2) << " " << show(p3);
        } else if constexpr(is_braces_constructible<type, any_type, any_type>{}) {
            auto&& [p1, p2] = object;
            ss << show(p1) << " " << show(p2);
        } else if constexpr(is_braces_constructible<type, any_type>{}) {
            auto&& [p1] = object;
            ss << show(p1);
        } else {
            ss << "Nil";
        }
        return ss.str();
    }

}
#endif
