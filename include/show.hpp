#ifndef SHOW_H
#define SHOW_H
#include <iostream>
#include <future>
#include "string.hpp"
#include "option.hpp"
#include "nat.hpp"
#include "type_checks.h"
#include "tuple.hpp"

using namespace Nat;
using namespace String;
using namespace Option;
using namespace Tuple;

template <class T, class... TArgs> decltype(void(T{std::declval<TArgs>()...}), std::true_type{}) test_is_braces_constructible(int);
template <class, class...> std::false_type test_is_braces_constructible(...);
template <class T, class... TArgs> using is_braces_constructible = decltype(test_is_braces_constructible<T, TArgs...>(0));

namespace Show {

    // nat -> string
    template<typename N=nat>
    typename std::enable_if<is_same_kind_v<N, nat>, string>::type
    show(N&& n) {
        return std::to_string(n);
    }

    // bool -> string
    template<typename B=bool>
    typename std::enable_if<is_same_kind_v<B, bool>, string>::type
    show(B&& b) {
        if (b) { return string("true"); }
        return string("false");;
    }

    // char -> string
    template<typename C=char>
    typename std::enable_if<is_same_kind_v<C, char>, string>::type
    show(C&& c) {
        return string(1, c);
    }

    // string -> string
    template<typename S=string, typename = std::enable_if_t<is_same_kind_v<string, S>>>
    static string show(S&& s) {
        return FWD(s);
    }

    // tuple (product) -> string
    template<class TupType, size_t... I>
    static string show(const TupType& t, std::index_sequence<I...>)
    {
        std::stringstream ss;
        ss<< "(";
        (..., (ss << (I == 0? "" : ", ") << show(std::get<I>(t))));
        ss << ")";
        return ss.str();
    }
    template<class ...Args>
    static string show(const tuple<Args...>& t)
    {
        return show(t, std::make_index_sequence<sizeof...(Args)>());
    }

    // Forward declaration
    template<class T>
    static string show_struct(T&& object) noexcept;

    // pointer (sum type)-> string
    template<class ...Args>
    static string show(std::shared_ptr<std::variant<Args...>> p);

    // pointer (sum type)-> string
    template<class ...Args>
    static string show(std::shared_ptr<std::variant<Args...>> p) {
        return std::visit([](auto&& arg) {
            return show_struct(FWD(arg));
        }, *p);
    }

    // Guard for structs
    template<class T>
    static string show(T&& t) noexcept {
        return show_struct(FWD(t));
    }

    // simple struct reflection
    struct any_type {
        template<class T>
        constexpr operator T(); // non explicit
    };

    // product type -> string
    template<class T>
    static string show_struct(T&& object) noexcept {
        using type = std::decay_t<T>;
        std::stringstream ss;
        if constexpr(is_braces_constructible<type, any_type, any_type, any_type, any_type, any_type, any_type>{}) {
            auto&& [p1, p2, p3, p4, p5, p6] = object;
            ss << SHOWTEMP(type) << " " << show(p1) << " " << show(p2) << " " << show(p3) << " " << show(p4) << " " << show(p5) << " " << show(p6);
        } else if constexpr(is_braces_constructible<type, any_type, any_type, any_type, any_type, any_type>{}) {
            auto&& [p1, p2, p3, p4, p5] = object;
            ss << SHOWTEMP(type) << " " << show(p1) << " " << show(p2) << " " << show(p3) << " " << show(p4) << " " << show(p5);
        } else if constexpr(is_braces_constructible<type, any_type, any_type, any_type, any_type>{}) {
            auto&& [p1, p2, p3, p4] = object;
            ss << SHOWTEMP(type) << " " << show(p1) << " " << show(p2) << " " << show(p3) << " " << show(p4);
        } else if constexpr(is_braces_constructible<type, any_type, any_type, any_type>{}) {
            auto&& [p1, p2, p3] = object;
            ss << SHOWTEMP(type) << " " << show(p1) << " " << show(p2) << " " << show(p3);
        } else if constexpr(is_braces_constructible<type, any_type, any_type>{}) {
            auto&& [p1, p2] = object;
            ss << SHOWTEMP(type) << " " << show(p1) << " " << show(p2);
        } else if constexpr(is_braces_constructible<type, any_type>{}) {
            auto&& [p1] = object;
            ss << SHOWTEMP(type) << " " << show(p1);
        } else {
            ss << SHOWTEMP(type);
        }
        return ss.str();
    }

}
#endif
