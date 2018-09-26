#ifndef SHOW_H
#define SHOW_H
#include <iostream>
#include <future>
#include "string.hpp"
#include "list.hpp"
#include "optional.hpp"
#include "nat.hpp"
#include "type_checks.h"
#include "tuple.hpp"

using namespace nat;
using namespace string;
using namespace list;
using namespace optional;

namespace shows {

    // Nat -> String
    static inline String show(nat::Nat n) {
        return std::to_string(n);
    }

    // String -> String
    template<typename S=String, typename = std::enable_if_t<is_same_kind_v<String, S>>>
    static String show(S&& s) {
        return FWD(s);
    }

    // List<T> -> String
    template<typename L, typename T = typename std::remove_reference_t<L>::value_type>
	typename std::enable_if<is_same_kind_v<L, List<T>>, String>::type
	show(L&& l) {
        std::stringstream ss;
        ss << "[";
        for(auto i = FWD(l.begin()); i != FWD(l.end()); ++i) {
            if (i != l.begin())
                ss << ", ";
            ss << *i;
        }
        ss << "]";
		return ss.str();
    }

    // Optional<T> -> String
    template<typename O, typename T = typename std::remove_reference_t<O>::value_type>
    typename std::enable_if<is_same_kind_v<O, Optional<T>>, String>::type
    show(O&& o) {
        if (o.has_value()) {
            return append(String("Some "), show(o.value()));
        }
        return String("None");
    }

    // tuple -> String
    template<class TupType, size_t... I>
    static String show(const TupType& t, std::index_sequence<I...>)
    {
        std::stringstream ss;
        ss<< "(";
        (..., (ss << (I == 0? "" : ", ") << std::get<I>(t)));
        ss << ")";
        return ss.str();
    }
    template<class ...Args>
    static String show(const std::tuple<Args...>& t)
    {
        return show(t, std::make_index_sequence<sizeof...(Args)>());
    }
}
#endif
