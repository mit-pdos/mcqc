#ifndef TUPLE_H
#define TUPLE_H
#include <tuple>
#include <utility>
#include "nat.hpp"
#include "optional.hpp"
#include "type_checks.h"

// Private, helper metafunc for getting fst using index_sequence
template <class... Args, std::size_t... Is>
constexpr auto _fst_helper(std::tuple<Args...> tp, std::index_sequence<Is...>)
{
    return std::tuple{std::get<Is>(tp)...};
}

namespace tuple {
    template<class ...Args>
    using Tuple = std::tuple<Args...>;

    // Tuple constructor
    template<class ...Args>
    inline static const std::tuple<Args...> mkTuple(Args... a) {
        return std::make_tuple(a...);
    }

    // constructive match
    template<typename Func, class ...Args, typename Ret = std::invoke_result_t<Func, Args...>,
             typename = std::enable_if_t<CallableWith<Func, Args...> && "Argument not callable with tuple types">>
    constexpr Ret match(Tuple<Args...>&& tp, Func f) {
        static_assert(CallableWith<Func, Args...>, "Tuple match closure not callable with element types");
        return std::apply(f, tp);
    }
    template<typename Func, class ...Args, typename Ret = std::invoke_result_t<Func, Args...>>
    constexpr Ret match(Tuple<Args...>& tp, Func f) {
        return match(std::forward<Tuple<Args...>>(tp), f);
    }

    // First of tuple
    // fst (1,2) = 1%nat
    // fst (1,2,3) = (1,2)%(nat * nat)
    template <class... Args>
    constexpr auto fst(Tuple<Args...>&& tp) {
        if constexpr (sizeof...(Args) == 1)
			throw "Attempting to call fst on a unary tuple, undefined behavior";
        else if constexpr (sizeof...(Args) == 2)
            return std::get<0>(std::forward<Tuple<Args...>>(tp));
        else
            return _fst_helper(std::forward<Tuple<Args...>>(tp), std::make_index_sequence<sizeof...(Args) - 1>{});
    }
    template <class... Args>
    constexpr auto fst(Tuple<Args...>& tp) {
		return fst(std::forward<Tuple<Args...>>(tp));
    }

    // Second of tuple
	// snd (1,2,3) = 3%nat
    template<class ...Args>
    constexpr auto snd(Tuple<Args...>&& tp) {
        return std::get<sizeof...(Args) - 1>(std::forward<Tuple<Args...>>(tp));
    }
    template<class ...Args>
    constexpr auto snd(Tuple<Args...>& tp) {
		return snd(std::forward<Tuple<Args...>>(tp));
    }
}
#endif
