#ifndef TUPLE_CPP
#define TUPLE_CPP
#include <tuple>
#include "nat.hpp"
#include "optional.hpp"
#include "type_checks.h"
#include <utility>

// Helper metafunc for getting fst
template <class... Args, std::size_t... Is>
constexpr auto _fst_helper(std::tuple<Args...> tp, std::index_sequence<Is...>)
{
    return std::tuple{std::get<Is>(tp)...};
}

namespace tuple {
    template<class ...Args>
    using Tuple = std::tuple<Args...>;
   
    template<class ...Args>
    inline static const std::tuple<Args...> mkTuple(Args... a) {
        return std::make_tuple(a...);
    }
 
    // Constructive match, l is considered immutable and will be copied safely
    template<typename Func, class ...Args, typename Ret = std::invoke_result_t<Func, Args...>>
    constexpr Ret match(Tuple<Args...> t, Func f) {
        static_assert(CallableWith<Func, Args...>, "Tuple match closure not callable with element types");
        return std::apply(f, t);
    }
    
    // fst (1,2) = 1%nat
    // fst (1,2,3) = (1,2)%(nat * nat)
    template <class... Args>
    constexpr auto fst(std::tuple<Args...> tp)
    {
        if constexpr (sizeof...(Args) == 2)
            return std::get<0>(tp);
        else
            return _fst_helper(tp, std::make_index_sequence<sizeof...(Args) - 1>{});
    }

    template<class ...Args>
    inline static decltype(auto) snd(Tuple<Args...> t) {
        return std::get<sizeof...(Args) - 1>(t);
    }
}
#endif
