#ifndef TUPLE_CPP
#define TUPLE_CPP
#include <tuple>
#include "nat.hpp"
#include "optional.hpp"
#include "type_checks.h"


namespace tuple {
    template<class ...Args>
    using Tuple = std::tuple<Args...>;
   
    template<class ...Args>
	inline static const std::tuple<Args...> mkTuple(Args... a) {
		return std::make_tuple(a...);
	}
 
    // Constructive match, l is considered immutable and will be copied safely
    template<typename Func, class ...Args, typename Ret = std::invoke_result_t<Func, Args...>>
    inline static const Ret match(Tuple<Args...> t, Func f) {
        static_assert(CallableWith<Func, Args...>, "Tuple match closure not callable with element types");
		return std::apply(f, t);
    }

    // TODO: Fst is more involved, since it returns multiple types depending on size:
    // fst (1,2) = 1%nat
    // fst (1,2,3) = (1,2)%(nat * nat)
    template<class ...Args>
    inline static decltype(auto) fst(Tuple<Args...> t);
 
    template<class ...Args>
    inline static decltype(auto) snd(Tuple<Args...> t) {
		return std::get<sizeof...(Args) - 1>(t);
	}
}
#endif
